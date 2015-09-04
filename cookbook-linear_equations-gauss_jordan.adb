-- Copyright 2015 Hadrien Grasland
--
-- This file is part of Numerical Cookbook.
--
-- Numerical Cookbook is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Numerical Cookbook is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Numerical Cookbook.  If not, see <http://www.gnu.org/licenses/>.


with Cookbook.Test;

package body Cookbook.Linear_Equations.Gauss_Jordan is

   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix; Right_Hand_Vectors : in out F_Containers.Matrix) is
      -- First, let's define some facilities for handling matrix rows and columns
      subtype Mat_Row is Index_Type range Matrix'First (1) .. Matrix'Last (1);
      subtype Mat_Col is Index_Type range Matrix'First (2) .. Matrix'Last (2);

      -- We'll also need some quick access to the matrix size, which is unique since the matrix is square
      Mat_Size : constant Size_Type := Matrix'Length (1); -- Named "n" in NR

      -- Ditto for the right-hand side vectors
      subtype RHS_Row is Index_Type range Right_Hand_Vectors'First (1) .. Right_Hand_Vectors'Last (1);
      subtype RHS_Col is Index_Type range Right_Hand_Vectors'First (2) .. Right_Hand_Vectors'Last (2);

      -- We'll often need to convert between matrix and right-hand side rows, and between rows and columns, so it's a good thing to automate it
      function Mat_Row_To_RHS_Row (Matrix_Row : Mat_Row) return RHS_Row is (Matrix_Row - Mat_Row'First + RHS_Row'First);
      function Mat_Row_To_Mat_Col (Matrix_Row : Mat_Row) return Mat_Col is (Matrix_Row - Mat_Row'First + Mat_Col'First);
      function Mat_Col_To_Mat_Row (Matrix_Col : Mat_Col) return Mat_Row is (Matrix_Col - Mat_Col'First + Mat_Row'First);
      function Mat_Col_To_RHS_Row (Matrix_Col : Mat_Col) return RHS_Row is (Matrix_Col - Mat_Col'First + RHS_Row'First);

      -- Finally, after row-wise pivoting, we may need to swap matrix columns, and this requires knowing the original pivot locations
      subtype Pivot_Index is Index_Type range Index_Type'First .. Index_Type'Pred (Index_Type'First + Mat_Size);
      Initial_Pivot_Rows : array (Pivot_Index) of Mat_Row; -- Named "indxr" in NR
      Initial_Pivot_Cols : array (Pivot_Index) of Mat_Col; -- Named "indxc" in NR
   begin
      -- Perform pivoting and matrix reduction Mat_Size times, deriving the inverse matrix in scrambled column order
      declare
         -- We'll be looking for a pivot, which is defined as a row and a column
         Pivot_Row : Mat_Row; -- Named "irow" in NR
         Pivot_Col : Mat_Col; -- Named "icol" in NR

         -- When we have found the pivot for a certain row/column, we take note of it to avoid looking at this (modified) part of the matrix again
         Pivot_Found : array (Mat_Col) of Boolean := (others => False); -- Named "ipiv" in NR
      begin
         for Current_Pivot in Pivot_Index loop
            -- Find a new pivot element, using the heuristic that the largest element is probably the best
            declare
               Pivot_Abs : Float_Type := 0.0; -- Named "big" in NR
            begin
               -- Parse the matrix for a pivot, avoiding the part that we already processed
               for Row in Mat_Row loop
                  if not Pivot_Found (Mat_Row_To_Mat_Col (Row)) then
                     for Col in Mat_Col loop
                        if not Pivot_Found (Col) then
                           declare
                              Value_Abs : constant Float_Type := abs Matrix (Row, Col);
                           begin
                              if Value_Abs >= Pivot_Abs then
                                 Pivot_Abs := Value_Abs;
                                 Pivot_Row := Row;
                                 Pivot_Col := Col;
                              end if;
                           end;
                        end if;
                     end loop;
                  end if;
               end loop;
               Pivot_Found (Pivot_Col) := True;

               -- If the pivot that we've found is zero, it means that the matrix is singular
               if Pivot_Abs = 0.0 then
                  raise Singular_Matrix;
               end if;
            end;

            -- At this point, (Pivot_Row, Pivot_Col) designates our current pivot element. We now put it on the diagonal, swapping rows if needed.
            -- We also memorize in which column and row each pivot was originally located, to swap the columns of the inverse matrix later.
            Initial_Pivot_Rows (Current_Pivot) := Pivot_Row;
            Initial_Pivot_Cols (Current_Pivot) := Pivot_Col;
            if Pivot_Row /= Pivot_Col then
               Swap_Rows (Matrix, Pivot_Row, Mat_Col_To_Mat_Row (Pivot_Col));
               Swap_Rows (Right_Hand_Vectors, Mat_Row_To_RHS_Row (Pivot_Row), Mat_Col_To_RHS_Row (Pivot_Col));
            end if;
            Pivot_Row := Mat_Col_To_Mat_Row (Pivot_Col);

            -- Divide the pivot row by the pivot element, and replace that element by its inverse
            declare
               Pivot_Inv : constant Float_Type := 1.0 / Matrix (Pivot_Row, Pivot_Col); -- Named "pivinv" in NR
            begin
               Matrix (Pivot_Row, Pivot_Col) := 1.0;
               for Col in Mat_Col loop
                  Matrix (Pivot_Row, Col) := Matrix (Pivot_Row, Col) * Pivot_Inv;
               end loop;
               for Col in RHS_Col loop
                  Right_Hand_Vectors (Mat_Row_To_RHS_Row (Pivot_Row), Col) := Right_Hand_Vectors (Mat_Row_To_RHS_Row (Pivot_Row), Col) * Pivot_Inv;
               end loop;
            end;

            -- Reduce the rows, except for the pivot one obviously
            for Row in Mat_Row loop
               if Row /= Pivot_Row then
                  declare
                     Tmp : constant Float_Type := Matrix (Row, Pivot_Col); -- Named "dum" in NR
                  begin
                     Matrix (Row, Pivot_Col) := 0.0;
                     for Col in Mat_Col loop
                        Matrix (Row, Col) := Matrix (Row, Col) * (1.0 - Tmp);
                     end loop;
                     for Col in RHS_Col loop
                        Right_Hand_Vectors (Mat_Row_To_RHS_Row (Row), Col) := Right_Hand_Vectors (Mat_Row_To_RHS_Row (Row), Col) * (1.0 - Tmp);
                     end loop;
                  end;
               end if;
            end loop;
         end loop;
      end;

      -- At this stage, the solution vectors are correct, but the inverse matrix may have suffered column swaps and must be unscrambled
      for Current_Pivot in reverse Pivot_Index loop
         if Initial_Pivot_Rows (Current_Pivot) /= Mat_Col_To_Mat_Row (Initial_Pivot_Cols (Current_Pivot)) then
            Swap_Cols (Matrix, Mat_Row_To_Mat_Col (Initial_Pivot_Rows (Current_Pivot)), Initial_Pivot_Cols (Current_Pivot));
         end if;
      end loop;
   end Gauss_Jordan_Elimination;


   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix) is
      No_Right_Hand_Side : F_Containers.Matrix (Matrix'First (1) .. Matrix'Last (1), Index_Type'Last .. Index_Type'Pred (Index_Type'Last));
   begin
      Gauss_Jordan_Elimination (Matrix, No_Right_Hand_Side);
   end Gauss_Jordan_Elimination;


   package Test_Runner is new Cookbook.Test;


   procedure Test_Child is
      use Test_Runner;

      procedure Test_Gauss_Jordan is
      begin
         -- Try inverting a 1x1 singular matrix, expect an exception
         declare
            Singular : F_Containers.Matrix := 0.0 * F_Containers.Identity_Matrix (1);
         begin
            Gauss_Jordan_Elimination (Singular);
            Test_Element_Property (False, "should throw an exception when encountering a singular matrix");
         exception
            when Singular_Matrix => Test_Element_Property (True, "should throw an exception upon encountering a singular matrix");
         end;

         -- Try inverting a basic 1x1 matrix, with and without a right-hand side
         declare
            Init_1x1 : constant F_Containers.Matrix (7 .. 7, 9 .. 9) := 0.1 * F_Containers.Identity_Matrix (1);
            Mat_1x1 : F_Containers.Matrix := Init_1x1;
            RHS_1x2 : F_Containers.Matrix (3 .. 3, 5 .. 6) := (3 => (5.0, 10.0));
         begin
            Gauss_Jordan_Elimination (Mat_1x1, RHS_1x2);
            Test_Element_Property (Mat_1x1 = (7 => (9 => 10.0)), "should work with a 1x1 matrix");
            Test_Element_Property (RHS_1x2 = (3 => (50.0, 100.0)), "should work with a 1x2 right-hand side");
            Gauss_Jordan_Elimination (Mat_1x1);
            Test_Element_Property (Mat_1x1 = Init_1x1, "should work when a matrix is inverted twice");
         end;

         -- Try it with a diagonal 2x2 matrix
         declare
            Mat_2x2 : F_Containers.Matrix (11 .. 12, 15 .. 16) := ((0.5, 0.0), (0.0, 4.0));
            RHS_2x2 : F_Containers.Matrix (42 .. 43, 65 .. 66) := ((5.0, 0.0), (0.0, 10.0));
         begin
            Gauss_Jordan_Elimination (Mat_2x2, RHS_2x2);
            Test_Element_Property (Mat_2x2 = ((2.0, 0.0), (0.0, 0.25)), "should work with a diagonal 2x2 matrix");
            Test_Element_Property (RHS_2x2 = ((10.0, 0.0), (0.0, 2.5)), "should work with a 2x2 right-hand side");
         end;

         -- Try it with a non-diagonal matrix
         declare
            Mat_2x2 : F_Containers.Matrix (50 .. 51, 3 .. 4) := ((0.0, 4.0), (0.5, 0.0));
            RHS_2x2 : F_Containers.Matrix (42 .. 43, 65 .. 66) := ((5.0, 0.0), (0.0, 10.0));
         begin
            Gauss_Jordan_Elimination (Mat_2x2, RHS_2x2);
            Test_Element_Property (Mat_2x2 = ((0.0, 2.0), (0.25, 0.0)), "should work with a non-diagonal 2x2 matrix");
            Test_Element_Property (RHS_2x2 = ((0.0, 20.0), (1.25, 0.0)), "should work with a 2x2 right-hand side");
         end;
      end Test_Gauss_Jordan;

      procedure Test_Linear_Equations_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Gauss_Jordan_Elimination"), Test_Gauss_Jordan'Access);
      end Test_Linear_Equations_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations.Gauss_Jordan"), Test_Linear_Equations_Package'Access);
   end Test_Child;

begin

   -- Automatically test the package when it is included
   Test_Runner.Elaboration_Self_Test (Test_Child'Access);

end Cookbook.Linear_Equations.Gauss_Jordan;
