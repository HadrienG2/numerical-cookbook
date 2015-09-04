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

package body Cookbook.Linear_Equations.Tridiagonal is

   function "*" (Left : Tridiagonal_Matrix; Right : F_Containers.Vector) return F_Containers.Vector is
      -- Quick access to the indices of the left and right components
      subtype Left_Index is Index_Type range Left.First_Index .. Left.Last_Index;
      subtype Right_Row is Index_Type range Right'First .. Right'Last;
      function Left_Col_To_Right_Row (Col : Left_Index) return Right_Row is (Col - Left_Index'First + Right_Row'First);
   begin
      return Result : F_Containers.Vector (Left_Index) do
         if Matrix_Size (Left) >= 2 then
            declare
               -- Quicker access to a couple of special component indices
               Second_Left_Index : constant Left_Index := Left_Index'Succ (Left_Index'First);
               Left_Index_Before_Last : constant Left_Index := Left_Index'Pred (Left_Index'Last);
               Second_Right_Row : constant Right_Row := Right_Row'Succ (Right_Row'First);
               Right_Row_Before_Last : constant Right_Row := Right_Row'Pred (Right_Row'Last);
            begin
               -- The first result element is special because there is no lower diagonal element
               Result (Left_Index'First) := Left.Diagonal (Left_Index'First) * Right (Right_Row'First) +
                 Left.Upper_Diagonal (Left_Index'First) * Right (Second_Right_Row);

               -- General computation for result elements associated to the middle of the matrix
               for Row in Second_Left_Index .. Left_Index_Before_Last loop
                  declare
                     Current_Right_Row : constant Right_Row := Left_Col_To_Right_Row (Row);
                     Previous_Right_Row : constant Right_Row := Right_Row'Pred (Current_Right_Row);
                     Next_Right_Row : constant Right_Row := Right_Row'Succ (Current_Right_Row);
                  begin
                     Result (Row) := Left.Lower_Diagonal (Row) * Right (Previous_Right_Row) +
                       Left.Diagonal (Row) * Right (Current_Right_Row) +
                       Left.Upper_Diagonal (Row) * Right (Next_Right_Row);
                  end;
               end loop;

               -- The last result element is special because there is no upper diagonal element
               Result (Left_Index'Last) := Left.Lower_Diagonal (Left_Index'Last) * Right (Right_Row_Before_Last) +
                 Left.Diagonal (Left_Index'Last) * Right (Right_Row'Last);
            end;
         elsif Matrix_Size (Left) = 1 then
            -- The one-element matrix case is also special because there are no diagonals to speak of
            Result (Left_Index'First) := Left.Diagonal (Left_Index'First) * Right (Right_Row'First);
         end if;
      end return;
   end "*";


   function Solve (Matrix : Tridiagonal_Matrix; Right_Hand_Vector : F_Containers.Vector) return F_Containers.Vector is
   begin
      -- TODO : Reuse and rewrite NR's code
   end;


   package Test_Runner is new Cookbook.Test;


   procedure Test_Child is
      use Test_Runner;

      procedure Test_MatVecMul is
      begin
         -- Test that null matrices (special case !) work
         declare
            Mat_0x0 : constant Tridiagonal_Matrix := (First_Index => Index_Type'Last,
                                                      Last_Index => Index_Type'Pred (Index_Type'Last),
                                                      Lower_Diagonal => (others => <>),
                                                      Diagonal => (others => <>),
                                                      Upper_Diagonal => (others => <>));
            Vec_0 : constant F_Containers.Vector (69 .. 68) := (others => <>);
            Result : constant F_Containers.Vector := Mat_0x0 * Vec_0 with Unreferenced;
         begin
            Test_Element_Property (True, "should work with 0x0 matrices");
         end;

         -- Test that 1x1 matrices (special case !) work
         declare
            Mat_1x1 : constant Tridiagonal_Matrix := (First_Index => Index_Type'First,
                                                      Last_Index => Index_Type'First,
                                                      Lower_Diagonal => (others => 42.0),
                                                      Diagonal => (others => 1.5),
                                                      Upper_Diagonal => (others => 42.0));
            Vec_1 : constant F_Containers.Vector (42 .. 42) := (others => 2.0);
            Result : constant F_Containers.Vector := Mat_1x1 * Vec_1;
         begin
            Test_Element_Property (Result (Result'First) = 3.0, "should work with 1x1 matrices");
         end;

         -- Test that 2x2 matrices (special case !) work
         declare
            Mat_2x2 : constant Tridiagonal_Matrix := (First_Index => 55,
                                                      Last_Index => 56,
                                                      Lower_Diagonal => (42.0, 33.0),  -- aka ((44.0, 1.25),
                                                      Diagonal => (44.0, 0.5),         --      (33.0, 0.5))
                                                      Upper_Diagonal => (1.25, 42.0));
            Vec_2 : constant F_Containers.Vector (22 .. 23) := (12.0, 15.0);
            Result : constant F_Containers.Vector := Mat_2x2 * Vec_2;
         begin
            Test_Element_Property (Result = (546.75, 403.5), "should work with 2x2 matrices");
         end;

         -- Test that 3x3 matrices (single loop execution) work
         declare
            Mat_3x3 : constant Tridiagonal_Matrix := (First_Index => 32,
                                                      Last_Index => 34,
                                                      Lower_Diagonal => (42.0, 1.0, 2.0),   -- aka ((3.0, 6.0, 0.0),
                                                      Diagonal => (3.0, 4.0, 5.0),          --      (1.0, 4.0, 7.0),
                                                      Upper_Diagonal => (6.0, 7.0, 42.0));  --      (0.0, 2.0, 5.0))
            Vec_3 : constant F_Containers.Vector (89 .. 91) := (8.0, 9.0, 10.0);
            Result : constant F_Containers.Vector := Mat_3x3 * Vec_3;
         begin
            Test_Element_Property (Result = (78.0, 114.0, 68.0), "should work with 3x3 matrices");
         end;

         -- Test that 4x4 matrices (multiple loop executions) work
         declare
            Mat_4x4 : constant Tridiagonal_Matrix := (First_Index => 250,
                                                      Last_Index => 253,                          -- aka ((11.0,  7.0,  0.0,  0.0),
                                                      Lower_Diagonal => (42.0, 14.0, 13.0, 12.0), --      (14.0, 10.0,  6.0,  0.0),
                                                      Diagonal => (11.0, 10.0, 9.0, 8.0),         --      ( 0.0, 13.0,  9.0,  5.0))
                                                      Upper_Diagonal => (7.0, 6.0, 5.0, 42.0));   --      ( 0.0,  0.0, 12.0,  8.0))
            Vec_4 : constant F_Containers.Vector (1 .. 4) := (4.0, 3.0, 2.0, 1.0);
            Result : constant F_Containers.Vector := Mat_4x4 * Vec_4;
         begin
            Test_Element_Property (Result = (65.0, 98.0, 62.0, 32.0), "should work with 4x4 matrices");
         end;
      end Test_MatVecMul;

      procedure Test_Tridiagonal_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Matrix_Vector_Multiplication"), Test_MatVecMul'Access);
         -- TODO : Intégrer des tests pour les autres fonctionnalités impliquant des matrices tridiagonales.
      end Test_Tridiagonal_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations.Tridiagonal"), Test_Tridiagonal_Package'Access);
   end Test_Child;

begin

   -- Automatically test the package when it is included
   Test_Runner.Elaboration_Self_Test (Test_Child'Access);

end Cookbook.Linear_Equations.Tridiagonal;
