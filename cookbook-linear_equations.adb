with Cookbook.Test;

package body Cookbook.Linear_Equations is

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
      subtype Pivot_Index is Index_Type range Index_Type'First .. Index_Type'First + Mat_Size - 1;
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
               for Col in Mat_Col loop
                  F_Utility.Swap (Matrix (Pivot_Row, Col),
                                  Matrix (Mat_Col_To_Mat_Row (Pivot_Col), Col));
               end loop;
               for Col in RHS_Col loop
                  F_Utility.Swap (Right_Hand_Vectors (Mat_Row_To_RHS_Row (Pivot_Row), Col),
                                  Right_Hand_Vectors (Mat_Col_To_RHS_Row (Pivot_Col), Col));
               end loop;
            end if;
            Pivot_Row := Mat_Col_To_Mat_Row (Pivot_Col);

            -- Divide the pivot row by the pivot element, and replace that element by its inverse (?)
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
            for Row in Mat_Row loop
               F_Utility.Swap (Matrix (Row, Mat_Row_To_Mat_Col (Initial_Pivot_Rows (Current_Pivot))),
                               Matrix (Row, Initial_Pivot_Cols (Current_Pivot)));
            end loop;
         end if;
      end loop;
   end Gauss_Jordan_Elimination;


   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix) is
      No_Right_Hand_Side : F_Containers.Matrix (Matrix'First (1) .. Matrix'Last (1), Index_Type'Last .. Index_Type'Last - 1);
   begin
      Gauss_Jordan_Elimination (Matrix, No_Right_Hand_Side);
   end Gauss_Jordan_Elimination;


   function Crout_LU_Decomposition (Matrix : F_Containers.Matrix) return LU_Decomposition is
   begin
      -- We begin by declaring the LU object, including the matrix copy, and will never work on the original
      -- matrix directly afterwards. This eases changing its definition later (e.g. using different row/column indexing).
      return LU : LU_Decomposition (First_Row => Matrix'First (1),
                                    Last_Row => Matrix'Last (1),
                                    First_Col => Matrix'First (2),
                                    Last_Col => Matrix'Last (2)) := (Decomposition => Matrix,
                                                                     Determinant_Multiplier => 1.0) do
         declare
            -- As for Gauss-Jordan elimination, we'll need to iterate over matrix rows and columns, so let's prepare for it
            subtype LU_Row is Index_Type range LU.First_Row .. LU.Last_Row;
            subtype LU_Col is Index_Type range LU.First_Col .. LU.Last_Col;

            -- We'll also need to memorize the scaling of matrix rows (magnitude of largest element)
            Rows_Scaling : array (LU_Row) of Float_Type;
         begin
            -- First, determine the implicit scaling of each matrix row, as defined by its largest matrix element
            for Row in LU_Row loop
               declare
                  Current_Row_Scaling : Float_Type := 0.0;
               begin
                  -- Look for the largest element in each row
                  for Col in LU_Col loop
                     declare
                        Item_Scaling : Float_Type := abs LU.Decomposition (Row, Col);
                     begin
                        if Item_Scaling > Current_Row_Scaling then
                           Current_Row_Scaling := Item_Scaling;
                        end if;
                     end;
                  end loop;

                  -- If all elements in the row are zero, the matrix is singular and it's pointless to continue.
                  -- Otherwise, save the scaling of that row and move on.
                  if Current_Row_Scaling = 0.0 then
                     raise Singular_Matrix;
                  else
                     Rows_Scaling (Row) := Current_Row_Scaling;
                  end if;
               end;
            end loop;

            -- TODO : "Outermost kij loop"
            -- TODO :    - Search largest pivot element (with scaling taken into account), memorize its original position
            -- TODO :    - Swap rows to put it on the matrix diagonal, acknowledging the effect on scaling (value swap) and determinant (sign change)
            -- TODO :    - Unlike in NR, raise exception if matrix proves singular here too
            -- TODO :    - Divide remaining rows by pivot element and reduce remaining submatrix
      end return;
   end Crout_LU_Decomposition;


   procedure Test is
      package Test_Runner is new Cookbook.Test;
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
            Init_1x1 : constant F_Containers.Matrix := 0.1 * F_Containers.Identity_Matrix (1);
            Mat_1x1 : F_Containers.Matrix := Init_1x1;
            RHS_1x2 : F_Containers.Matrix (3 .. 3, 5 .. 6) := (3 => (5 => 5.0, 6 => 10.0));
         begin
            Gauss_Jordan_Elimination (Mat_1x1, RHS_1x2);
            Test_Element_Property (Mat_1x1 (Mat_1x1'First (1), Mat_1x1'First (2)) = 10.0, "should work with a 1x1 matrix");
            Test_Element_Property (RHS_1x2 (RHS_1x2'First (1), RHS_1x2'First (2)) = 50.0, "should work with a 1x2 right-hand side");
            Test_Element_Property (RHS_1x2 (RHS_1x2'First (1), RHS_1x2'Last (2)) = 100.0, "should work with a 1x2 right-hand side");
            Gauss_Jordan_Elimination (Mat_1x1);
            Test_Element_Property (Mat_1x1 = Init_1x1, "should work when a matrix is inverted twice");
         end;

         -- Try it with a diagonal 2x2 matrix
         declare
            Mat_2x2 : F_Containers.Matrix := 0.5 * F_Containers.Identity_Matrix (2);
            RHS_2x2 : F_Containers.Matrix (42 .. 43, 65 .. 66) := (42 => (65 => 5.0, 66 => 0.0), 43 => (65 => 0.0, 66 => 10.0));
         begin
            Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'Last (2)) := 4.0;
            Gauss_Jordan_Elimination (Mat_2x2, RHS_2x2);
            Test_Element_Property (Mat_2x2 (Mat_2x2'First (1), Mat_2x2'First (2)) = 2.0, "should work with a diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'First (1), Mat_2x2'Last (2)) = 0.0, "should work with a diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'First (2)) = 0.0, "should work with a diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'Last (2)) = 0.25, "should work with a diagonal 2x2 matrix");
            Test_Element_Property (RHS_2x2 (RHS_2x2'First (1), RHS_2x2'First (2)) = 10.0, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'Last (1), RHS_2x2'First (2)) = 0.0, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'First (1), RHS_2x2'Last (2)) = 0.0, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'Last (1), RHS_2x2'Last (2)) = 2.5, "should work with a 2x2 right-hand side");
         end;

         -- Try it with a non-diagonal matrix
         declare
            Mat_2x2 : F_Containers.Matrix := 0.0 * F_Containers.Identity_Matrix (2);
            RHS_2x2 : F_Containers.Matrix (42 .. 43, 65 .. 66) := (42 => (65 => 5.0, 66 => 0.0), 43 => (65 => 0.0, 66 => 10.0));
         begin
            Mat_2x2 (Mat_2x2'First (1), Mat_2x2'Last (2)) := 4.0;
            Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'First (2)) := 0.5;
            Gauss_Jordan_Elimination (Mat_2x2, RHS_2x2);
            Test_Element_Property (Mat_2x2 (Mat_2x2'First (1), Mat_2x2'First (2)) = 0.0, "should work with a non-diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'First (1), Mat_2x2'Last (2)) = 2.0, "should work with a non-diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'First (2)) = 0.25, "should work with a non-diagonal 2x2 matrix");
            Test_Element_Property (Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'Last (2)) = 0.0, "should work with a non-diagonal 2x2 matrix");
            Test_Element_Property (RHS_2x2 (RHS_2x2'First (1), RHS_2x2'First (2)) = 0.0, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'Last (1), RHS_2x2'First (2)) = 1.25, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'First (1), RHS_2x2'Last (2)) = 20.0, "should work with a 2x2 right-hand side");
            Test_Element_Property (RHS_2x2 (RHS_2x2'Last (1), RHS_2x2'Last (2)) = 0.0, "should work with a 2x2 right-hand side");
         end;
      end Test_Gauss_Jordan;

      procedure Test_Linear_Equations_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Gauss_Jordan_Elimination"), Test_Gauss_Jordan'Access);
         -- TODO : Test other linear equation solvers as they are added
      end Test_Linear_Equations_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations"), Test_Linear_Equations_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Linear_Equations;
