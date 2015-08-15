with Cookbook.Test;

package body Cookbook.Linear_Equations.LU_Decomp is

   function Is_LU_Decomposition_Of (LU : LU_Decomposition; Original_Matrix : F_Containers.Matrix) return Boolean is
     (LU.First_Row = Original_Matrix'First (1) and then LU.Last_Row = Original_Matrix'Last (1) and then
      LU.First_Col = Original_Matrix'First (2) and then LU.Last_Col = Original_Matrix'Last (2) and then
      Unscramble (LU, Lower (LU) * Upper (LU)) = Original_Matrix);


   function Crout_LU_Decomposition (Matrix : F_Containers.Matrix) return LU_Decomposition is
   begin
      -- We begin by declaring the LU object, including the matrix copy, and will never work on the original
      -- matrix directly afterwards. This eases changing its definition later (e.g. using different row/column indexing).
      return LU : LU_Decomposition := (First_Row => Matrix'First (1),
                                       Last_Row => Matrix'Last (1),
                                       First_Col => Matrix'First (2),
                                       Last_Col => Matrix'Last (2),
                                       Data => Matrix,
                                       Determinant_Multiplier => 1.0,
                                       Initial_Row_Positions => <>) do
         declare
            -- As for Gauss-Jordan elimination, we'll need to iterate over matrix rows and columns, so let's prepare for it
            subtype LU_Row is Index_Type range LU.First_Row .. LU.Last_Row;
            subtype LU_Col is Index_Type range LU.First_Col .. LU.Last_Col;
            function Col_To_Row (Col : LU_Col) return LU_Row is (Col - LU_Col'First + LU_Row'First);

            -- We'll also need to memorize the scaling of matrix rows (magnitude of largest element)
            Rows_Scaling : array (LU_Row) of Float_Type;
         begin
            -- First, determine the implicit scaling of each matrix row, as defined by its largest matrix element
            for Row in LU_Row loop
               declare
                  Current_Row_Magnitude : Float_Type := 0.0;
               begin
                  -- Look for the largest element in each row
                  for Col in LU_Col loop
                     declare
                        Item_Magnitude : constant Float_Type := abs LU.Data (Row, Col);
                     begin
                        if Item_Magnitude > Current_Row_Magnitude then
                           Current_Row_Magnitude := Item_Magnitude;
                        end if;
                     end;
                  end loop;

                  -- If all elements in the row are zero, the matrix is singular and it's pointless to continue.
                  -- Otherwise, save the scaling of that row and move on.
                  if Current_Row_Magnitude = 0.0 then
                     raise Singular_Matrix;
                  else
                     Rows_Scaling (Row) := 1.0 / Current_Row_Magnitude;
                  end if;
               end;
            end loop;

            -- Column by column, and operating simultaneously on identically numbered rows, reduce the matrix copy to its LU decomposition.
            for Pivot_Col in LU_Col loop
               declare
                  Pivot_Row : LU_Row;
               begin
                  -- Look for the next pivot in this column, taking scaling into account
                  declare
                     Scaled_Pivot_Abs : Float_Type := 0.0;
                  begin
                     for Row in Col_To_Row (Pivot_Col) .. LU.Last_Row loop
                        declare
                           Scaled_Item_Abs : constant Float_Type := Rows_Scaling (Row) * abs LU.Data (Row, Pivot_Col);
                        begin
                           if Scaled_Item_Abs > Scaled_Pivot_Abs then
                              Scaled_Pivot_Abs := Scaled_Item_Abs;
                              Pivot_Row := Row;
                           end if;
                        end;
                     end loop;

                     -- If we can't find anything better than zero, it means that the matrix is singular.
                     -- Unlike NR, I choose to consistently raise an exception when singularities are encountered.
                     if Scaled_Pivot_Abs = 0.0 then
                        raise Singular_Matrix;
                     end if;
                  end;

                  -- If the pivot is not on the diagonal, put it there by swapping rows.
                  -- Properly account for the fact that this also changes determinant sign and row scaling.
                  LU.Initial_Row_Positions (Col_To_Row (Pivot_Col)) := Pivot_Row;
                  if Pivot_Row /= Col_To_Row (Pivot_Col) then
                     Swap_Rows (LU.Data, Pivot_Row, Col_To_Row (Pivot_Col));
                     LU.Determinant_Multiplier := -LU.Determinant_Multiplier;
                     F_Utility.Swap (Rows_Scaling (Pivot_Row), Rows_Scaling (Col_To_Row (Pivot_Col)));
                     Pivot_Row := Col_To_Row (Pivot_Col);
                  end if;

                  -- Divide the remainder of the pivot's column by the pivot element and reduce the remaining submatrix
                  declare
                     Pivot_Element : constant Float_Type := LU.Data (Pivot_Row, Pivot_Col);
                  begin
                     for Row in Pivot_Row + Index_Type'(1) .. LU.Last_Row loop
                        LU.Data (Row, Pivot_Col) := LU.Data (Row, Pivot_Col) / Pivot_Element;
                        declare
                           Pivot_Col_Value : constant Float_Type := LU.Data (Row, Pivot_Col);
                        begin
                           for Col in Pivot_Col + Index_Type'(1) .. LU.Last_Col loop
                              LU.Data (Row, Col) := LU.Data (Row, Col) - Pivot_Col_Value * LU.Data (Pivot_Row, Col);
                           end loop;
                        end;
                     end loop;
                  end;
               end;
            end loop;
         end;
      end return;
   end Crout_LU_Decomposition;


   function Is_Lower_Triangular (Matrix : F_Containers.Matrix) return Boolean is
     (for all Row in Matrix'Range (1) =>
          (for all Col in Matrix'Range (2) =>
             (if Size_Type'(Row - Matrix'First (1)) < Size_Type'(Col - Matrix'First (2)) then Matrix (Row, Col) = 0.0)));


   function Is_Upper_Triangular (Matrix : F_Containers.Matrix) return Boolean is
     (for all Row in Matrix'Range (1) =>
          (for all Col in Matrix'Range (2) =>
             (if Size_Type'(Row - Matrix'First (1)) > Size_Type'(Col - Matrix'First (2)) then Matrix (Row, Col) = 0.0)));


   function Lower (LU : LU_Decomposition) return F_Containers.Matrix is
   begin
      return Result : F_Containers.Matrix (LU.First_Row .. LU.Last_Row, LU.First_Col .. LU.Last_Col) do
         for Row in Result'Range (1) loop
            for Col in Result'Range (2) loop
               declare
                  Row_Offset : constant Size_Type := Row - Result'First (1);
                  Col_Offset : constant Size_Type := Col - Result'First (2);
               begin
                  if Col_Offset < Row_Offset then
                     Result (Row, Col) := LU.Data (Row, Col);
                  elsif Col_Offset = Row_Offset then
                     Result (Row, Col) := 1.0;
                  else
                     Result (Row, Col) := 0.0;
                  end if;
               end;
            end loop;
         end loop;
      end return;
   end Lower;


   function Upper (LU : LU_Decomposition) return F_Containers.Matrix is
   begin
      return Result : F_Containers.Matrix (LU.First_Row .. LU.Last_Row, LU.First_Col .. LU.Last_Col) do
         for Row in Result'Range (1) loop
            for Col in Result'Range (2) loop
               declare
                  Row_Offset : constant Size_Type := Row - Result'First (1);
                  Col_Offset : constant Size_Type := Col - Result'First (2);
               begin
                  if Col_Offset >= Row_Offset then
                     Result (Row, Col) := LU.Data (Row, Col);
                  else
                     Result (Row, Col) := 0.0;
                  end if;
               end;
            end loop;
         end loop;
      end return;
   end Upper;


   function Unscramble (LU_Permutation : LU_Decomposition; Scrambled_Matrix : F_Containers.Matrix) return F_Containers.Matrix is
   begin
      return Result : F_Containers.Matrix := Scrambled_Matrix do
         for Row in reverse Result'Range (1) loop
            if Row /= LU_Permutation.Initial_Row_Positions (Row) then
               Swap_Rows (Result, Row, LU_Permutation.Initial_Row_Positions (Row));
            end if;
         end loop;
      end return;
   end Unscramble;


   procedure Test is
      package Test_Runner is new Cookbook.Test;
      use Test_Runner;

      procedure Test_Crout_LU is
      begin
         -- Try inverting a 1x1 singular matrix, expect an exception
         declare
            Singular : constant F_Containers.Matrix := 0.0 * F_Containers.Identity_Matrix (1);
         begin
            declare
               LU : constant LU_Decomposition := Crout_LU_Decomposition (Singular);
            begin
               Test_Element_Property (False, "should throw an exception when encountering a singular matrix");
            end;
         exception
            when Singular_Matrix => Test_Element_Property (True, "should throw an exception upon encountering a singular matrix");
         end;

         -- Try decomposing a basic 1x1 matrix
         declare
            Mat_1x1 : constant F_Containers.Matrix (3 .. 3, 5 .. 5) := 0.1 * F_Containers.Identity_Matrix (1);
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_1x1);
         begin
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 0.1, "should work with a 1x1 matrix");
         end;

         -- Try it with a diagonal 2x2 matrix
         declare
            Mat_2x2 : F_Containers.Matrix (24 .. 25, 15 .. 16) := 0.5 * F_Containers.Identity_Matrix (2);
         begin
            Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'Last (2)) := 4.0;
            declare
               LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
            begin
               Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 0.5, "should work with a diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'Last (2)) = 0.0, "should work with a diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'First (2)) = 0.0, "should work with a diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'Last (2)) = 4.0, "should work with a diagonal 2x2 matrix");
            end;
         end;

         -- Try it with an inversed-diagonal 2x2 matrix
         declare
            Mat_2x2 : F_Containers.Matrix (55 .. 56, 44 .. 45) := 0.0 * F_Containers.Identity_Matrix (2);
         begin
            Mat_2x2 (Mat_2x2'First (1), Mat_2x2'Last (2)) := 4.0;
            Mat_2x2 (Mat_2x2'Last (1), Mat_2x2'First (2)) := 0.5;
            declare
               LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
            begin
               Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 0.5, "should work with a non-diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'Last (2)) = 0.0, "should work with a non-diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'First (2)) = 0.0, "should work with a non-diagonal 2x2 matrix");
               Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'Last (2)) = 4.0, "should work with a non-diagonal 2x2 matrix");
            end;
         end;
      end Test_Crout_LU;

      procedure Test_Linear_Equations_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Crout_LU_Decomposition"), Test_Crout_LU'Access);
         -- TODO : Test other LU-related methods
      end Test_Linear_Equations_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations.LU_Decomp"), Test_Linear_Equations_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Linear_Equations.LU_Decomp;
