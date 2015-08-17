with Cookbook.Test;

package body Cookbook.Linear_Equations.LU_Decomp is

   function Is_LU_Decomposition_Of (LU : LU_Decomposition; Original_Matrix : F_Containers.Matrix) return Boolean is
     (LU.First_Row = Original_Matrix'First (1) and then LU.Last_Row = Original_Matrix'Last (1) and then
      LU.First_Col = Original_Matrix'First (2) and then LU.Last_Col = Original_Matrix'Last (2) and then
      Compute_Original_Matrix (LU) = Original_Matrix);


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
                     for Row in Pivot_Row + Size_Type'(1) .. LU.Last_Row loop
                        LU.Data (Row, Pivot_Col) := LU.Data (Row, Pivot_Col) / Pivot_Element;
                        declare
                           Pivot_Col_Value : constant Float_Type := LU.Data (Row, Pivot_Col);
                        begin
                           for Col in Pivot_Col + Size_Type'(1) .. LU.Last_Col loop
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


   function Compute_Original_Matrix (LU : LU_Decomposition) return F_Containers.Matrix is
     (Unscramble (LU, Lower (LU) * Upper (LU)));


   function Solve (LU : LU_Decomposition; Right_Hand_Vector : F_Containers.Vector) return F_Containers.Vector is
      subtype LU_Row is Index_Type range LU.First_Row .. LU.Last_Row;
      subtype LU_Col is Index_Type range LU.First_Col .. LU.Last_Col;
      subtype RHS_Row is Index_Type range Right_Hand_Vector'First .. Right_Hand_Vector'Last;

      function LU_Row_To_LU_Col (Row : LU_Row) return LU_Col is (Row - LU_Row'First + LU_Col'First);
      function LU_Row_To_RHS (Row : LU_Row) return RHS_Row is (Row - LU_Row'First + RHS_Row'First);
      function LU_Col_To_RHS (Col : LU_Col) return RHS_Row is (Col - LU_Col'First + RHS_Row'First);
   begin
      return Result : F_Containers.Vector := Right_Hand_Vector do
         -- Do the forward substitution, using the lower-triangular part of LU. Do not forget to unscramble the solution in the way
         declare
            Nonzero_Element_Encountered : Boolean := False;
            First_Nonzero_Col : LU_Col := LU_Col'First; -- DEBUG : This assignment is unnecessary, but used to suppress an undue GNAT 2015 warning
         begin
            for Row in LU_Row loop
               declare
                  Initial_RHS_Row_Pos : constant RHS_Row := LU_Row_To_RHS (LU.Initial_Row_Positions (Row));
                  Sum : Float_Type := Result (Initial_RHS_Row_Pos);
               begin
                  Result (Initial_RHS_Row_Pos) := Result (LU_Row_To_RHS (Row));
                  if Nonzero_Element_Encountered then
                     for Col in First_Nonzero_Col .. LU_Row_To_LU_Col (Row) - Size_Type'(1) loop
                        Sum := Sum - LU.Data (Row, Col) * Result (LU_Col_To_RHS (Col));
                     end loop;
                  elsif Sum /= 0.0 then
                     Nonzero_Element_Encountered := True;
                     First_Nonzero_Col := LU_Row_To_LU_Col (Row);
                  end if;
                  Result (LU_Row_To_RHS (Row)) := Sum;
               end;
            end loop;
         end;

         -- Do the backsubstitution, using the upper-triangular part of LU
         for Row in reverse LU_Row loop
            declare
               Sum : Float_Type := Result (LU_Row_To_RHS (Row));
            begin
               for Col in LU_Row_To_LU_Col (Row) + Size_Type'(1) .. LU_Col'Last loop
                  Sum := Sum - LU.Data (Row, Col) * Result (LU_Col_To_RHS (Col));
               end loop;
               Result (LU_Row_To_RHS (Row)) := Sum / LU.Data (Row, LU_Row_To_LU_Col (Row));
            end;
         end loop;
      end return;
   end Solve;


   function Solve (LU : LU_Decomposition; Right_Hand_Vectors : F_Containers.Matrix) return F_Containers.Matrix is
      subtype RHS_Row is Index_Type range Right_Hand_Vectors'First (1) .. Right_Hand_Vectors'Last (1);
      subtype RHS_Col is Index_Type range Right_Hand_Vectors'First (2) .. Right_Hand_Vectors'Last (2);
   begin
      return Result : F_Containers.Matrix (RHS_Row'First .. RHS_Row'Last, RHS_Col'First .. RHS_Col'Last) do
         -- Solve for the right-hand side vectors one by one
         for Col in RHS_Col loop
            declare
               Current_Vector : F_Containers.Vector (RHS_Row);
            begin
               for Row in RHS_Row loop
                  Current_Vector (Row) := Right_Hand_Vectors (Row, Col);
               end loop;
               Current_Vector := Solve (LU, Current_Vector);
               for Row in RHS_Row loop
                 Result (Row, Col) := Current_Vector (Row);
               end loop;
            end;
         end loop;
      end return;
   end Solve;


   function Inverse_Matrix (LU : LU_Decomposition) return F_Containers.Matrix is
   begin
      return Result : F_Containers.Matrix := F_Containers.Identity_Matrix (Matrix_Size (LU)) do
         Result := Solve (LU, Result);
      end return;
   end Inverse_Matrix;

   function Determinant (LU : LU_Decomposition) return Float_Type is
      subtype LU_Row is Index_Type range LU.First_Row .. LU.Last_Row;
      subtype LU_Col is Index_Type range LU.First_Col .. LU.Last_Col;
      function Row_To_Col (Row : LU_Row) return LU_Col is (Row - LU_Row'First + LU_Col'First);
   begin
      return Result : Float_Type := LU.Determinant_Multiplier do
         for Row in LU_Row loop
            Result := Result * LU.Data (Row, Row_To_Col (Row));
         end loop;
      end return;
   end Determinant;


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
               LU : constant LU_Decomposition := Crout_LU_Decomposition (Singular) with Unreferenced;
            begin
               Test_Element_Property (False, "should throw an exception when encountering a singular matrix");
            end;
         exception
            when Singular_Matrix => Test_Element_Property (True, "should throw an exception upon encountering a singular matrix");
         end;

         -- Try a normal 1x1 matrix
         declare
            Mat_1x1 : constant F_Containers.Matrix (3 .. 3, 5 .. 5) := 0.1 * F_Containers.Identity_Matrix (1);
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_1x1);
         begin
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 0.1, "should work with a 1x1 matrix");
         end;

         -- Test that 2x2 singular matrices also raise exceptions
         declare
            Singular2 : constant F_Containers.Matrix (15 .. 16, 8 .. 9) := ((1.0, 1.0), (1.0, 1.0));
         begin
            declare
               LU : constant LU_Decomposition := Crout_LU_Decomposition (Singular2);
            begin
               pragma Unreferenced (LU);  -- DEBUG : For some reason, an aspect would make GNAT 2015 unhappy even if it's fine for a 1x1 matrix
               Test_Element_Property (False, "should throw an exception when encountering a singular matrix");
            end;
         exception
            when Singular_Matrix => Test_Element_Property (True, "should throw an exception upon encountering a singular matrix");
         end;

         -- Try it with a lower triangular 2x2 matrix
         declare
            Mat_2x2 : constant F_Containers.Matrix (24 .. 25, 15 .. 16) := ((100.0, 0.0), (13.13, 4.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
         begin
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 100.0, "should work with a lower-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'Last (2)) = 0.0, "should work with a lower-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'First (2)) = 0.1313, "should work with a lower-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'Last (2)) = 4.0, "should work with a lower-triangular 2x2 matrix");
         end;

         -- Try it with an upper-triangular 2x2 matrix
         declare
            Mat_2x2 : constant F_Containers.Matrix (55 .. 56, 44 .. 45) := ((50.0, 18.0), (0.0, 0.5));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
         begin
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 50.0, "should work with an upper-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'Last (2)) = 18.0, "should work with an upper-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'First (2)) = 0.0, "should work with an upper-triangular 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'Last (2)) = 0.5, "should work with an upper-triangular 2x2 matrix");
         end;

         -- Try it with a reverse-diagonal matrix, check that it pivots properly
         declare
            Mat_2x2 : constant F_Containers.Matrix (50 .. 51, 3 .. 4) := ((0.0, 4.0), (0.5, 0.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
         begin
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'First (2)) = 0.5, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'First (1), LU.Data'Last (2)) = 0.0, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'First (2)) = 0.0, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (LU.Data (LU.Data'Last (1), LU.Data'Last (2)) = 4.0, "should work with a reverse-diagonal 2x2 matrix");
         end;

         -- Also try with a 3x3 matrix in order to achieve full coverage
         declare
            Mat_3x3 : constant F_Containers.Matrix (11 .. 13, 31 .. 33) := ((10.0, 100.0, 0.0), (20.0, 0.0, 0.0), (14.0, 50.0, 15.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_3x3);
         begin
            Test_Element_Property (LU.Data (11, 31) = 20.0, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (11, 32) = 0.0, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (11, 33) = 0.0, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (12, 31) = 0.5, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (12, 32) = 100.0, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (12, 33) = 0.0, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (13, 31) = 0.7, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (13, 32) = 0.5, "should work with a 3x3 matrix");
            Test_Element_Property (LU.Data (13, 33) = 15.0, "should work with a 3x3 matrix");
         end;
      end Test_Crout_LU;

      procedure Test_Solve is
      begin
         -- Try to solve for two right-hand sides with a 3x3 matrix
         declare
            Mat_3x3 : constant F_Containers.Matrix (11 .. 13, 31 .. 33) := ((10.0, 100.0, 0.0), (20.0, 0.0, 0.0), (14.0, 50.0, 15.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_3x3);
            RHS_3x2 : constant F_Containers.Matrix (53 .. 55, 8 .. 9) := ((6.0, 0.0), (0.0, 3.0), (0.0, 24.0));
            Result : constant F_Containers.Matrix := Solve (LU, RHS_3x2);
         begin
            Test_Element_Property (Result (53, 8) = 0.0, "should work with a 3x3 matrix and a 3x2 right-hand-side");
            Test_Element_Property (Result (54, 8) = 0.06, "should work with a 3x3 matrix and a 3x2 right-hand-side");
            Test_Element_Property (Result (55, 8) = -0.2, "should work with a 3x3 matrix and a 3x2 right-hand-side");
            Test_Element_Property (Result (53, 9) = 0.15, "should work with a 3x3 matrix and a 3x2 right-hand-side");
            Test_Element_Property (Result (54, 9) = -0.015, "should work with a 3x3 matrix and a 3x2 right-hand-side");
            Test_Element_Property (Result (55, 9) = 1.51, "should work with a 3x3 matrix and a 3x2 right-hand-side");
         end;
      end Test_Solve;

      procedure Test_Inverse is
      begin
         -- Try to inverse a simple reverse-diagonal matrix
         declare
            Mat_2x2 : constant F_Containers.Matrix (50 .. 51, 3 .. 4) := ((0.0, 4.0), (0.5, 0.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
            Mat_Inv : constant F_Containers.Matrix := Inverse_Matrix (LU);
         begin
            Test_Element_Property (Mat_Inv (Mat_Inv'First (1), Mat_Inv'First (2)) = 0.0, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (Mat_Inv (Mat_Inv'First (1), Mat_Inv'Last (2)) = 2.0, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (Mat_Inv (Mat_Inv'Last (1), Mat_Inv'First (2)) = 0.25, "should work with a reverse-diagonal 2x2 matrix");
            Test_Element_Property (Mat_Inv (Mat_Inv'Last (1), Mat_Inv'Last (2)) = 0.0, "should work with a reverse-diagonal 2x2 matrix");
         end;
      end Test_Inverse;

      procedure Test_Determinant is
      begin
         -- Try to compute the determinant of our favorite reverse-diagonal matrix
         declare
            Mat_2x2 : constant F_Containers.Matrix (50 .. 51, 3 .. 4) := ((0.0, 4.0), (0.5, 0.0));
            LU : constant LU_Decomposition := Crout_LU_Decomposition (Mat_2x2);
            Det : constant Float_Type := Determinant (LU);
         begin
            Test_Element_Property (Det = -2.0, "should work with a reverse-diagonal 2x2 matrix");
         end;
      end Test_Determinant;

      procedure Test_Linear_Equations_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Crout_LU_Decomposition"), Test_Crout_LU'Access);
         Test_Package_Element (To_Entity_Name ("Solve"), Test_Solve'Access);
         Test_Package_Element (To_Entity_Name ("Inverse_Matrix"), Test_Inverse'Access);
         Test_Package_Element (To_Entity_Name ("Determinant"), Test_Determinant'Access);
         -- TODO : Test other LU-related methods
      end Test_Linear_Equations_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations.LU_Decomp"), Test_Linear_Equations_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Linear_Equations.LU_Decomp;
