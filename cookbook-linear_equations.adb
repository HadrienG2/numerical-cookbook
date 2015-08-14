package body Cookbook.Linear_Equations is

   function Crout_LU_Decomposition (Matrix : F_Containers.Matrix) return LU_Decomposition is
   begin
      -- We begin by declaring the LU object, including the matrix copy, and will never work on the original
      -- matrix directly afterwards. This eases changing its definition later (e.g. using different row/column indexing).
      return LU : LU_Decomposition := (First_Row => Matrix'First (1),
                                       Last_Row => Matrix'Last (1),
                                       First_Col => Matrix'First (2),
                                       Last_Col => Matrix'Last (2),
                                       Decomposition => Matrix,
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

            -- Column by column, operating simultaneously on identically numbered rows, reduce the matrix copy to its LU decomposition.
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
                           Scaled_Item_Abs : Float_Type := Rows_Scaling (Row) * abs LU.Decomposition (Row, Pivot_Col);
                        begin
                           if Scaled_Item_Abs > Scaled_Pivot_Abs then
                              Scaled_Pivot_Abs := Scaled_Item_Abs;
                              Pivot_Row := Row;
                           end if;
                        end;
                     end loop;

                     -- If we can't find anything better than zero, it means that the matrix is singular. Unlike NR, I choose to consistently raise.
                     if Scaled_Pivot_Abs = 0.0 then
                        raise Singular_Matrix;
                     end if;
                  end;

                  -- If the pivot is not on the diagonal, put it there by swapping rows. Note that this also changes determinant sign and row scaling.
                  LU.Initial_Row_Positions (Col_To_Row (Pivot_Col)) := Pivot_Row;
                  if Pivot_Row /= Col_To_Row (Pivot_Col) then
                     F_Containers.Swap_Rows (LU.Decomposition, Pivot_Row, Col_To_Row (Pivot_Col));
                     LU.Determinant_Multiplier := -LU.Determinant_Multiplier;
                     F_Utility.Swap (Rows_Scaling (Pivot_Row), Rows_Scaling (Col_To_Row (Pivot_Col)));
                     Pivot_Row := Col_To_Row (Pivot_Col);
                  end if;

                  -- Divide the remainder of the pivot's column by the pivot element and reduce the remaining submatrix
                  declare
                     Pivot_Element : Float_Type := LU.Decomposition (Pivot_Row, Pivot_Col);
                  begin
                     for Row in Pivot_Row + Index_Type'(1) .. LU.Last_Row loop
                        LU.Decomposition (Row, Pivot_Col) := LU.Decomposition (Row, Pivot_Col) / Pivot_Element;
                        declare
                           Pivot_Col_Value : Float_Type := LU.Decomposition (Row, Pivot_Col);
                        begin
                           for Col in Pivot_Col + Index_Type'(1) .. LU.Last_Col loop
                              LU.Decomposition (Row, Col) := LU.Decomposition (Row, Col) - Pivot_Col_Value*LU.Decomposition (Pivot_Row, Col);
                           end loop;
                        end;
                     end loop;
                  end;
               end;
            end loop;
         end;
      end return;
   end Crout_LU_Decomposition;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Linear_Equations;
