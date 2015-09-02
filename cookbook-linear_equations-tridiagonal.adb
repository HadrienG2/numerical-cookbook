with Cookbook.Test;

package body Cookbook.Linear_Equations.Tridiagonal is

   function "*" (Left : Tridiagonal_Matrix; Right : F_Containers.Vector) return F_Containers.Vector is
      -- We will need to manipulate indices for the left matrix...
      subtype Left_Index is Index_Type range Left.First_Index .. Left.Last_Index;
      Second_Left_Index : constant Left_Index := Left_Index'Succ (Left_Index'First);
      Left_Index_Before_Last : constant Left_Index := Left_Index'Pred (Left_Index'Last);

      -- ...as well as rows of the right vector...
      subtype Right_Row is Index_Type range Right'First .. Right'Last;
      Second_Right_Row : constant Right_Row := Right_Row'Succ (Right_Row'First);
      Right_Row_Before_Last : constant Right_Row := Right_Row'Pred (Right_Row'Last);

      -- ...and to convert between both
      function Left_Col_To_Right_Row (Col : Left_Index) return Right_Row is (Col - Left_Index'First + Right_Row'First);
   begin
      return Result : F_Containers.Vector (Left_Index) do
         if Matrix_Size (Left) >= 2 then
            -- The first element is special because there is no lower diagonal element
            Result (Left_Index'First) := Left.Diagonal (Left_Index'First) * Right (Right_Row'First) +
              Left.Upper_Diagonal (Left_Index'First) * Right (Second_Right_Row);

            -- This is the general case for matrix elements located in the middle of the matrix
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

            -- The last element is special because there is no upper diagonal element
            Result (Left_Index'Last) := Left.Lower_Diagonal (Left_Index'Last) * Right (Right_Row_Before_Last) +
              Left.Diagonal (Left_Index'Last) * Right (Right_Row'Last);
         elsif Matrix_Size (Left) = 1 then
            -- The one-element matrix case is also special because there are no diagonals to speak of
            Result (Left_Index'First) := Left.Diagonal (Left_Index'First) * Right (Right_Row'First);
         end if;
      end return;
   end "*";

   procedure Test is
      package Test_Runner is new Cookbook.Test;
      use Test_Runner;

      procedure Test_MatVecMul is
      begin
         -- TODO : Try a 0x0 matrix
         -- TODO : Try a 1x1 matrix
         -- TODO : Try a 2x2 matrix
         -- TODO : Try a 3x3 matrix
         -- TODO : Try a 4x4 matrix
      end Test_MatVecMul;

      procedure Test_Tridiagonal_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Matrix_Vector_Multiplication"), Test_MatVecMul'Access);
      end Test_Tridiagonal_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations.Tridiagonal"), Test_Tridiagonal_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Linear_Equations.Tridiagonal;
