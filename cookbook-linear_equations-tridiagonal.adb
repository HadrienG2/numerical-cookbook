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

   procedure Test is
      package Test_Runner is new Cookbook.Test;
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
            Vec_0x0 : constant F_Containers.Vector (69 .. 68) := (others => <>);
            Result : constant F_Containers.Vector := Mat_0x0 * Vec_0x0 with Unreferenced;
         begin
            Test_Element_Property (True, "should work with 0x0 matrices");
         end;

         -- Test that 1x1 matrices (special case !) work
         declare
            Mat_1x1 : constant Tridiagonal_Matrix := (First_Index => Index_Type'First,
                                                      Last_Index => Index_Type'First,
                                                      Lower_Diagonal => (others => <>),
                                                      Diagonal => (others => 1.5),
                                                      Upper_Diagonal => (others => <>));
            Vec_1x1 : constant F_Containers.Vector (42 .. 42) := (others => 2.0);
            Result : constant F_Containers.Vector := Mat_1x1 * Vec_1x1;
         begin
            Test_Element_Property (Result (Result'First) = 3.0, "should work with 1x1 matrices");
         end;

         -- Test that 2x2 matrices (special case !) work
         declare
            Mat_2x2 : constant Tridiagonal_Matrix := (First_Index => 55,
                                                      Last_Index => 56,
                                                      Lower_Diagonal => (0.0, 33.0),    -- aka ((44.0, 1.25),
                                                      Diagonal => (44.0, 0.5),          --      (33.0, 0.5))
                                                      Upper_Diagonal => (1.25, 0.0));
            Vec_2x2 : constant F_Containers.Vector (22 .. 23) := (12.0, 15.0);
            Result : constant F_Containers.Vector := Mat_2x2 * Vec_2x2;
         begin
            Test_Element_Property (Result = (546.75, 403.5), "should work with 2x2 matrices");
         end;

         -- TODO : Try a 3x3 matrix (single loop execution)
         -- TODO : Try a 4x4 matrix (two loop executions)
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
