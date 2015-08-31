package body Cookbook.Linear_Equations.Tridiagonal is

   function "*" (Left : Tridiagonal_Matrix; Right : F_Containers.Vector) return F_Containers.Vector is
   begin
      return Result : F_Containers.Vector (First_Index .. Last_Index) := (others => Zero) do
         -- TODO : Do not forget that the matrix can be 0x0, 1x1 and 2x2
         -- TODO : Handle corner case for first matrix element
         -- TODO : Handle general multiplication case
         -- TODO : Handle corner case for last matrix element
      end return;
   end "*";

end Cookbook.Linear_Equations.Tridiagonal;
