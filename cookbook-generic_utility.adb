package body Cookbook.Generic_Utility is

   procedure Swap (A, B : in out T) is
      X : constant T := A;
   begin
      A := B; B := X;
   end Swap;

end Cookbook.Generic_Utility;
