package body Cookbook is

   package body Generic_Utility is

      procedure Swap (A, B : in out T) is
         X : constant T := A;
      begin
         A := B; B := X;
      end Swap;

   end Generic_Utility;

end Cookbook;
