with Cookbook;
with Cookbook.Float_Containers;
with Cookbook.Float_Utility;
with Cookbook.Linear_Equations;
with Cookbook.Linear_Equations.Gauss_Jordan;
with Cookbook.Linear_Equations.LU_Decomp;
with Cookbook.Linear_Equations.Tridiagonal;

procedure Main is
   -- We take float equality as equality up to a difference of one significant digit for nonzero numbers,
   -- and absolute value less than float prevision for comparison with zero.
   function Relatively_Equal (F1, F2 : Float) return Boolean is
      Comparison_Resolution : constant Float := 10.0**(-(Float'Digits - 1));
      Min_F : constant Float := (if (abs F1) < (abs F2) then F1 else F2);
      Max_F : constant Float := (if (abs F1) > (abs F2) then F1 else F2);
   begin
      if Min_F /= 0.0 then
         return abs ((Max_F - Min_F) / Min_F) < Comparison_Resolution;
      else
         return abs Max_F < Comparison_Resolution;
      end if;
   end Relatively_Equal;

   -- Instantiate the cookbook
   package Float_Cookbook is new Cookbook (Float_Type => Float, "=" => Relatively_Equal, Index_Type => Positive, Size_Type => Natural);
   package Float_Utility is new Float_Cookbook.Float_Utility;
   package Float_Containers is new Float_Cookbook.Float_Containers;
   package Linear_Equations is new Float_Cookbook.Linear_Equations (F_Containers => Float_Containers,
                                                                    F_Utility => Float_Utility);
   package Gauss_Jordan is new Linear_Equations.Gauss_Jordan with Unreferenced;
   package LU_Decomp is new Linear_Equations.LU_Decomp with Unreferenced;
   package Tridiagonal is new Linear_Equations.Tridiagonal with Unreferenced;
begin
   null;
end Main;
