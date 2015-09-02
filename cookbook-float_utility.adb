with Cookbook.Generic_Utility;
with Cookbook.Test;

package body Cookbook.Float_Utility is

   package Implementation is new Cookbook.Generic_Utility (Float_Type, "abs" => Cookbook."abs");

   function Max (A, B : Float_Type) return Float_Type renames Implementation.Max;
   function Min (A, B : Float_Type) return Float_Type renames Implementation.Min;
   procedure Swap (A, B : in out Float_Type) renames Implementation.Swap;
   function Sign (A, B : Float_Type) return Float_Type renames Implementation.Sign;

   procedure Test is
      package Test_Runner is new Cookbook.Test;
      use Test_Runner;

      procedure Test_Max is
      begin
         Test_Element_Property (Max (1.0, 1.0) = 1.0, "should work with equal numbers");
         Test_Element_Property (Max (1.0, 2.0) = 2.0, "should work with a right-hand-side maximum");
         Test_Element_Property (Max (2.0, 1.0) = 2.0, "should work with a left-hand-side maximum");
      end Test_Max;

      procedure Test_Min is
      begin
         Test_Element_Property (Min (2.0, 2.0) = 2.0, "should work with equal numbers");
         Test_Element_Property (Min (2.0, 1.0) = 1.0, "should work with a right-hand-side minimum");
         Test_Element_Property (Min (1.0, 2.0) = 1.0, "should work with a left-hand-side minimum");
      end Test_Min;

      procedure Test_Swap is
         A : Float_Type := 1.0;
         B : Float_Type := 2.0;
      begin
         Swap (A, B);
         Test_Element_Property (A = 2.0, "should put the right-hand-side argument on the left-hand-side");
         Test_Element_Property (B = 1.0, "should put the left-hand-side argument on the right-hand-side");
      end Test_Swap;

      procedure Test_Sign is
      begin
         Test_Element_Property (Sign (2.0, 1.0) = 2.0, "of (A,B) should return A when A and B are positive");
         Test_Element_Property (Sign (-2.0, 1.0) = 2.0, "of (A,B) should return -A when A is negative and B is positive");
         Test_Element_Property (Sign (-2.0, -1.0) = -2.0, "of (A,B) should return A when A and B are negative");
         Test_Element_Property (Sign (2.0, -1.0) = -2.0, "of (A,B) should return -A when A is positive and B is negative");
      end Test_Sign;

      procedure Test_Utility_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Max"), Test_Max'Access);
         Test_Package_Element (To_Entity_Name ("Min"), Test_Min'Access);
         Test_Package_Element (To_Entity_Name ("Swap"), Test_Swap'Access);
         Test_Package_Element (To_Entity_Name ("Sign"), Test_Sign'Access);
      end Test_Utility_Package;
   begin
      Test_Package (To_Entity_Name ("Float_Utility"), Test_Utility_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Float_Utility;
