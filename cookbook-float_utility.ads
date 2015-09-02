with Cookbook.Generic_Utility;

generic
package Cookbook.Float_Utility is

   package Implementation is new Cookbook.Generic_Utility (Float_Type,
                                                           "abs" => Cookbook."abs");

   function Max (A, B : Float_Type) return Float_Type renames Implementation.Max;
   function Min (A, B : Float_Type) return Float_Type renames Implementation.Min;
   procedure Swap (A, B : in out Float_Type) renames Implementation.Swap;
   function Sign (A, B : Float_Type) return Float_Type renames Implementation.Sign;

   -- Test the functions within this package
   procedure Test;

end Cookbook.Float_Utility;
