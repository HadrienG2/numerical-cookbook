generic
package Cookbook.Float_Utility is

   -- Under the hood, this is just an implementation of Cookbook.Generic_Utility for Float_Type
   function Max (A, B : Float_Type) return Float_Type
     with Inline;
   function Min (A, B : Float_Type) return Float_Type
     with Inline;
   procedure Swap (A, B : in out Float_Type)
     with Inline;
   function Sign (A, B : Float_Type) return Float_Type
     with Inline;

   -- Test the functions within this package
   procedure Test;

end Cookbook.Float_Utility;
