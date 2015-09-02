generic

   -- Floating-point type used by the numerical routines
   -- NOTE : The package expects an approximate float equality operation, rather than exact float equality (which rarely makes sense)
   type Float_Type is digits <>;
   with function "abs" (A : Float_Type) return Float_Type is <> with Unreferenced;
   with function "=" (A, B : Float_Type) return Boolean with Unreferenced;

   -- Discrete type used for array indexes and sizes
   type Index_Type is range <>;
   type Size_Type is range <>; -- Must include 0, obviously
   with function "+" (A : Index_Type; B : Size_Type) return Index_Type is <> with Unreferenced;
   with function "-" (A : Index_Type; B : Size_Type) return Index_Type is <> with Unreferenced;
   with function "-" (A, B : Index_Type) return Size_Type is <> with Unreferenced;
   with function "+" (B : Size_Type) return Index_Type is <> with Unreferenced;

package Cookbook is

   -- This package is empty, its only purposes are...
   --   * To act as a root of the package hierarchy
   --   * To declare the floating-point and array indexing types used by the rest of Numerical Cookbook

end Cookbook;
