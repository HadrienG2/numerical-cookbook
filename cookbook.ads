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
   with function "-" (A, B : Index_Type) return Size_Type is <> with Unreferenced;
   with function "+" (B : Size_Type) return Index_Type is <> with Unreferenced;

package Cookbook is

   -- These are the basic utility macros as defined by Numerical Recipes
   generic
      type T is private;
      with function "<" (A, B : T) return Boolean is <>;
      with function "<" (A : T; B : Float_Type) return Boolean is <>;
      with function "abs" (A : T) return T is <>;
      with function "-" (A : T) return T is <>;
   package Generic_Utility is

      function Max (A, B : T) return T is
        (if A < B then B else A);

      function Min (A, B : T) return T is
        (if A < B then A else B);

      procedure Swap (A, B : in out T)
        with
          Post => (A = B'Old and then B = A'Old),
          Inline;

      function Sign (A, B : T) return T is
        (if B < 0.0 then -abs A else abs A);

   end Generic_Utility;

end Cookbook;
