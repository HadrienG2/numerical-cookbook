generic
   type T is private;
   with function "<" (A, B : T) return Boolean is <>;
   with function "<" (A : T; B : Float_Type) return Boolean is <>;
   with function "abs" (A : T) return T is <>;
   with function "-" (A : T) return T is <>;
package Cookbook.Generic_Utility is

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

end Cookbook.Generic_Utility;
