-- Copyright 2015 Hadrien Grasland
--
-- This file is part of Numerical Cookbook.
--
-- Numerical Cookbook is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Numerical Cookbook is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Numerical Cookbook.  If not, see <http://www.gnu.org/licenses/>.


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
