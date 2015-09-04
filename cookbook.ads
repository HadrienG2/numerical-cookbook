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

   -- Floating-point type used by the numerical routines
   -- NOTE : The package expects an approximate float equality operation, rather than exact float equality (which rarely makes sense).
   type Float_Type is digits <>;
   with function "abs" (A : Float_Type) return Float_Type is <> with Unreferenced;
   with function "=" (A, B : Float_Type) return Boolean with Unreferenced;

   -- Discrete type used for array indexes and sizes
   type Index_Type is range <>;
   type Size_Type is range <>; -- NOTE : This integral type must cover 0, obviously
   with function "+" (A : Index_Type; B : Size_Type) return Index_Type is <> with Unreferenced;
   with function "-" (A : Index_Type; B : Size_Type) return Index_Type is <> with Unreferenced;
   with function "-" (A, B : Index_Type) return Size_Type is <> with Unreferenced;

package Cookbook is

   -- This package is empty, its only purposes are...
   --   * To act as a root of the Numerical Cookbook package hierarchy
   --   * To declare the floating-point and array indexing types used by the rest of the Numerical Cookbook suite

end Cookbook;
