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
