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


with Cookbook.Float_Containers, Cookbook.Float_Utility;

generic
   with package F_Utility is new Cookbook.Float_Utility;
   with package F_Containers is new Cookbook.Float_Containers;
package Cookbook.Linear_Equations is

   use type F_Containers.Matrix, F_Containers.Vector;

   -- It is unfeasible to invert singular matrices. If such an attempt is detected, the following exception will be thrown.
   Singular_Matrix : exception;

   -- In this package, we'll be using square matrices a lot, so let's define what we mean by that
   function Is_Square_Matrix (Matrix : F_Containers.Matrix) return Boolean is
      (Matrix'Length (1) = Matrix'Length (2));

   -- Swapping rows and columns is very useful for pivoting algorithms
   procedure Swap_Rows (Mat : in out F_Containers.Matrix; Row_1, Row_2 : Index_Type)
     with
       Pre => (Row_1 in Mat'Range (1) and then Row_2 in Mat'Range (1)),
       Post => (for all Col in Mat'Range (2) =>
                  (Mat (Row_1, Col) = Mat'Old (Row_2, Col) and then Mat (Row_2, Col) = Mat'Old (Row_1, Col)));
   procedure Swap_Cols (Mat : in out F_Containers.Matrix; Col_1, Col_2 : Index_Type)
     with
       Pre => (Col_1 in Mat'Range (2) and then Col_2 in Mat'Range (2)),
       Post => (for all Row in Mat'Range (1) =>
                  (Mat (Row, Col_1) = Mat'Old (Row, Col_2) and then Mat (Row, Col_2) = Mat'Old (Row, Col_1)));

   -- Test the functions within this package
   procedure Test;

end Cookbook.Linear_Equations;
