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
package Cookbook.Linear_Equations.Gauss_Jordan is

   -- Linear equation solver using in place Gauss-Jordan elimination.
   --   * Takes as input a square matrix A, and optionally a set of right-hand-side vectors Bm packed in a matrix
   --   * Stores the inverse of A in place of A, and the solution vectors Xm such that A*Xm = Bm in place of the Bm
   -- Due to the in-place nature of the algorithm, if an exception arises, the original matrices should be considered destroyed
   --
   -- Even if the preconditions are satisfied, the function may still throw Singular_Matrix if the input is singular.
   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix; Right_Hand_Vectors : in out F_Containers.Matrix)
     with
       Pre => (Is_Square_Matrix (Matrix) and then Matrix'Length (1) = Right_Hand_Vectors'Length (1) and then
                  Matrix'Length (1) > 0),
       Post => (Matrix * Matrix'Old = F_Containers.Identity_Matrix (Matrix'Length (1)) and then
                  Right_Hand_Vectors = Matrix * Right_Hand_Vectors'Old);
   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix)
     with
       Pre => (Is_Square_Matrix (Matrix) and then Matrix'Length (1) > 0),
       Post => (Matrix * Matrix'Old = F_Containers.Identity_Matrix (Matrix'Length (1)));

   -- Test the functions within this package
   procedure Test_Child;

end Cookbook.Linear_Equations.Gauss_Jordan;
