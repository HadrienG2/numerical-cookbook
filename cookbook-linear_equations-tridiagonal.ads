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
package Cookbook.Linear_Equations.Tridiagonal is

   -- Ideally, we would specify the matrix offset and size, and avoid storing nonexistent matrix values
   --
   -- However, the rules of Ada 2012 (RM12 3.8.12/3) forbid the use of expressions containing multiple discriminants. So we have to do without.
   type Tridiagonal_Matrix (First_Index, Last_Index : Index_Type) is
      record
         Lower_Diagonal : F_Containers.Vector (First_Index .. Last_Index); -- Lower_Diagonal(First_Index) is undefined and will not be read
         Diagonal : F_Containers.Vector (First_Index .. Last_Index);
         Upper_Diagonal : F_Containers.Vector (First_Index .. Last_Index); -- Upper_Diagonal(Last_Index) is undefined and will not be read
      end record;

   -- We need a couple of basic operators for tridiagonal matrices as defined above
   function Matrix_Size (Matrix : Tridiagonal_Matrix) return Size_Type is
     (Matrix.Last_Index - Matrix.First_Index + 1);
   function "*" (Left : Tridiagonal_Matrix; Right : F_Containers.Vector) return F_Containers.Vector
     with
       Pre => (Matrix_Size (Left) = Right'Length),
       Post => ("*"'Result'Length = Matrix_Size (Left));

   function Solve (Matrix : Tridiagonal_Matrix; Right_Hand_Vector : F_Containers.Vector) return F_Containers.Vector
     with
       Pre => (Matrix_Size (Matrix) = Right_Hand_Vector'Length),
       Post => (Solve'Result'Length = Right_Hand_Vector'Length and then
                  Matrix * Solve'Result = Right_Hand_Vector);

   -- Test the functions within this package
   procedure Test_Child;

end Cookbook.Linear_Equations.Tridiagonal;
