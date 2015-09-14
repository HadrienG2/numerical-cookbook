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
   --
   -- The tridiagonal matrix is stored in the structure as follows :
   --
   -- Diagonal (First_Row)                 Upper_Diagonal (First_Row)           0                                   0
   -- Lower_Diagonal (First_Row + 1)       Diagonal (First_Row + 1)             Upper_Diagonal(First_Row + 1)       0
   -- 0                                    Lower_Diagonal (First_Row + 2)       Diagonal (First_Row + 2)            Upper_Diagonal (First_Row + 2)
   --
   type Tridiagonal_Matrix (First_Row, Last_Row : Index_Type) is
      record
         Lower_Diagonal : F_Containers.Vector (First_Row .. Last_Row); -- Lower_Diagonal(First_Row) is undefined and will not be read
         Diagonal : F_Containers.Vector (First_Row .. Last_Row);
         Upper_Diagonal : F_Containers.Vector (First_Row .. Last_Row); -- Upper_Diagonal(Last_Row) is undefined and will not be read
      end record;

   -- We need a couple of basic operators for tridiagonal matrices as defined above
   function Matrix_Size (Matrix : Tridiagonal_Matrix) return Size_Type is
     (Matrix.Last_Row - Matrix.First_Row + 1);
   function "*" (Left : Tridiagonal_Matrix; Right : F_Containers.Vector) return F_Containers.Vector
     with
       Pre => (Matrix_Size (Left) = Right'Length),
       Post => ("*"'Result'Length = Matrix_Size (Left));

   -- The efficiency of solving dridiagonal systems comes at the price of decreased stability : a zero pivot may be encountered even if
   -- the matrix is not singular. For example, this algorithm will fail on the invertible 2x2 matrix ((0.0, 1.0), (1.0, 0.0)).
   --
   -- The tridiagonal solver throws a special exception in place of Singular_Matrix in order to clarify this.
   Zero_Pivot_Encountered : exception;
   function Solve (Matrix : Tridiagonal_Matrix; Right_Hand_Vector : F_Containers.Vector) return F_Containers.Vector
     with
       Pre => (Matrix_Size (Matrix) = Right_Hand_Vector'Length and then
                     Matrix_Size (Matrix) > 0),
       Post => (Solve'Result'Length = Right_Hand_Vector'Length and then
                  Matrix * Solve'Result = Right_Hand_Vector);

   -- Test the functions within this package
   procedure Test_Child;

end Cookbook.Linear_Equations.Tridiagonal;
