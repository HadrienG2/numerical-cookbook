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
package Cookbook.Linear_Equations.Band_Diagonal is

   -- To store and process band-diagonal matrices, we need...
   --   * The amount of matrix elements below and above the matrix' diagonal
   --   * A dataset featuring the nonzero elements of every row of the matrix
   --
   -- The following data structure provides all of this. For a matrix with 1 sub-diagonal element and 2 super-diagonal elements, the actual matrix
   -- would look like this :
   --
   -- Data(First_Row,     Diagonal_Col)        Data(First_Row,     Diagonal_Col + 1)    Data(First_Row,     Diagonal_Col + 2)    0
   -- Data(First_Row + 1, Diagonal_Col - 1)    Data(First_Row + 1, Diagonal_Col)        Data(First_Row + 1, Diagonal_Col + 1)    Data(First_Row + 1, Diagonal_Col + 2)
   -- 0                                        Data(First_Row + 2, Diagonal_Col - 1)    Data(First_Row + 2, Diagonal_Col)        Data(First_Row + 2, Diagonal_Col + 1)
   --
   type Band_Diagonal_Matrix (First_Row, Last_Row, First_Col, Last_Col : Index_Type;
                              Elements_Below_Diag, Elements_Above_Diag : Index_Type) is
      record
         Diagonal_Col : Index_Type := First_Col + Elements_Below_Diag;  -- Should be constant, but that is forbidden by Ada 2012
         Data : F_Containers.Matrix (First_Row .. Last_Row, First_Col .. Last_Col);
      end record;

end Cookbook.Linear_Equations.Band_Diagonal;
