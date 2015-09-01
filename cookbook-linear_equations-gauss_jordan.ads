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
   procedure Test;

end Cookbook.Linear_Equations.Gauss_Jordan;
