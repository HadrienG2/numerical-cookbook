with Cookbook.Float_Containers, Cookbook.Float_Utility;

generic
   with package F_Utility is new Cookbook.Float_Utility;
   with package F_Containers is new Cookbook.Float_Containers;
package Cookbook.Linear_Equations is

   use type F_Containers.Matrix;

   Singular_Matrix : exception;

   -- Linear equation solver using in place Gauss-Jordan elimination.
   --   * Takes as input a square matrix A, and optionally a set of right-hand-side vectors Bm packed in a matrix
   --   * Stores the inverse of A in place of A, and the solution vectors Xm such that A*Xm = Bm in place of the Bm
   -- Due to the in-place nature of the algorithm, if an exception arises, the original matrices should be considered destroyed
   --
   -- Even if the preconditions are satisfied, the function may still throw Singular_Matrix if the input is singular.
   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix; Right_Hand_Vectors : in out F_Containers.Matrix)
     with
       Pre => (Matrix'Length (1) = Matrix'Length (2) and then
                     Matrix'Length (1) = Right_Hand_Vectors'Length (1)),
       Post => (Matrix * Matrix'Old = F_Containers.Identity_Matrix (Matrix'Length (1)) and then
                  Right_Hand_Vectors = Matrix * Right_Hand_Vectors'Old);
   procedure Gauss_Jordan_Elimination (Matrix : in out F_Containers.Matrix)
     with
       Pre => (Matrix'Length (1) = Matrix'Length (2)),
       Post => (Matrix * Matrix'Old = F_Containers.Identity_Matrix (Matrix'Length (1)));

   -- LU decompositions are stored inside an opaque object, which allows for some storage optimizations.
   -- Components are indirectly available through member functions.
   type LU_Decomposition(<>) is private
     with
       Type_Invariant => Lower (LU_Decomposition) * Upper (LU_Decomposition) = Original_Matrix (LU_Decomposition);
   function Matrix_Size (LU : LU_Decomposition) return Size_Type;
   function Lower (LU : LU_Decomposition) return F_Containers.Matrix
     with
       Post => (Lower'Result'Length (1) = Lower'Result'Length (2) and then
                      Lower'Result'Length (1) = Matrix_Size (LU));
   function Upper (LU : LU_Decomposition) return F_Containers.Matrix
     with
       Post => (Upper'Result'Length (1) = Upper'Result'Length (2) and then
                      Upper'Result'Length (1) = Matrix_Size (LU));
   function Original_Matrix (LU : LU_Decomposition) return F_Containers.Matrix
     with
       Post => (Original_Matrix'Result'Length (1) = Original_Matrix'Result'Length (2) and then
                      Original_Matrix'Result'Length (1) = Matrix_Size (LU));

   -- LU decomposition may be computed using Crout's algorithm, and opens many possibilities for efficient computations on the original matrix
   --
   -- As before, the algorithm may throw Singular_Matrix even if all preconditions are satisfied if the input is singular.
   function Crout_LU_Decomposition (Matrix : F_Containers.Matrix) return LU_Decomposition
     with
       Pre => (Matrix'Length (1) = Matrix'Length (2)),
       Post => (Matrix_Size (Crout_LU_Decomposition'Result) = Matrix'Length (1) and then
                      Lower (Crout_LU_Decomposition'Result) * Upper (Crout_LU_Decomposition'Result) = Matrix);
   -- TODO : Add RHS solver, inverse computation, determinant, and all that jazz, with appropriate pre/postconditions


   -- Test the functions within this package
   procedure Test;

private

   -- Ideally, we would specify the matrix offset and size instead of the full matrix bounds, to avoid information duplication between Last_Row
   -- and Last_Col along with a risk of ending up with a non-square matrix in the event of a coding error.
   --
   -- However, the rules of Ada 2012 (RM12 3.8.12/3) forbid the use of expressions containing multiple discriminants, like "First_Row + Mat_Size",
   -- in many scenarios including the definition of array boundaries.
   type LU_Decomposition (First_Row, Last_Row, First_Col, Last_Col : Index_Type) is
      record
         Decomposition : F_Containers.Matrix (First_Row .. Last_Row, First_Col .. Last_Col);
         Determinant_Multiplier : Float_Type;
         -- TODO : Add components to store permutations, etc.
      end record;

   function Matrix_Size (LU : LU_Decomposition) return Size_Type is
      (LU.Last_Row - LU.First_Row + 1);

end Cookbook.Linear_Equations;
