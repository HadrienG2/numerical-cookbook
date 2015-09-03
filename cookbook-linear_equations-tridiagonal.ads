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

   -- Test the functions within this package
   procedure Test_Child;

end Cookbook.Linear_Equations.Tridiagonal;
