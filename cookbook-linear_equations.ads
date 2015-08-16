with Cookbook.Float_Containers, Cookbook.Float_Utility;

generic
   with package F_Utility is new Cookbook.Float_Utility;
   with package F_Containers is new Cookbook.Float_Containers;
package Cookbook.Linear_Equations is

   use type F_Containers.Matrix, F_Containers.Vector;

   Singular_Matrix : exception;

   -- In this package, we'll be using square matrices a lot, so let's define what we mean by that
   function Is_Square_Matrix (Matrix : F_Containers.Matrix) return Boolean is
      (Matrix'Length (1) = Matrix'Length (2));

   -- Swapping rows and columns is very useful for pivoting algorithms
   procedure Swap_Rows (Mat : in out F_Containers.Matrix; Row_1, Row_2 : Index_Type)
     with
       Pre => (Row_1 in Mat'Range (1) and then Row_2 in Mat'Range (1)),
       Post => (for all J in Mat'Range (2) => Mat (Row_1, J) = Mat'Old (Row_2, J) and then Mat (Row_2, J) = Mat'Old (Row_1, J));
   procedure Swap_Cols (Mat : in out F_Containers.Matrix; Col_1, Col_2 : Index_Type)
     with
       Pre => (Col_1 in Mat'Range (2) and then Col_2 in Mat'Range (2)),
       Post => (for all I in Mat'Range (1) => Mat (I, Col_1) = Mat'Old (I, Col_2) and then Mat (I, Col_2) = Mat'Old (I, Col_1));

end Cookbook.Linear_Equations;
