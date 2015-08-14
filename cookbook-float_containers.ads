with Cookbook.Generic_Containers;

generic
package Cookbook.Float_Containers is

   package Implementation is new Cookbook.Generic_Containers (Item_Type => Float_Type, Zero => 0.0, One => 1.0);

   subtype Vector is Implementation.Vector;
   subtype Matrix is Implementation.Matrix;

   procedure Swap_Rows (Mat : in out Matrix; Row_1, Row_2 : Index_Type) renames Implementation.Swap_Rows;
   procedure Swap_Cols (Mat : in out Matrix; Col_1, Col_2 : Index_Type) renames Implementation.Swap_Cols;

   function Identity_Matrix (Size : Size_Type) return Matrix renames Implementation.Identity_Matrix;

   -- TODO : Implement unit tests once matrix functionality has stabilized

end Cookbook.Float_Containers;
