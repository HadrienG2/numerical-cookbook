with Cookbook.Generic_Containers;

generic
package Cookbook.Float_Containers is

   package Implementation is new Cookbook.Generic_Containers (Item_Type => Float_Type,
                                                              Zero => 0.0,
                                                              One => 1.0,
                                                              "=" => Cookbook."=");

   subtype Vector is Implementation.Vector;
   subtype Matrix is Implementation.Matrix;

   function Identity_Matrix (Size : Size_Type) return Matrix renames Implementation.Identity_Matrix;

   -- Test the functions within this package
   procedure Test;

end Cookbook.Float_Containers;
