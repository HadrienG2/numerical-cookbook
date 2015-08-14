generic
   type Item_Type is private;
   Zero : Item_Type;
   One : Item_Type;
   with function "+" (Left, Right : Item_Type) return Item_Type is <>;
   with function "*" (Left, Right : Item_Type) return Item_Type is <>;
package Cookbook.Generic_Containers is

   -- This can be changed to actual vector/matrix classes if the need arises. But that would be inconvenient in Ada as
   -- it is hard to implement custom indexing operators in this language.
   type Vector is array (Index_Type range <>) of Item_Type;
   type Matrix is array (Index_Type range <>, Index_Type range <>) of Item_Type;

   -- Multiplication by a scalar is business as usual
   function "*" (Left : Item_Type; Right : Matrix) return Matrix
     with
       Post => ("*"'Result'Length (1) = Right'Length (1) and then "*"'Result'Length (2) = Right'Length (2));
   function "*" (Left : Item_Type; Right : Vector) return Vector
     with
       Post => ("*"'Result'Length = Right'Length);

   -- Product between matrices and vectors follow NR's definition
   function "*" (Left, Right : Matrix) return Matrix
     with
       Pre => (Left'Length (2) = Right'Length (1)),
       Post => ("*"'Result'Length (1) = Left'Length (1) and then "*"'Result'Length (2) = Right'Length (2));
   function "*" (Left : Matrix; Right : Vector) return Vector
     with
       Pre => (Left'Length (2) = Right'Length),
       Post => ("*"'Result'Length = Left'Length (1));
   function "*" (Left : Vector; Right : Matrix) return Vector
     with
       Pre => (Left'Length = Right'Length (1)),
       Post => ("*"'Result'Length = Right'Length (2));
   function "*" (Left, Right : Vector) return Item_Type
     with
       Pre => (Left'Length = Right'Length);

   -- Swapping rows and columns is very useful for pivoting algorithms
   procedure Swap_Rows (Mat : in out Matrix; Row_1, Row_2 : Index_Type)
     with
       Pre => (Row_1 in Mat'Range (1) and then Row_2 in Mat'Range (1)),
       Post => (for all J in Mat'Range (2) => Mat (Row_1, J) = Mat'Old (Row_2, J) and then Mat (Row_2, J) = Mat'Old (Row_1, J));
   procedure Swap_Cols (Mat : in out Matrix; Col_1, Col_2 : Index_Type)
     with
       Pre => (Col_1 in Mat'Range (2) and then Col_2 in Mat'Range (2)),
       Post => (for all I in Mat'Range (1) => Mat (I, Col_1) = Mat'Old (I, Col_2) and then Mat (I, Col_2) = Mat'Old (I, Col_1));

   -- The identity matrix is often useful, so we'll provide a cheap function to generate it
   function Identity_Matrix (Size : Size_Type) return Matrix
     with
       Post => (Identity_Matrix'Result'Length (1) = Identity_Matrix'Result'Length (2) and then Identity_Matrix'Result'Length (1) = Size);

end Cookbook.Generic_Containers;
