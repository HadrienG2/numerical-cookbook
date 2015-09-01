generic
   type Item_Type is private;
   Zero : Item_Type;
   One : Item_Type;
   with function "+" (Left, Right : Item_Type) return Item_Type is <>;
   with function "*" (Left, Right : Item_Type) return Item_Type is <>;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
package Cookbook.Generic_Containers is

   -- This can be changed to actual vector/matrix classes if the need arises. But that would be inconvenient in Ada as
   -- it is hard to implement custom indexing operators in this language.
   type Vector is array (Index_Type range <>) of Item_Type;
   type Matrix is array (Index_Type range <>, Index_Type range <>) of Item_Type;

   -- Comparison uses the custom equality operator
   overriding function "=" (Left, Right : Matrix) return Boolean
     with
       Pre => (Left'Length (1) = Right'Length (1) and then Left'Length (2) = Right'Length (2));

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

   -- The identity matrix is often useful, so we'll provide a cheap function to generate it
   function Identity_Matrix (Size : Size_Type) return Matrix
     with
       Pre => (Size > 0),
       Post => (Identity_Matrix'Result'Length (1) = Identity_Matrix'Result'Length (2) and then
                      Identity_Matrix'Result'Length (1) = Size);

end Cookbook.Generic_Containers;
