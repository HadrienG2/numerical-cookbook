package body Cookbook.Generic_Containers is

   package Item_Utility is new Cookbook.Generic_Utility (Item_Type);


   function "*" (Left : Item_Type; Right : Matrix) return Matrix is
   begin
      return Result : Matrix (Right'Range (1), Right'Range (2)) do
         for I in Right'Range (1) loop
            for J in Right'Range (2) loop
               Result (I, J) := Left * Right (I, J);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Item_Type; Right : Vector) return Vector is
   begin
      return Result : Vector (Right'Range) do
         for I in Right'Range loop
            Result (I) := Left * Right (I);
         end loop;
      end return;
   end "*";


   function "*" (Left, Right : Matrix) return Matrix is
   begin
      return Result : Matrix (Left'Range (1), Right'Range (2)) := (others => (others => Zero)) do
         for I in Left'Range (1) loop
            for K in Right'Range (2) loop
               for J in Left'Range (2) loop
                  Result (I, K) := Result (I, K) + Left (I, J) * Right (J - Left'First (2) + Right'First (1), K);
               end loop;
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Matrix; Right : Vector) return Vector is
   begin
      return Result : Vector (Left'Range (1)) := (others => Zero) do
         for I in Left'Range (1) loop
            for J in Left'Range (2) loop
               Result (I) := Result (I) + Left (I, J) * Right (J - Left'First (2) + Right'First);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Vector; Right : Matrix) return Vector is
   begin
      return Result : Vector (Right'Range (2)) := (others => Zero) do
         for I in Left'Range loop
            for J in Right'Range (2) loop
               Result (J) := Result (J) + Left (I) * Right (I - Left'First + Right'First (1), J);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left, Right : Vector) return Item_Type is
   begin
      return Result : Item_Type := Zero do
         for I in Left'Range loop
            Result := Result + Left (I) * Right (I - Left'First + Right'First);
         end loop;
      end return;
   end "*";

   procedure Swap_Rows (Mat : in out Matrix; Row_1, Row_2 : Index_Type) is
   begin
      for Col in Mat'Range (2) loop
         Item_Utility.Swap (Mat (Row_1, Col), Mat (Row_2, Col));
      end loop;
   end Swap_Rows;


   procedure Swap_Cols (Mat : in out Matrix; Col_1, Col_2 : Index_Type) is
   begin
      for Row in Mat'Range (1) loop
         Item_Utility.Swap (Mat (Row, Col_1), Mat (Row, Col_2));
      end loop;
   end Swap_Cols;


   function Identity_Matrix (Size : Size_Type) return Matrix is
   begin
      return Result : Matrix (Index_Type'First .. Index_Type'First + Size - 1,
                              Index_Type'First .. Index_Type'First + Size - 1) := (others => (others => Zero)) do
         for I in Result'Range (1) loop
            Result (I, I) := One;
         end loop;
      end return;
   end Identity_Matrix;

end Cookbook.Generic_Containers;
