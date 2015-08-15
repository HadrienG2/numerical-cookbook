package body Cookbook.Linear_Equations is

   procedure Swap_Rows (Mat : in out F_Containers.Matrix; Row_1, Row_2 : Index_Type) is
   begin
      for Col in Mat'Range (2) loop
         F_Utility.Swap (Mat (Row_1, Col), Mat (Row_2, Col));
      end loop;
   end Swap_Rows;


   procedure Swap_Cols (Mat : in out F_Containers.Matrix; Col_1, Col_2 : Index_Type) is
   begin
      for Row in Mat'Range (1) loop
         F_Utility.Swap (Mat (Row, Col_1), Mat (Row, Col_2));
      end loop;
   end Swap_Cols;

end Cookbook.Linear_Equations;
