with Cookbook.Test;

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


   package Test_Runner is new Cookbook.Test;


   procedure Test is
      use Test_Runner;

      procedure Test_Swap_Rows is
         Mat : F_Containers.Matrix (68 .. 70, 55 .. 57) := ((4.0, 2.0, 1.0),
                                                            (7.0, 5.0, 3.0),
                                                            (9.0, 8.0, 6.0));
      begin
         Swap_Rows (Mat, 68, 69);
         Test_Element_Property (Mat = ((7.0, 5.0, 3.0),
                                       (4.0, 2.0, 1.0),
                                       (9.0, 8.0, 6.0)), "should work with the first two rows");
         Swap_Rows (Mat, 68, 70);
         Test_Element_Property (Mat = ((9.0, 8.0, 6.0),
                                       (4.0, 2.0, 1.0),
                                       (7.0, 5.0, 3.0)), "should work with the first and last rows");
         Swap_Rows (Mat, 69, 70);
         Test_Element_Property (Mat = ((9.0, 8.0, 6.0),
                                       (7.0, 5.0, 3.0),
                                       (4.0, 2.0, 1.0)), "should work with the last two rows");
      end Test_Swap_Rows;

      procedure Test_Swap_Cols is
         Mat : F_Containers.Matrix (68 .. 70, 55 .. 57) := ((4.0, 7.0, 9.0),
                                                            (2.0, 5.0, 8.0),
                                                            (1.0, 3.0, 6.0));
      begin
         Swap_Cols (Mat, 55, 56);
         Test_Element_Property (Mat = ((7.0, 4.0, 9.0),
                                       (5.0, 2.0, 8.0),
                                       (3.0, 1.0, 6.0)), "should work with the first two columns");
         Swap_Cols (Mat, 55, 57);
         Test_Element_Property (Mat = ((9.0, 4.0, 7.0),
                                       (8.0, 2.0, 5.0),
                                       (6.0, 1.0, 3.0)), "should work with the first and last columns");
         Swap_Cols (Mat, 56, 57);
         Test_Element_Property (Mat = ((9.0, 7.0, 4.0),
                                       (8.0, 5.0, 2.0),
                                       (6.0, 3.0, 1.0)), "should work with the last two columns");
      end Test_Swap_Cols;

      procedure Test_Linear_Equations_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Swap_Rows"), Test_Swap_Rows'Access);
         Test_Package_Element (To_Entity_Name ("Swap_Cols"), Test_Swap_Cols'Access);
      end Test_Linear_Equations_Package;
   begin
      Test_Package (To_Entity_Name ("Linear_Equations"), Test_Linear_Equations_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test_Runner.Elaboration_Self_Test (Test'Access);

end Cookbook.Linear_Equations;
