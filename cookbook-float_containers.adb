with Cookbook.Test;

package body Cookbook.Float_Containers is

   use type Matrix, Vector;


   package Test_Runner is new Cookbook.Test;


   procedure Test is
      use Test_Runner;

      -- TODO : Add tests for all containers operations (as specified in generic_containers.ads)

      procedure Test_Identity_Matrix is
      begin
         -- NOTE : 0x0 identity matrices are forbidden by a precondition, so we don't have to test for them

         declare
            Id_1x1 : constant Matrix := Identity_Matrix (1);
         begin
            Test_Element_Property (Id_1x1'Length (1) = 1, "should produce 1x1 identity matrices");
            Test_Element_Property (Id_1x1'Length (2) = 1, "should produce 1x1 identity matrices");
            Test_Element_Property (Id_1x1 (Id_1x1'First (1), Id_1x1'First (2)) = 1.0, "should produce 1x1 identity matrices");
         end;

         declare
            Id_2x2 : constant Matrix := Identity_Matrix (2);
         begin
            Test_Element_Property (Id_2x2 = ((1.0, 0.0), (0.0, 1.0)), "should produce 2x2 identity matrices");
         end;
      end Test_Identity_Matrix;

      procedure Test_Containers_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Vector_Equality"), Test_Vector_Equality'Access);
         -- TODO : Run tests for all containers operations (as specified in generic_containers.ads)
         Test_Package_Element (To_Entity_Name ("Identity_Matrix"), Test_Identity_Matrix'Access);
      end Test_Containers_Package;
   begin
      Test_Package (To_Entity_Name ("Float_Containers"), Test_Containers_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test_Runner.Elaboration_Self_Test (Test'Access);

end Cookbook.Float_Containers;
