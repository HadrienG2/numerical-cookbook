with Cookbook.Test;

package body Cookbook.Float_Containers is

   procedure Test is
      package Test_Runner is new Cookbook.Test;
      use Test_Runner;

      procedure Test_Identity_Matrix is
      begin
         -- TODO : Test the identity matrix generation routine
      end Test_Identity_Matrix;

      -- TODO : Add tests for all containers operations (as specified in generic_containers.ads)

      procedure Test_Containers_Package is
      begin
         Test_Package_Element (To_Entity_Name ("Identity_Matrix"), Test_Identity_Matrix'Access);
         -- TODO : Run tests for all containers operations (as specified in generic_containers.ads)
      end Test_Containers_Package;
   begin
      Test_Package (To_Entity_Name ("Float_Containers"), Test_Containers_Package'Access);
   end Test;

begin

   -- Automatically test the package when it is included
   Test;

end Cookbook.Float_Containers;
