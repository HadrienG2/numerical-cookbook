-- Copyright 2015 Hadrien Grasland
--
-- This file is part of Numerical Cookbook.
--
-- Numerical Cookbook is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Numerical Cookbook is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Numerical Cookbook.  If not, see <http://www.gnu.org/licenses/>.


with Cookbook.Test;

package body Cookbook.Float_Containers is

   use type Matrix, Vector;


   package Test_Runner is new Cookbook.Test;


   procedure Test is
      use Test_Runner;

      procedure Test_Vector_Equality is
      begin
         declare
            Vec0_1 : constant Vector (55 .. 54) := (others => <>);
            Vec0_2 : constant Vector (2 .. 1) := (others => <>);
         begin
            Test_Element_Property (Vec0_1 = Vec0_2, "should work with vectors of size 0");
         end;

         declare
            Vec1_1 : constant Vector (42 .. 42) := (42 => 0.5);
            Vec1_2 : constant Vector (2 .. 2) := (2 => 0.5);
            Vec1_3 : constant Vector (16 .. 16) := (16 => 0.6);
         begin
            Test_Element_Property (Vec1_1 = Vec1_2, "should work with equal vectors of size 1");
            Test_Element_Property (Vec1_1 /= Vec1_3, "should work with distinct vectors of size 1");
         end;

         declare
            Vec_2_1 : constant Vector (5 .. 6) := (0.1, 0.2);
            Vec_2_2 : constant Vector (86 .. 87) := (0.1, 0.2);
            Vec_2_3 : constant Vector (100 .. 101) := (0.2, 0.2);
            Vec_2_4 : constant Vector (52 .. 53) := (0.1, 0.3);
            Vec_2_5 : constant Vector (24 .. 25) := (0.5, 0.6);
         begin
            Test_Element_Property (Vec_2_1 = Vec_2_2, "should work with equal vectors of size 2");
            Test_Element_Property (Vec_2_1 /= Vec_2_3, "should work with vectors of size 2 differing by first component");
            Test_Element_Property (Vec_2_1 /= Vec_2_4, "should work with vectors of size 2 differing by second component");
            Test_Element_Property (Vec_2_1 /= Vec_2_5, "should work with vectors of size 2 differing by both components");
         end;
      end Test_Vector_Equality;

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
