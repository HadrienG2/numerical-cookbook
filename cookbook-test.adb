with Ada.Assertions;
with Ada.Text_IO;

package body Cookbook.Test is

   use type Entity_Name;

   Global_Element_Prefix : Entity_Name;

   procedure Elaboration_Self_Test (Testing_Procedure : not null access procedure) is
   begin
      if Test_On_Elaboration then
         Testing_Procedure.all;
      end if;
   end Elaboration_Self_Test;

   function Assertions_Enabled return Boolean is
   begin
      pragma Assert (False);
      return False;
   exception
      when Ada.Assertions.Assertion_Error => return True;
   end Assertions_Enabled;


   procedure Test_Package (Package_Name : Entity_Name; Testing_Procedure : not null access procedure) is
      Full_Package_Name : constant Entity_Name := Entity_Prefix & Package_Name;
   begin
      -- We need assertions for this and the user has been warned
      if not Assertions_Enabled then
         raise Assertions_Required with "Cannot test " & To_String (Full_Package_Name) & ", assertions are not enabled";
      end if;

      -- Take note that a package test is running
      Global_Package_Test_Running := True;

      -- Run the testing procedure
      Global_Package_Prefix := Full_Package_Name & '.';
      Testing_Procedure.all;

      -- Report success (if any)
      Ada.Text_IO.Put_Line (To_String (Full_Package_Name) & " was successfully tested");

      -- Take note that the package test is over
      Global_Package_Test_Running := False;
   exception
      when Ada.Assertions.Assertion_Error =>
         raise;
      when others =>
         pragma Assert (False, "The testing procedure for " & To_String (Full_Package_Name) & " should not throw exceptions");
   end Test_Package;


   procedure Test_Package_Element (Element_Name : Entity_Name; Testing_Procedure : not null access procedure) is
      Full_Element_Name : constant Entity_Name := Global_Package_Prefix & Element_Name;
   begin
      -- Take note that an element test is running
      Global_Element_Test_Running := True;

      -- Run the testing procedure
      Global_Element_Prefix := Full_Element_Name & ' ';
      Testing_Procedure.all;

      -- Take note that the element test is over
      Global_Element_Test_Running := False;
   exception
      when Ada.Assertions.Assertion_Error =>
         raise;
      when others =>
         pragma Assert (False, "The testing procedure for " & To_String (Full_Element_Name) & " should not throw exceptions");
   end Test_Package_Element;


   procedure Test_Element_Property (Property : Boolean; Description : String) is
   begin
      pragma Assert (Property, To_String (Global_Element_Prefix) & Description);
   end Test_Element_Property;

end Cookbook.Test;
