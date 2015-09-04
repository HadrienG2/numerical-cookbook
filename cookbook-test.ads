with Ada.Strings;
with Ada.Strings.Bounded;

generic
package Cookbook.Test is

   -- The boolean flag centrally enables or disables package self-testing by the function below
   Test_On_Elaboration : Boolean := True;
   procedure Elaboration_Self_Test (Testing_Procedure : not null access procedure);

   -- Check if assertions are currently enabled
   function Assertions_Enabled return Boolean;

   -- We shall use this string type for package and package element element names
   Max_Entity_Name_Length : constant := 80;
   package Entity_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Entity_Name_Length);
   subtype Entity_Name is Entity_Names.Bounded_String;
   function To_Entity_Name (Source : String;
                            Drop : Ada.Strings.Truncation := Ada.Strings.Error) return Entity_Name renames Entity_Names.To_Bounded_String;
   function To_String (Source : Entity_Name) return String renames Entity_Names.To_String;

   -- Input need not add the "Cookbook." prefix to package names, nor the package name to element names, as it shall be prepended automatically
   -- However, the length of the string must be right.
   Entity_Prefix : constant Entity_Name := To_Entity_Name ("Cookbook.");
   function Valid_Package_Name (Prospective_Name : Entity_Name) return Boolean;
   function Valid_Element_Name (Prospective_Name : Entity_Name) return Boolean;

   -- This exception will be thrown if one attempts to test a package without assertions on
   Assertions_Required : exception;

   -- The test runner is a state machine internally, this allows us to check its status
   function Package_Test_Running return Boolean;
   function Element_Test_Running return Boolean;

   -- Run tests for a complete package
   procedure Test_Package (Package_Name : Entity_Name; Testing_Procedure : not null access procedure)
     with Pre => (not Package_Test_Running and then
                    Assertions_Enabled and then
                      Valid_Package_Name (Package_Name));

   -- Run tests for a single package element (package testing procedures should call this)
   procedure Test_Package_Element (Element_Name : Entity_Name; Testing_Procedure : not null access procedure)
     with Pre => (Package_Test_Running and then
                    not Element_Test_Running and then
                      Valid_Element_Name (Element_Name));

   -- Check a single assertion about an element, with a human-readable description such as "'s result should be greater than zero"
   procedure Test_Element_Property (Property : Boolean; Description : String)
     with Pre => Element_Test_Running;

private

   Global_Package_Test_Running : Boolean := False;
   function Package_Test_Running return Boolean is (Global_Package_Test_Running);

   Global_Element_Test_Running : Boolean := False;
   function Element_Test_Running return Boolean is (Global_Element_Test_Running);

   function Valid_Package_Name (Prospective_Name : Entity_Name) return Boolean is
     (Entity_Names.Length (Prospective_Name) < Max_Entity_Name_Length - Entity_Names.Length (Entity_Prefix));

   Global_Package_Prefix : Entity_Name;
   function Valid_Element_Name (Prospective_Name : Entity_Name) return Boolean is
     (Entity_Names.Length (Prospective_Name) < Max_Entity_Name_Length - Entity_Names.Length (Global_Package_Prefix));

end Cookbook.Test;
