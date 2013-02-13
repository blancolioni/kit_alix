private with Ada.Containers.Indefinite_Vectors;
with Aquarius.Drys.Declarations;

package Aquarius.Drys.Projects is

   type Project is tagged private;

   procedure Add_Package (To_Project : in out Project;
                          Item       : in     Declarations.Package_Type'Class);

   procedure Write_Project (Item   : in Project;
                            File   : in out Writer_Interface'Class);

private

   package Package_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive,
                                            Declarations.Package_Type'Class,
                                            Declarations."=");

   type Project is tagged
      record
         Packages : Package_Vectors.Vector;
      end record;

end Aquarius.Drys.Projects;
