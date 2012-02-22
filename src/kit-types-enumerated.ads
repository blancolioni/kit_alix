with Ada.Containers.Indefinite_Vectors;

package Kit.Types.Enumerated is

   type Enumerated_Type is
     new Kit.Types.Kit_Type with private;

   procedure Add_Literal (To      : in out Enumerated_Type;
                          Literal : String);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Enumerated_Type is
     new Kit.Types.Kit_Type with
      record
         Literals : String_Vectors.Vector;
      end record;

   overriding
   function Return_Subtype
     (Item : Enumerated_Type)
      return String;

   overriding
   function To_Declaration
     (Item : Enumerated_Type)
      return Aquarius.Drys.Declaration'Class;

   overriding
   function To_Storage_Array
     (Item        : Enumerated_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

end Kit.Types.Enumerated;
