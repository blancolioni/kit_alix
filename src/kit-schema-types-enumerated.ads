with Ada.Containers.Indefinite_Vectors;

package Kit.Schema.Types.Enumerated is

   type Enumerated_Type is
     new Kit.Schema.Types.Kit_Type with private;

   procedure Add_Literal (To      : in out Enumerated_Type;
                          Literal : String);

   type Record_Type_Enumeration is
     new Enumerated_Type with private;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Enumerated_Type is
     new Kit.Schema.Types.Kit_Type with
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

   overriding
   function Default_Value (Item : Enumerated_Type)
                           return Aquarius.Drys.Expression'Class;

   function Create_Database_Record
     (For_Type : Enumerated_Type)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Storage_Array_Transfer
     (Item          : Enumerated_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class;

   type Record_Type_Enumeration is
     new Enumerated_Type with null record;

   function Size (Item : Record_Type_Enumeration) return Natural;

end Kit.Schema.Types.Enumerated;
