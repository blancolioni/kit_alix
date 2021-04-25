with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

package Kit.Schema.Types.Enumerated is

   type Enumerated_Type is
     new Kit.Schema.Types.Root_Kit_Type with private;

   procedure Set_Defining_Package
     (Of_Type : in out Enumerated_Type'Class;
      Name    : String);

   procedure Add_Literal (To      : in out Enumerated_Type;
                          Literal : String);

   type Record_Type_Enumeration is
     new Enumerated_Type with private;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Enumerated_Type is
     new Kit.Schema.Types.Root_Kit_Type with
      record
         Literals : String_Vectors.Vector;
         Pkg_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   function Return_Subtype
     (Item : Enumerated_Type)
      return String;

   overriding
   function To_Declaration
     (Item : Enumerated_Type)
      return Syn.Declaration'Class;

   overriding
   function To_Storage_Array
     (Item        : Enumerated_Type;
      Object      : Syn.Expression'Class)
      return Syn.Expression'Class;

   overriding function Has_Custom_Type
     (Item : Enumerated_Type)
      return Boolean
   is (Item.User_Defined);

   overriding function Default_Value
     (Item : Enumerated_Type)
      return Syn.Expression'Class;

   overriding function Default_Argument_Value
     (Item : Enumerated_Type)
      return Syn.Expression'Class;

   overriding function Haskell_Type_Name
     (Item : Enumerated_Type)
      return String;

   overriding function Create_Database_Record
     (For_Type : Enumerated_Type)
      return Syn.Statement'Class;

   overriding
   function Storage_Array_Transfer
     (Item          : Enumerated_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   overriding function Argument_Handle_Subtype
     (Item : Enumerated_Type)
      return String;

   type Record_Type_Enumeration is
     new Enumerated_Type with null record;

   overriding function Size (Item : Record_Type_Enumeration) return Natural;

   overriding function Has_Custom_Type
     (Item : Record_Type_Enumeration)
      return Boolean
   is (True);

end Kit.Schema.Types.Enumerated;
