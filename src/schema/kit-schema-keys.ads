private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Kit.Names;
with Kit.Schema.Fields;

package Kit.Schema.Keys is

   type Root_Key_Type is
     new Kit.Names.Root_Named_Object with private;

   function Size (Key : Root_Key_Type) return Natural;

   function Unique (Key : Root_Key_Type) return Boolean;
   function Base_Reference (Key : Root_Key_Type) return Boolean;
   function Base_Table_Name (Key : Root_Key_Type) return String
     with Pre => Base_Reference (Key);

   procedure Add_Field
     (Key   : in out Root_Key_Type'Class;
      Field : Kit.Schema.Fields.Field_Type);

   function Field_Count (Key : Root_Key_Type'Class) return Natural;
   function Field (Key    : Root_Key_Type'Class;
                   Index  : Positive)
                   return Kit.Schema.Fields.Field_Type;
   function Contains (Key : Root_Key_Type'Class;
                      Field_Name : String)
                      return Boolean;

   function Contains (Key : Root_Key_Type'Class;
                      Field : Kit.Schema.Fields.Field_Type)
                      return Boolean;

   type Key_Type is access all Root_Key_Type'Class;

   function Create_Key
     (Name           : in     String;
      Unique         : in     Boolean;
      Base_Reference : in String := "")
      return Key_Type;

private

   package Key_Field_Vector is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Kit.Schema.Fields.Field_Type,
        "="          => Kit.Schema.Fields."=");

   type Root_Key_Type is
     new Kit.Names.Root_Named_Object with
      record
         Unique         : Boolean;
         Base_Reference : Ada.Strings.Unbounded.Unbounded_String;
         Fields         : Key_Field_Vector.Vector;
      end record;

end Kit.Schema.Keys;
