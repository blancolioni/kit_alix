private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Schema.Fields;

package Kit.Schema.Keys is

   type Key_Type is
     new Kit.Names.Root_Named_Object with private;

   procedure Create_Key
     (Item       : in out Key_Type;
      Name       : in     String;
      Unique     : in     Boolean);

   procedure Add_Field
     (Key   : in out Key_Type;
      Field : not null access Kit.Schema.Fields.Field_Type'Class);

   function Size (Key : Key_Type) return Natural;
   function Unique (Key : Key_Type) return Boolean;

   function Field_Count (Key : Key_Type) return Natural;
   function Field (Key    : Key_Type;
                   Index  : Positive)
                   return Kit.Schema.Fields.Field_Type'Class;
   function Contains (Key : Key_Type;
                      Field_Name : String)
                      return Boolean;

   function Contains (Key : Key_Type;
                      Field : Kit.Schema.Fields.Field_Type'Class)
                      return Boolean;

private

   type Field_Access is access all Kit.Schema.Fields.Field_Type'Class;

   package Key_Field_Vector is
      new Ada.Containers.Vectors (Positive, Field_Access);

   type Key_Type is
     new Kit.Names.Root_Named_Object with
      record
         Unique     : Boolean;
         Fields     : Key_Field_Vector.Vector;
      end record;

end Kit.Schema.Keys;
