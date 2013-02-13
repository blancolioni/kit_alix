private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Schema.Fields;

package Kit.Schema.Keys is

   type Root_Key_Type is
     abstract new Kit.Names.Root_Named_Object with private;

   procedure Create_Key
     (Item           : in out Root_Key_Type'Class;
      Name           : in     String;
      Unique         : in     Boolean;
      Base_Reference : in Boolean := False);

   function Size (Key : Root_Key_Type) return Natural is abstract;

   function Unique (Key : Root_Key_Type) return Boolean;
   function Base_Reference (Key : Root_Key_Type) return Boolean;

   type Key_Type is new Root_Key_Type with private;

   overriding function Size (Key : Key_Type) return Natural;
   procedure Add_Field
     (Key   : in out Key_Type;
      Field : not null access Kit.Schema.Fields.Field_Type'Class);

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

   type Root_Key_Type is
     abstract new Kit.Names.Root_Named_Object
   with
      record
         Unique         : Boolean;
         Base_Reference : Boolean;
      end record;

   type Field_Access is access all Kit.Schema.Fields.Field_Type'Class;

   package Key_Field_Vector is
      new Ada.Containers.Vectors (Positive, Field_Access);

   type Key_Type is
     new Root_Key_Type with
      record
         Fields     : Key_Field_Vector.Vector;
      end record;

end Kit.Schema.Keys;
