private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Schema.Types;

package Kit.Schema.Fields is

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with private;

   function Size
     (Item : Root_Field_Type)
      return Natural;

   procedure Set_Field_Options
     (Field    : in out Root_Field_Type'Class;
      Created  : Boolean := False;
      Readable : Boolean := False;
      Writable : Boolean := False);

   type Field_Type is new Root_Field_Type with private;

   procedure Create_Field
     (Item       : in out Field_Type;
      Name       : in     String;
      Field_Type : in     Kit.Schema.Types.Kit_Type'Class);

   function Get_Field_Type (Item : Field_Type)
                           return Kit.Schema.Types.Kit_Type'Class;

   function Created (Field : Field_Type) return Boolean;
   --  return True if Field should be part of the Create subprograms

   function Readable (Field : Field_Type) return Boolean;
   --  return true if Field can be read

   function Writeable (Field : Field_Type) return Boolean;
   --  return true if Field can be written

   type Compound_Field_Type is new Root_Field_Type with private;

   procedure Create_Field (Item       : in out Compound_Field_Type;
                           Name       : in     String);

   procedure Add_Field (Item  : in out Compound_Field_Type;
                        Field : not null access Field_Type'Class);

   function Contains (Item  : Compound_Field_Type;
                      Field : Field_Type'Class)
                      return Boolean;

   function Field_Count (Item : Compound_Field_Type) return Natural;
   function Field (Item : Compound_Field_Type;
                   Index : Positive)
                   return Field_Type'Class;
   function Ada_Name (Item  : Compound_Field_Type;
                      Index : Positive)
                      return String;

private

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with
      record
         Size       : Natural;
         Created    : Boolean  := True;
         Readable   : Boolean  := True;
         Writeable  : Boolean  := True;
      end record;

   type Field_Type is new Root_Field_Type with
      record
         Field_Type : access Kit.Schema.Types.Kit_Type'Class;
      end record;

   type Field_Access is access all Root_Field_Type'Class;

   function Equal_Fields (Left, Right : Field_Access) return Boolean;

   package Field_Vectors is
     new Ada.Containers.Vectors (Positive, Field_Access, Equal_Fields);

   type Compound_Field_Type is new Root_Field_Type with
      record
         Fields : Field_Vectors.Vector;
      end record;

end Kit.Schema.Fields;
