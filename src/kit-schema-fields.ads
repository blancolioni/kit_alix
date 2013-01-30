with Kit.Names;
with Kit.Schema.Types;

package Kit.Schema.Fields is

   type Field_Type is
     new Kit.Names.Root_Named_Object with private;

   function Size
     (Item : Field_Type)
      return Natural;

   procedure Set_Field_Options
     (Field          : in out Field_Type'Class;
      Created        : Boolean := False;
      Readable       : Boolean := False;
      Writable       : Boolean := False;
      Base_Reference : Boolean := False);

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

   function Base_Reference (Field : Field_Type) return Boolean;

private

   type Field_Type is
     new Kit.Names.Root_Named_Object with
      record
         Size           : Natural;
         Created        : Boolean  := True;
         Readable       : Boolean  := True;
         Writeable      : Boolean  := True;
         Base_Reference : Boolean := False;
         Field_Type     : access Kit.Schema.Types.Kit_Type'Class;
      end record;

end Kit.Schema.Fields;
