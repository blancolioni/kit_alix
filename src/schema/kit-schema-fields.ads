with Kit.Names;
with Kit.Schema.Types;

package Kit.Schema.Fields is

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with private;

   function Size
     (Item : Root_Field_Type)
      return Natural;

   procedure Set_Field_Options
     (Field          : in out Root_Field_Type'Class;
      Created        : Boolean := False;
      Readable       : Boolean := False;
      Writable       : Boolean := False;
      Base_Reference : Boolean := False);

   procedure Set_Display_Field
     (Field : in out Root_Field_Type'Class);

   procedure Set_Default
     (Field : in out Root_Field_Type'Class);

   function Get_Field_Type (Item : Root_Field_Type)
                           return Kit.Schema.Types.Kit_Type;

   function Created (Field : Root_Field_Type) return Boolean;
   --  return True if Field should be part of the Create subprograms

   function Readable (Field : Root_Field_Type) return Boolean;
   --  return true if Field can be read

   function Writeable (Field : Root_Field_Type) return Boolean;
   --  return true if Field can be written

   function Display (Field : Root_Field_Type) return Boolean;
   --  return true if Field should be used to represent its record
   --  instead of the record's database index.

   function Has_Default_Value (Field : Root_Field_Type) return Boolean;
   --  return true if the Field has a default value

   function Base_Reference (Field : Root_Field_Type) return Boolean;

   type Field_Type is access all Root_Field_Type'Class;

   function Create_Field
     (Name      : in     String;
      With_Type : in     Kit.Schema.Types.Kit_Type)
      return Field_Type;

private

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with
      record
         Size           : Natural;
         Created        : Boolean := True;
         Readable       : Boolean := True;
         Writeable      : Boolean := True;
         Has_Default    : Boolean := False;
         Display        : Boolean := False;
         Base_Reference : Boolean := False;
         Field_Type     : Kit.Schema.Types.Kit_Type;
      end record;

   function Has_Default_Value (Field : Root_Field_Type) return Boolean
   is (Field.Has_Default);

end Kit.Schema.Fields;
