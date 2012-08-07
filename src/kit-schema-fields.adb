package body Kit.Schema.Fields is


   ------------------
   -- Create_Field --
   ------------------

   procedure Create_Field (Item       : in out Field_Type;
                           Name       : in     String;
                           Field_Type : in     Kit.Schema.Types.Kit_Type'Class)
   is
   begin
      Item.Create (Name);
      Item.Field_Type :=
        new Kit.Schema.Types.Kit_Type'Class'(Field_Type);
      Item.Size := Item.Field_Type.Size;
   end Create_Field;

   -------------
   -- Created --
   -------------

   function Created (Field : Field_Type) return Boolean is
   begin
      return Field.Created;
   end Created;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type (Item : Field_Type)
                           return Kit.Schema.Types.Kit_Type'Class
   is
   begin
      return Item.Field_Type.all;
   end Get_Field_Type;

   --------------
   -- Readable --
   --------------

   function Readable (Field : Field_Type) return Boolean is
   begin
      return Field.Readable;
   end Readable;

   -----------------------
   -- Set_Field_Options --
   -----------------------

   procedure Set_Field_Options
     (Field    : in out Field_Type'Class;
      Created  : Boolean := False;
      Readable : Boolean := False;
      Writable : Boolean := False)
   is
   begin
      Field.Created := Created;
      Field.Readable := Readable;
      Field.Writeable := Writable;
   end Set_Field_Options;

   ----------
   -- Size --
   ----------

   function Size (Item : Field_Type) return Natural is
   begin
      return Item.Size;
   end Size;

   ---------------
   -- Writeable --
   ---------------

   function Writeable (Field : Field_Type) return Boolean is
   begin
      return Field.Writeable;
   end Writeable;

end Kit.Schema.Fields;
