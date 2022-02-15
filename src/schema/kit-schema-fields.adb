package body Kit.Schema.Fields is

   --------------------
   -- Base_Reference --
   --------------------

   function Base_Reference (Field : Root_Field_Type) return Boolean is
   begin
      return Field.Base_Reference;
   end Base_Reference;

   ------------------
   -- Create_Field --
   ------------------

   function Create_Field
     (Name      : in     String;
      With_Type : in     Kit.Schema.Types.Kit_Type)
      return Field_Type
   is
      Item : constant Field_Type := new Root_Field_Type;
   begin
      Item.Create (Name);
      Item.Field_Type := With_Type;
      Item.Size := Item.Field_Type.Size;
      return Item;
   end Create_Field;

   -------------
   -- Created --
   -------------

   function Created (Field : Root_Field_Type) return Boolean is
   begin
      return Field.Created;
   end Created;

   -------------
   -- Display --
   -------------

   function Display (Field : Root_Field_Type) return Boolean is
   begin
      return Field.Display;
   end Display;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type
     (Item : Root_Field_Type)
      return Kit.Schema.Types.Kit_Type
   is
   begin
      return Item.Field_Type;
   end Get_Field_Type;

   --------------
   -- Readable --
   --------------

   function Readable (Field : Root_Field_Type) return Boolean is
   begin
      return Field.Readable;
   end Readable;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Field : in out Root_Field_Type'Class)
   is
   begin
      Field.Has_Default := True;
   end Set_Default;

   -----------------------
   -- Set_Display_Field --
   -----------------------

   procedure Set_Display_Field
     (Field : in out Root_Field_Type'Class)
   is
   begin
      Field.Display := True;
   end Set_Display_Field;

   -----------------------
   -- Set_Field_Options --
   -----------------------

   procedure Set_Field_Options
     (Field          : in out Root_Field_Type'Class;
      Created        : Boolean := False;
      Readable       : Boolean := False;
      Writable       : Boolean := False;
      Base_Reference : Boolean := False)
   is
   begin
      Field.Created := Created;
      Field.Readable := Readable;
      Field.Writeable := Writable;
      Field.Base_Reference := Base_Reference;
   end Set_Field_Options;

   ----------
   -- Size --
   ----------

   function Size (Item : Root_Field_Type) return Natural is
   begin
      return Item.Size;
   end Size;

   ---------------
   -- Writeable --
   ---------------

   function Writeable (Field : Root_Field_Type) return Boolean is
   begin
      return Field.Writeable;
   end Writeable;

end Kit.Schema.Fields;
