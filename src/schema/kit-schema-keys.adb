package body Kit.Schema.Keys is

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Key   : in out Root_Key_Type'Class;
      Field : Kit.Schema.Fields.Field_Type)
   is
   begin
      Key.Fields.Append (Field);
   end Add_Field;

   --------------------
   -- Base_Reference --
   --------------------

   function Base_Reference (Key : Root_Key_Type) return Boolean is
   begin
      return Key.Base_Reference;
   end Base_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Key : Root_Key_Type'Class;
      Field_Name : String)
      return Boolean
   is
   begin
      for F of Key.Fields loop
         if F.Ada_Name = Field_Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Key : Root_Key_Type'Class;
      Field : Kit.Schema.Fields.Field_Type)
      return Boolean
   is
   begin
      return Key.Contains (Field.Ada_Name);
   end Contains;

   ----------------
   -- Create_Key --
   ----------------

   function Create_Key
     (Name           : in     String;
      Unique         : in     Boolean;
      Base_Reference : in Boolean := False)
      return Key_Type
   is
      Item           : Root_Key_Type;
   begin
      Kit.Names.Root_Named_Object (Item).Create (Name);
      Item.Unique := Unique;
      Item.Base_Reference := Base_Reference;
      return new Root_Key_Type'(Item);
   end Create_Key;

   -----------
   -- Field --
   -----------

   function Field
     (Key    : Root_Key_Type'Class;
      Index  : Positive)
      return Kit.Schema.Fields.Field_Type
   is
   begin
      return Key.Fields.Element (Index);
   end Field;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Key : Root_Key_Type'Class) return Natural is
   begin
      return Key.Fields.Last_Index;
   end Field_Count;

   ----------
   -- Size --
   ----------

   function Size
     (Key : Root_Key_Type)
      return Natural
   is
      Result : Natural := 0;
   begin
      for F of Key.Fields loop
         Result := Result + F.Size;
      end loop;
      return Result + 8;    --  add 8 for database index
   end Size;

   ------------
   -- Unique --
   ------------

   function Unique (Key : Root_Key_Type) return Boolean is
   begin
      return Key.Unique;
   end Unique;

end Kit.Schema.Keys;
