package body Kit.Schema.Keys is

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Key   : in out Key_Type;
      Field : not null access Kit.Schema.Fields.Field_Type'Class)
   is
   begin
      Key.Fields.Append (Field_Access (Field));
   end Add_Field;

   --------------
   -- Contains --
   --------------

   function Contains
     (Key : Key_Type;
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
     (Key : Key_Type;
      Field : Kit.Schema.Fields.Field_Type'Class)
      return Boolean
   is
   begin
      return Key.Contains (Field.Ada_Name);
   end Contains;

   ----------------
   -- Create_Key --
   ----------------

   procedure Create_Key
     (Item       : in out Root_Key_Type'Class;
      Name       : in     String;
      Unique     : in     Boolean)
   is
   begin
      Kit.Names.Root_Named_Object (Item).Create (Name);
      Item.Unique := Unique;
   end Create_Key;

   -----------
   -- Field --
   -----------

   function Field
     (Key    : Key_Type;
      Index  : Positive)
      return Kit.Schema.Fields.Field_Type'Class
   is
   begin
      return Key.Fields.Element (Index).all;
   end Field;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Key : Key_Type) return Natural is
   begin
      return Key.Fields.Last_Index;
   end Field_Count;

   ----------
   -- Size --
   ----------

   function Size (Key : Key_Type) return Natural is
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
