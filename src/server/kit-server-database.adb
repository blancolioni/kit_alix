with Ada.Text_IO;

with Marlowe.Key_Storage;

with Kit.Db.Kit_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Type;

with Kit.Server.Storage;

package body Kit.Server.Database is

   ---------
   -- Get --
   ---------

   procedure Get
     (Item     : in out Database_Record'Class;
      Table    : in     Marlowe.Table_Index;
      Index    : in     Marlowe.Database_Index)
   is
      use System.Storage_Elements;
      Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Table_Index
                (Positive (Table));
      Length  : constant Storage_Count :=
                  Storage_Count (Rec.Record_Length);
      Storage : Storage_Array (0 .. Length - 1);
   begin
      Item.Table := Table;
      Item.Index := Index;
      Item.Value.Clear;
      Marlowe.Btree_Handles.Get_Record (Handle, Table, Index,
                                        Storage'Address);
      Item.Value.Append (Storage);

      declare
         Base  : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                   Kit.Db.Kit_Record_Base.First_By_Derived (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Record : Kit.Db.Kit_Record.Kit_Record_Type :=
                               Kit.Db.Kit_Record.Get (Base.Base);
               Length      : constant Storage_Count :=
                               Storage_Count (Base_Record.Record_Length);
               Base_Storage : Storage_Array (0 .. Length - 1);
               Index        : Marlowe.Database_Index;
               Base_Offset  : constant Storage_Offset :=
                                Storage_Offset (Base.Offset);
            begin
               Marlowe.Key_Storage.From_Storage
                 (Index, Storage (Base_Offset .. Base_Offset + 7));
               Marlowe.Btree_Handles.Get_Record
                 (Handle,
                  Marlowe.Table_Index (Base_Record.Table_Index),
                  Index,
                  Base_Storage'Address);
               Item.Value.Append (Base_Storage);
            end;
            Base.Next;
         end loop;
      end;

   end Get;

   ------------------
   -- Record_Index --
   ------------------

   function Record_Index
     (Item : Database_Record'Class)
      return Marlowe.Database_Index
   is
   begin
      return Item.Index;
   end Record_Index;

   ------------
   -- Report --
   ------------

   procedure Report (Item     : Database_Record'Class;
                     Width    : Positive := 12;
                     Across   : Boolean := False;
                     Headings : Boolean := False)
   is

      procedure Put_With_Width (Text : String);

      procedure Report_Record
        (Store_Index : Positive;
         Rec         : Kit.Db.Kit_Record.Kit_Record_Type);

      --------------------
      -- Put_With_Width --
      --------------------

      procedure Put_With_Width (Text : String) is
         Image : String (1 .. Width - 1) := (others => ' ');
      begin
         if Text'Length < Width then
            Image (1 .. Text'Length) := Text;
         else
            Image := Text (Text'First .. Text'First + Width - 2);
         end if;
         Ada.Text_IO.Put (Image & ' ');
      end Put_With_Width;

      -------------------
      -- Report_Record --
      -------------------

      procedure Report_Record
        (Store_Index : Positive;
         Rec         : Kit.Db.Kit_Record.Kit_Record_Type)
      is
         use System.Storage_Elements;
         Field : Kit.Db.Kit_Field.Kit_Field_Type :=
                   Kit.Db.Kit_Field.First_By_Kit_Record (Rec.Reference);
         Value        : constant Storage_Array :=
                          Item.Value.Element (Store_Index);
      begin
         while Field.Has_Element loop
            if Across and then Headings then
               Put_With_Width (Field.Name);
            else
               declare
                  Field_Type   : constant Kit.Db.Kit_Type.Kit_Type_Type :=
                                   Kit.Db.Kit_Type.Get (Field.Field_Type);
                  Offset       : constant Storage_Offset :=
                                   Storage_Offset (Field.Field_Offset);
                  Length       : constant Storage_Count :=
                                   Storage_Count (Field.Field_Length);
                  Last         : constant Storage_Offset :=
                                   Offset + Length - 1;
                  Image        : constant String :=
                                   Kit.Server.Storage.Storage_To_String
                                     (Value (Offset .. Last), Field_Type);
               begin
                  if Across then
                     Put_With_Width (Image);
                  else
                     Ada.Text_IO.Put (Field.Name);
                     Ada.Text_IO.Set_Col (20);
                     Ada.Text_IO.Put (Field_Type.Name);
                     Ada.Text_IO.Set_Col (40);
                     Ada.Text_IO.Put_Line (Image);
                  end if;
               end;
            end if;
            Field.Next;
         end loop;

      end Report_Record;

      Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Table_Index
                (Positive (Item.Table));
      Base  : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                Kit.Db.Kit_Record_Base.First_By_Derived (Rec.Reference);

      Base_Index : Positive := 2;

   begin

      Base_Index := 2;

      while Base.Has_Element loop
         declare
            Base_Record : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
         begin
            Report_Record (Base_Index, Base_Record);
         end;
         Base.Next;
         Base_Index := Base_Index + 1;
      end loop;

      Report_Record (1, Rec);

   end Report;

   ------------
   -- Report --
   ------------

   procedure Report (Table : Marlowe.Table_Index) is
      use type Marlowe.Database_Index;
      Index : Marlowe.Database_Index := 1;
      Item  : Database_Record;
      First : Boolean := True;
   begin
      while Marlowe.Btree_Handles.Valid_Index (Handle, Table, Index) loop
         Item.Get (Table, Index);
         if First then
            Item.Report (Across   => True,
                         Headings => True);
            First := False;
            Ada.Text_IO.New_Line;
         end if;
         Item.Report (Across => True);
         Ada.Text_IO.New_Line;
         Index := Index + 1;
      end loop;
   end Report;

   -----------------
   -- Table_Index --
   -----------------

   function Table_Index
     (Item : Database_Record'Class)
      return Marlowe.Table_Index
   is
   begin
      return Item.Table;
   end Table_Index;

end Kit.Server.Database;
