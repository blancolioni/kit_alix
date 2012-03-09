with Ada.Text_IO;

with Marlowe.Key_Storage;

with Kit.Db.Kit_Enumeration;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Literal;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Type;

package body Kit.Server.Database is

   function Storage_Image
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type)
     return String;

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
            declare
               Field_Name   : constant String := Field.Name;
               Field_Type   : constant Kit.Db.Kit_Type.Kit_Type_Type :=
                                Kit.Db.Kit_Type.Get (Field.Field_Type);
               Offset       : constant Storage_Offset :=
                                Storage_Offset (Field.Field_Offset);
               Length       : constant Storage_Count :=
                                Storage_Count (Field.Field_Length);
               Last         : constant Storage_Offset := Offset + Length - 1;
               Image        : constant String :=
                                Storage_Image
                                  (Value (Offset .. Last), Field_Type);
            begin
               if Across then
                  if Headings then
                     Put_With_Width (Field_Name);
                  else
                     Put_With_Width (Image);
                  end if;
               else
                  Ada.Text_IO.Put (Field.Name);
                  Ada.Text_IO.Set_Col (20);
                  Ada.Text_IO.Put (Field_Type.Name);
                  Ada.Text_IO.Set_Col (40);
                  Ada.Text_IO.Put_Line (Image);
               end if;
            end;

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

   -------------------
   -- Storage_Image --
   -------------------

   function Storage_Image
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type)
      return String
   is
   begin
      case Value_Type.Top_Record is
         when Kit.Db.R_Kit_Integer =>
            declare
               X : Integer;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Integer'Image (X);
            end;
         when Kit.Db.R_Kit_Float =>
            declare
               X : Float;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Float'Image (X);
            end;
         when Kit.Db.R_Kit_Long_Float =>
            declare
               X : Long_Float;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Long_Float'Image (X);
            end;
         when Kit.Db.R_Kit_String =>
            declare
               X : String (1 .. Value'Length);
               Last : Natural;
            begin
               Marlowe.Key_Storage.From_Storage (X, Last, Value);
               return X (1 .. Last);
            end;
         when Kit.Db.R_Kit_Enumeration =>
            declare
               use type System.Storage_Elements.Storage_Element;
               X : Natural := 0;
               Enum : Kit.Db.Kit_Enumeration.Kit_Enumeration_Type :=
                        Kit.Db.Kit_Enumeration.First_By_Name
                          (Value_Type.Name);
            begin
               if Value'Length = 1
                 and then Value (Value'First) < 128
               then
                  X := Natural (Value (Value'First));
               else
                  Marlowe.Key_Storage.From_Storage (X, Value);
               end if;

               declare
                  Lit : constant Kit.Db.Kit_Literal.Kit_Literal_Type :=
                          Kit.Db.Kit_Literal.First_By_Enum_Value
                            (Enum.Reference, X);
               begin
                  return Lit.Name;
               end;
            end;

         when others =>
            return Marlowe.Key_Storage.Image (Value);
      end case;
   end Storage_Image;

end Kit.Server.Database;
