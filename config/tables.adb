with Ada.Strings.Fixed;

with Marlowe.Btree_Handles;
with Marlowe.Key_Storage;

with {Database}.Kit_Enumeration;
with {Database}.Kit_Field;
with {Database}.Kit_Key;
with {Database}.Kit_Key_Field;
with {Database}.Kit_Literal;
with {Database}.Kit_Record;
with {Database}.Kit_Record_Base;
with {Database}.Kit_Type;

with {Database}.Marlowe_Keys;

package body {Database}.Tables is

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit_Type.Kit_Type_Type)
      return String;

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit_Type.Kit_Type_Type;
      Storage    : out System.Storage_Elements.Storage_Array);

   function Key_To_Storage
     (Table     : Database_Table'Class;
      Key_Name  : String;
      Key_Value : String;
      First     : Boolean)
      return System.Storage_Elements.Storage_Array;

   function Get_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Marlowe.Btree_Handles.Btree_Reference;

   ---------
   -- Get --
   ---------

   function Get
     (Table        : Database_Table'Class;
      Reference    : Record_Reference)
      return Database_Record
   is
      use System.Storage_Elements;
      Rec : Kit_Record.Kit_Record_Type :=
              Kit_Record.Get_By_Table_Index
                (Positive (Table.Index));
      Length  : constant Storage_Count :=
                  Storage_Count (Rec.Record_Length);
      Storage : Storage_Array (0 .. Length - 1);
   begin
      return Item : Database_Record do
         Item.Table := Table.Index;
         Item.Index := Marlowe.Database_Index (Reference);

         Marlowe.Btree_Handles.Get_Record
           (Marlowe_Keys.Handle, Table.Index, Item.Index,
            Storage'Address);
         Item.Value.Append (Storage);

         for Base of Kit_Record_Base.Select_By_Derived (Rec.Reference) loop
            declare
               Base_Record  : Kit_Record.Kit_Record_Type :=
                                Kit_Record.Get (Base.Base);
               Length       : constant Storage_Count :=
                                Storage_Count (Base_Record.Record_Length);
               Base_Storage : Storage_Array (0 .. Length - 1);
               Index        : Marlowe.Database_Index;
               Base_Offset  : constant Storage_Offset :=
                                Storage_Offset (Base.Offset);
            begin
               Marlowe.Key_Storage.From_Storage
                 (Index, Storage (Base_Offset .. Base_Offset + 7));
               Marlowe.Btree_Handles.Get_Record
                 (Marlowe_Keys.Handle,
                  Marlowe.Table_Index (Base_Record.Table_Index),
                  Index,
                  Base_Storage'Address);
               Item.Value.Append (Base_Storage);
            end;
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String)
      return Database_Record
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
        Get_Key_Reference (Table, Key_Name);
      First : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, True);
      Last  : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, False);
      Mark : constant Marlowe.Btree_Handles.Btree_Mark :=
        Marlowe.Btree_Handles.Search
          (Marlowe_Keys.Handle, Ref,
           First, Last,
           Marlowe.Closed, Marlowe.Closed,
           Marlowe.Forward);
   begin
      if Marlowe.Btree_Handles.Valid (Mark) then
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Marlowe.Btree_Handles.Get_Key
                           (Mark));
         begin
            return Get (Table,
                        Record_Reference (Index));
         end;
      else
         return Item : Database_Record do
            Item.Table := Table.Index;
            Item.Index := 0;
         end return;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (From_Record : Database_Record'Class;
      Field_Name  : String)
      return String
   is
      use System.Storage_Elements;
      Rec   : Kit_Record.Kit_Record_Type :=
                Kit_Record.Get_By_Table_Index
                  (Positive (From_Record.Table));
      pragma Assert (Rec.Has_Element);
      Field : Kit_Field.Kit_Field_Type :=
                Kit_Field.Get_By_Record_Field
                  (Rec.Reference, Field_Name);
      Field_Type  : Kit_Type_Reference;
      Store_Index : Positive := 1;
      Start       : Storage_Offset;
      Last        : Storage_Offset;
   begin
      if Field.Has_Element then
         Start := Storage_Offset (Field.Field_Offset);
         Last  := Start
           + Storage_Offset (Field.Field_Length)
           - 1;
         Field_Type := Field.Field_Type;
      else
         declare
            Bases : Kit_Record_Base.Selection :=
                      Kit_Record_Base.Select_By_Derived
                        (Rec.Reference);
            Found : Boolean := False;
         begin
            Store_Index := 2;
            for Base of Bases loop
               declare
                  Base_Field : Kit_Field.Kit_Field_Type :=
                                 Kit_Field.Get_By_Record_Field
                                   (Base.Base, Field_Name);
               begin
                  if Base_Field.Has_Element then
                     Start := Storage_Offset (Base_Field.Field_Offset);
                     Last  := Start
                       + Storage_Offset (Base_Field.Field_Length)
                       - 1;
                     Field_Type := Base_Field.Field_Type;
                     Found := True;
                     exit;
                  end if;
               end;
               Store_Index := Store_Index + 1;
            end loop;

            if not Found then
               raise Constraint_Error
                 with "no field '" & Field_Name & "' for record " & Rec.Name;
            end if;
         end;
      end if;

      return Storage_To_String
        (From_Record.Value.Element (Store_Index) (Start .. Last),
         Kit_Type.Get (Field_Type));
   end Get;

   -----------------------
   -- Get_Key_Reference --
   -----------------------

   function Get_Key_Reference
     (Table     : Database_Table'Class;
      Key_Name  : String)
      return Marlowe.Btree_Handles.Btree_Reference
   is
      Key_Full_Name : constant String := Table.Name & "_" & Key_Name;
   begin
      return Marlowe.Btree_Handles.Get_Reference
        (Marlowe_Keys.Handle, Key_Full_Name);
   end Get_Key_Reference;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Table_Name : String)
      return Database_Table
   is
      Rec : constant Kit_Record.Kit_Record_Type :=
              Kit_Record.Get_By_Name (Table_Name);
   begin
      if Rec.Has_Element then
         return (Index => Marlowe.Table_Index (Rec.Table_Index));
      else
         return (Index => 0);
      end if;
   end Get_Table;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Table : Database_Table'Class) return Boolean is
      use type Marlowe.Table_Index;
   begin
      return Table.Index /= 0;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Rec : Database_Record'Class) return Boolean is
      use type Marlowe.Database_Index;
   begin
      return Rec.Index /= 0;
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Get_Key_Reference (Table, Key_Name);
      Mark : Marlowe.Btree_Handles.Btree_Mark :=
               Marlowe.Btree_Handles.Search (Marlowe_Keys.Handle,
                                             Ref, Marlowe.Forward);
   begin
      while Marlowe.Btree_Handles.Valid (Mark) loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Marlowe.Btree_Handles.Get_Key
                           (Mark));
            Rec   : Database_Record := Get (Table,
                                            Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Marlowe.Btree_Handles.Next (Mark);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Get_Key_Reference (Table, Key_Name);
      First : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, True);
      Last : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table, Key_Name, Key_Value, False);
      Mark : Marlowe.Btree_Handles.Btree_Mark :=
               Marlowe.Btree_Handles.Search
                 (Marlowe_Keys.Handle,
                  Ref,
                  First, Last,
                  Marlowe.Closed, Marlowe.Closed,
                  Marlowe.Forward);
   begin
      while Marlowe.Btree_Handles.Valid (Mark) loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Marlowe.Btree_Handles.Get_Key
                           (Mark));
            Rec   : Database_Record := Get (Table,
                                            Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Marlowe.Btree_Handles.Next (Mark);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      First        : String;
      Last         : String;
      Process      : not null access procedure
        (Item : Database_Record'Class))
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Get_Key_Reference (Table, Key_Name);
      First_Storage : constant System.Storage_Elements.Storage_Array :=
                        Key_To_Storage (Table, Key_Name, First, True);
      Last_Storage : constant System.Storage_Elements.Storage_Array :=
                        Key_To_Storage (Table, Key_Name, Last, False);
      Mark : Marlowe.Btree_Handles.Btree_Mark :=
               Marlowe.Btree_Handles.Search
                 (Marlowe_Keys.Handle,
                  Ref,
                  First_Storage, Last_Storage,
                  Marlowe.Closed, Marlowe.Closed,
                  Marlowe.Forward);
   begin
      while Marlowe.Btree_Handles.Valid (Mark) loop
         declare
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index
                        (Marlowe.Btree_Handles.Get_Key
                           (Mark));
            Rec   : Database_Record := Get (Table,
                                            Record_Reference (Index));
         begin
            Process (Rec);
         end;
         Marlowe.Btree_Handles.Next (Mark);
      end loop;
   end Iterate;

   --------------------
   -- Key_To_Storage --
   --------------------

   function Key_To_Storage
     (Table     : Database_Table'Class;
      Key_Name  : String;
      Key_Value : String;
      First     : Boolean)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Rec : Kit_Record.Kit_Record_Type :=
              Kit_Record.Get_By_Table_Index
                (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Key : Kit_Key.Kit_Key_Type :=
              Kit_Key.Get_By_Record_Key
                (Rec.Reference, Key_Name);
      pragma Assert (Key.Has_Element);
      Result : Storage_Array (1 .. Storage_Count (Key.Length));
      Start  : Storage_Offset := 1;
      Key_Fields : Kit_Key_Field.Selection :=
                    Kit_Key_Field.Select_By_Kit_Key
                      (Key.Reference);
   begin
      if First then
         Result := (others => 0);
      else
         Result := (others => Storage_Element'Last);
      end if;
      for Key_Field of Key_Fields loop
         declare
            Field : Kit_Field.Kit_Field_Type :=
                      Kit_Field.Get (Key_Field.Kit_Field);
            Field_Type : Kit_Type.Kit_Type_Type :=
                           Kit_Type.Get (Field.Field_Type);
            Last       : constant Storage_Offset :=
                           Start + Storage_Count (Field.Field_Length) - 1;
         begin
            String_To_Storage
              (Value      => Key_Value,
               Value_Type => Field_Type,
               Storage    => Result (Start .. Last));
            Start := Last + 1;
         end;
      end loop;
      return Result;
   end Key_To_Storage;

   ----------
   -- Name --
   ----------

   function Name (Table : Database_Table'Class) return String is
      Rec : constant Kit_Record.Kit_Record_Type :=
              Kit_Record.Get_By_Table_Index
                (Natural (Table.Index));
   begin
      return Rec.Name;
   end Name;

   ---------------
   -- Reference --
   ---------------

   function Reference (Rec : Database_Record'Class) return Record_Reference is
   begin
      return Record_Reference (Rec.Index);
   end Reference;

   -----------------------
   -- Storage_To_String --
   -----------------------

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit_Type.Kit_Type_Type)
      return String
   is
   begin
      case Value_Type.Top_Record is
         when R_Kit_Integer =>
            declare
               X : Integer;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Integer'Image (X);
            end;
         when R_Kit_Float =>
            declare
               X : Float;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Ada.Strings.Fixed.Trim (Float'Image (X),
                                              Ada.Strings.Left);
            end;
         when R_Kit_Long_Float =>
            declare
               X : Long_Float;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Ada.Strings.Fixed.Trim (Long_Float'Image (X),
                                              Ada.Strings.Left);
            end;
         when R_Kit_String =>
            declare
               X : String (1 .. Value'Length);
               Last : Natural;
            begin
               Marlowe.Key_Storage.From_Storage (X, Last, Value);
               return X (1 .. Last);
            end;
         when R_Kit_Enumeration =>
            declare
               use type System.Storage_Elements.Storage_Element;
               X : Marlowe.Key_Storage.Unsigned_Integer := 0;
               Enum : Kit_Enumeration.Kit_Enumeration_Type :=
                        Kit_Enumeration.Get_By_Name
                          (Value_Type.Name);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);

               declare
                  Lit : constant Kit_Literal.Kit_Literal_Type :=
                          Kit_Literal.Get_By_Enum_Value
                            (Enum.Reference, Natural (X));
               begin
                  return Lit.Name;
               end;
            end;
         when R_Kit_Reference =>
            declare
               Index      : Marlowe.Database_Index;
            begin
               Marlowe.Key_Storage.From_Storage (Index, Value);
               return Marlowe.Database_Index'Image (Index);
            end;

         when others =>
            return Marlowe.Key_Storage.Image (Value);
      end case;
   end Storage_To_String;

   -----------------------
   -- String_To_Storage --
   -----------------------

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit_Type.Kit_Type_Type;
      Storage    : out System.Storage_Elements.Storage_Array)
   is
   begin
      case Value_Type.Top_Record is
         when R_Kit_Integer =>
            declare
               X : constant Integer := Integer'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Float =>
            declare
               X : constant Float := Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Long_Float =>
            declare
               X : constant Long_Float := Long_Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_String =>
            Marlowe.Key_Storage.To_Storage (Value, Storage);
         when R_Kit_Enumeration =>
            declare
               use type System.Storage_Elements.Storage_Element;
               Enum : Kit_Enumeration.Kit_Enumeration_Type :=
                        Kit_Enumeration.Get_By_Name
                          (Value_Type.Name);
               Lit : constant Kit_Literal.Kit_Literal_Type :=
                       Kit_Literal.Get_By_Enum_Name
                         (Enum.Reference, Value);
               X   : constant Marlowe.Key_Storage.Unsigned_Integer :=
                       Marlowe.Key_Storage.Unsigned_Integer (Lit.Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when R_Kit_Reference =>
            declare
               Index : constant Marlowe.Database_Index :=
                         Marlowe.Database_Index'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (Index, Storage);
            end;
         when others =>
            Storage := (others => 0);
      end case;
   end String_To_Storage;

   ---------------
   -- To_String --
   ---------------

   function To_String (Reference : Record_Reference) return String is
      Result : constant String := Record_Reference'Image (Reference);
   begin
      return Result (2 .. Result'Last);
   end To_String;

end {Database}.Tables;
