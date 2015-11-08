with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with Marlowe.Key_Storage;

with Kit.Mutex;

with {database}.Marlowe_Keys;

package body {database}.Kit_Deferred_Keys is

   type Deferred_Key_Change
     (Key_Length : System.Storage_Elements.Storage_Count) is
      record
         Reference : Marlowe.Database_Index;
         Key       : Marlowe.Data_Stores.Key_Reference;
         Old_Value : System.Storage_Elements.Storage_Array (1 .. Key_Length);
         New_Value : System.Storage_Elements.Storage_Array (1 .. Key_Length);
      end record;

   package List_Of_Deferred_Key_Changes is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Deferred_Key_Change);

   type Deferred_Key_Table_Entry is
      record
         Mutex   : Kit.Mutex.Mutex_Type;
         Changes : List_Of_Deferred_Key_Changes.List;
      end record;

   Deferred_Key_Table : array (Marlowe.Table_Index range 1 .. {table_count})
     of Deferred_Key_Table_Entry;

   ----------------
   -- Check_Keys --
   ----------------

   procedure Check_Keys
     (Table : Marlowe.Table_Index)
   is
      use type System.Storage_Elements.Storage_Array;
      Table_Entry : Deferred_Key_Table_Entry renames
                      Deferred_Key_Table (Table);
   begin
      Table_Entry.Mutex.Lock;

      if not Table_Entry.Changes.Is_Empty then
         Ada.Text_IO.Put_Line
           ("Table"
            & Marlowe.Table_Index'Image (Table)
            & ": updating"
            & Ada.Containers.Count_Type'Image
                (Table_Entry.Changes.Length)
            & " deferred keys");
      end if;

      for Change of Table_Entry.Changes loop
         Marlowe_Keys.Handle.Delete
           (Reference => Change.Key,
            Key       =>
              Change.Old_Value
            & Marlowe.Key_Storage.To_Storage_Array (Change.Reference));
         Marlowe_Keys.Handle.Insert
           (Reference => Change.Key,
            Key       =>
              Change.New_Value
            & Marlowe.Key_Storage.To_Storage_Array (Change.Reference));
      end loop;

      Table_Entry.Changes.Clear;

      Table_Entry.Mutex.Unlock;
   end Check_Keys;

   -------------------------
   -- Close_Deferred_Keys --
   -------------------------

   procedure Close_Deferred_Keys is
   begin
      for Table in Deferred_Key_Table'Range loop
         Check_Keys (Table);
      end loop;
   end Close_Deferred_Keys;

   -----------------
   -- Key_Changed --
   -----------------

   procedure Key_Changed
     (Table     : Marlowe.Table_Index;
      Key       : Marlowe.Data_Stores.Key_Reference;
      Reference : Marlowe.Database_Index;
      Old_Value : System.Storage_Elements.Storage_Array;
      New_Value : System.Storage_Elements.Storage_Array)
   is
      use List_Of_Deferred_Key_Changes;
      Table_Entry : Deferred_Key_Table_Entry renames
                      Deferred_Key_Table (Table);
      Current     : Cursor := No_Element;
      New_Change  : Deferred_Key_Change :=
                      (Old_Value'Length, Reference, Key, Old_Value, New_Value);
   begin
      Table_Entry.Mutex.Lock;
      for Position in Table_Entry.Changes.Iterate loop
         declare
            use Marlowe, Marlowe.Data_Stores;
            Change : Deferred_Key_Change renames Element (Position);
         begin
            if Change.Reference = Reference
              and then Change.Key = Key
            then
               Current := Position;
               New_Change.Old_Value := Change.Old_Value;
               exit;
            end if;
         end;
      end loop;

      if Has_Element (Current) then
         Table_Entry.Changes.Replace_Element (Current, New_Change);
      else
         Table_Entry.Changes.Insert (Table_Entry.Changes.First, New_Change);
      end if;

      Table_Entry.Mutex.Unlock;
   end Key_Changed;

end {database}.Kit_Deferred_Keys;
