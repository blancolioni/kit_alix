with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Marlowe.Data_Stores;
with Marlowe.Key_Storage;

with Kit.Db.Marlowe_Keys;

with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;

with Kit.SQL.Database.Types;

package body Kit.SQL.Database is

   subtype Real_Field_Reference is
     Field_Reference range 1 .. Field_Reference'Last;

   subtype Real_Key_Reference is
     Key_Reference range 1 .. Key_Reference'Last;

   subtype Real_Table_Reference is
     Table_Reference range 1 .. Table_Reference'Last;

   package Storage_Array_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Cached_Record is
      record
         Reference : Record_Reference;
         Data      : Storage_Array_Vectors.Vector;
      end record;

   function Get_Record
     (Reference : Record_Reference)
      return Cached_Record;

   function Read_Record
     (Reference : Record_Reference)
      return Cached_Record;

   function Get_Field_Storage
     (Rec : Cached_Record;
      Field : Positive)
      return System.Storage_Elements.Storage_Array;

   package Cached_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cached_Record);

   function Record_Key
     (Rec   : Record_Reference)
      return String
   is (Rec.Table'Image & Rec.Index'Image);

   package Cached_Record_Maps is
     new WL.String_Maps (Cached_Record_Lists.Cursor,
                         Cached_Record_Lists."=");

   Record_List : Cached_Record_Lists.List;
   Record_Map  : Cached_Record_Maps.Map;

   package Data_Type_Holders is
     new Ada.Containers.Indefinite_Holders
       (Data_Type'Class);

   type Field_Record is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Table       : Table_Reference;
         Base_Table  : Table_Reference;
         Schema_Ref  : Kit.Db.Kit_Field_Reference;
         Base_Index  : Natural;
         Table_Index : Positive;
         Field_Type  : Data_Type_Holders.Holder;
         Offset      : System.Storage_Elements.Storage_Offset;
         Length      : System.Storage_Elements.Storage_Count;
         Default_Key : Real_Key_Reference;
         Display     : Boolean;
      end record;

   package Field_Record_Vectors is
     new Ada.Containers.Vectors (Real_Field_Reference, Field_Record);

   package Field_Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Real_Field_Reference);

   type Key_Record is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Table       : Table_Reference;
         Base_Table  : Table_Reference;
         Fields      : Field_Reference_Vectors.Vector;
         Length      : System.Storage_Elements.Storage_Count;
         Unique      : Boolean;
         Descending  : Boolean;
         Marlowe_Ref : Marlowe.Data_Stores.Key_Reference;
      end record;

   package Key_Record_Vectors is
     new Ada.Containers.Vectors (Real_Key_Reference, Key_Record);

   package Key_Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Real_Key_Reference);

   type Table_Base_Record is
      record
         Base   : Table_Reference;
         Index  : Positive;
         Length : System.Storage_Elements.Storage_Count;
      end record;

   package Table_Base_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Base_Record);

   type Table_Record is
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
         Index         : Marlowe.Table_Index;
         Schema_Ref    : Kit.Db.Kit_Record_Reference;
         Length        : System.Storage_Elements.Storage_Count;
         Bases         : Table_Base_Vectors.Vector;
         Fields        : Field_Reference_Vectors.Vector;
         Keys          : Key_Reference_Vectors.Vector;
         Default_Key   : Real_Key_Reference;
         Display_Field : Field_Reference := No_Field;
      end record;

   function Get_Table
     (Reference : Real_Table_Reference)
      return Table_Record;

   procedure Create_Table
     (From      : Kit.Db.Kit_Record.Kit_Record_Type);

   package Table_Record_Vectors is
     new Ada.Containers.Vectors (Real_Table_Reference, Table_Record);

   package Table_Reference_Maps is
     new WL.String_Maps (Table_Reference);

   Field_Vector : Field_Record_Vectors.Vector;
   Key_Vector   : Key_Record_Vectors.Vector;
   Table_Vector : Table_Record_Vectors.Vector;
   Table_Map    : Table_Reference_Maps.Map;

   --------------------
   -- Contains_Field --
   --------------------

   function Contains_Field
     (Key   : Key_Reference;
      Field : Field_Reference)
      return Boolean
   is
      Fields : constant Field_Reference_Array := Get_Key_Fields (Key);
   begin
      for F of Fields loop
         if F = Field then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Field;

   ------------------
   -- Create_Table --
   ------------------

   procedure Create_Table
     (From      : Kit.Db.Kit_Record.Kit_Record_Type)
   is
      Reference : constant Kit.Db.Kit_Record_Reference :=
                    From.Get_Kit_Record_Reference;

      Table     : Table_Record;
      Table_Ref : constant Table_Reference :=
                    Table_Vector.Last_Index + 1;
      Handle    : constant Marlowe.Data_Stores.Data_Store :=
                    Kit.Db.Marlowe_Keys.Handle;
      Next_Index : Natural := 0;

      procedure Add_Base
        (Rec    : Kit.Db.Kit_Record.Kit_Record_Type;
         Ref    : Table_Reference;
         Index  : Natural);

      function Find_Field
        (Ref : Kit.Db.Kit_Field_Reference)
         return Natural;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base
        (Rec    : Kit.Db.Kit_Record.Kit_Record_Type;
         Ref    : Table_Reference;
         Index  : Natural)
      is
      begin
         if Index > 0 then
            Table.Bases.Append
              (Table_Base_Record'
                 (Base   => Table_Map.Element (Rec.Name),
                  Index  => Index,
                  Length =>
                    System.Storage_Elements.Storage_Count
                      (Rec.Record_Length)));
         end if;

         for Field of
           Kit.Db.Kit_Field.Select_By_Kit_Record
             (Rec.Get_Kit_Record_Reference)
         loop

            Next_Index := Next_Index + 1;

            declare
               subtype Storage is System.Storage_Elements.Storage_Offset;
               Field_Rec : constant Field_Record :=
                             Field_Record'
                               (Name        => +Field.Name,
                                Table       => Table_Ref,
                                Base_Table  => Ref,
                                Base_Index  => Index,
                                Table_Index => Next_Index,
                                Schema_Ref  =>
                                  Field.Get_Kit_Field_Reference,
                                Field_Type  =>
                                  Data_Type_Holders.To_Holder
                                    (Types.To_Data_Type (Field.Field_Type)),
                                Offset      => Storage (Field.Field_Offset),
                                Length      => Storage (Field.Field_Length),
                                Default_Key => 1,
                                Display     => Field.Display);
            begin
               Field_Vector.Append (Field_Rec);
               Table.Fields.Append (Field_Vector.Last_Index);
               if Field_Rec.Display then
                  Table.Display_Field := Field_Vector.Last_Index;
               end if;
            end;
         end loop;

         for Key of
           Kit.Db.Kit_Key.Select_By_Kit_Record
             (Rec.Get_Kit_Record_Reference)
         loop
            declare
               subtype Storage is System.Storage_Elements.Storage_Offset;
               use type Kit.Db.Kit_Field_Reference;
               Key_Rec : Key_Record :=
                           Key_Record'
                             (Name        => +Key.Name,
                              Table       => Table_Ref,
                              Base_Table  => Ref,
                              Fields      => <>,
                              Length      => Storage (Key.Length),
                              Unique      => Key.Is_Unique,
                              Descending  => False,
                              Marlowe_Ref =>
                                Handle.Get_Reference
                                  (From.Name & "_" & Key.Name));
               Cancel : Boolean := False;

            begin
               for Key_Field of
                 Kit.Db.Kit_Key_Field.Select_By_Kit_Key
                   (Key.Get_Kit_Key_Reference)
               loop
                  if Key_Field.Kit_Field = Kit.Db.Null_Kit_Field_Reference then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: key "
                        & (-Key_Rec.Name)
                        & " in table "
                        & (-Table.Name)
                        & " has a null field");
                     Cancel := True;

                  else

                     declare
                        Field_Index : constant Natural :=
                                        Find_Field (Key_Field.Kit_Field);
                     begin
                        if Field_Index = 0 then
                           raise Constraint_Error with
                             "cannot find field "
                             & Kit.Db.Kit_Field.Get (Key_Field.Kit_Field).Name
                             & " of key "
                             & (-Key_Rec.Name)
                             & " in table "
                             & (-Table.Name);
                        else
                           Key_Rec.Fields.Append
                             (Table.Fields.Element (Field_Index));
                        end if;
                     end;
                  end if;
               end loop;

               if not Cancel then
                  Key_Vector.Append (Key_Rec);
                  if Table.Keys.Is_Empty then
                     Table.Default_Key := Key_Vector.Last_Index;
                  end if;
                  Table.Keys.Append (Key_Vector.Last_Index);
               end if;
            end;
         end loop;

      end Add_Base;

      ----------------
      -- Find_Field --
      ----------------

      function Find_Field
        (Ref : Kit.Db.Kit_Field_Reference)
         return Natural
      is
      begin
         for I in 1 .. Table.Fields.Last_Index loop
            declare
               use type Kit.Db.Kit_Field_Reference;
               Field : constant Field_Reference := Table.Fields.Element (I);
            begin
               if Field_Vector.Element (Field).Schema_Ref = Ref then
                  return I;
               end if;
            end;
         end loop;
         return 0;
      end Find_Field;

   begin

      Table.Name := +From.Name;
      Table.Index := Marlowe.Table_Index (From.Table_Index);
      Table.Schema_Ref := Reference;
      Table.Length :=
        System.Storage_Elements.Storage_Offset
          (From.Record_Length);

      Table_Vector.Append (Table);

      for Base of Kit.Db.Kit_Record_Base.Select_By_Derived (Reference) loop
         declare
            Base_Rec : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                         Kit.Db.Kit_Record.Get (Base.Base);
         begin
            Add_Base
              (Rec    => Base_Rec,
               Ref    => Get_Table (Base_Rec.Name),
               Index  => Base.Offset);
         end;
      end loop;

      Add_Base
        (Rec    => From,
         Ref    => Table_Ref,
         Index  => 0);

      for Field of Table.Fields loop
         Field_Vector (Field).Default_Key := Table.Default_Key;
      end loop;

      for Key of Table.Keys loop
         declare
            Key_Rec : constant Key_Record := Key_Vector.Element (Key);
            Field   : constant Field_Reference :=
                        Key_Rec.Fields.First_Element;
         begin
            if Field_Vector.Element (Field).Default_Key
              = Table.Default_Key
            then
               Field_Vector.Reference (Field).Default_Key := Key;
            end if;
         end;

      end loop;

      Table_Vector.Replace_Element (Table_Ref, Table);
      Table_Map.Insert (From.Name, Table_Ref);
   end Create_Table;

   ---------------------------
   -- Get_Default_Field_Key --
   ---------------------------

   function Get_Default_Field_Key
     (Field : Field_Reference)
      return Key_Reference
   is
   begin
      return Field_Vector.Element (Field).Default_Key;
   end Get_Default_Field_Key;

   ---------------------
   -- Get_Default_Key --
   ---------------------

   function Get_Default_Key
     (Table : Table_Reference)
      return Key_Reference
   is
   begin
      return Table_Vector.Element (Table).Default_Key;
   end Get_Default_Key;

   -----------------------
   -- Get_Display_Field --
   -----------------------

   function Get_Display_Field
     (Table : Table_Reference)
      return Field_Reference
   is
   begin
      return Table_Vector.Element (Table).Display_Field;
   end Get_Display_Field;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Table : Table_Reference;
      Index : Positive)
      return Field_Reference
   is
   begin
      return Get_Table (Table).Fields.Element (Index);
   end Get_Field;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Table : Table_Reference;
      Name  : String)
      return Field_Reference
   is
      T : constant Table_Record := Get_Table (Table);
   begin
      for Reference of T.Fields loop
         declare
            Field : constant Field_Record :=
                      Field_Vector.Element (Reference);
         begin
            if Same (Field.Name, Name) then
               return Reference;
            end if;
         end;
      end loop;
      return No_Field;
   end Get_Field;

   ---------------------
   -- Get_Field_Count --
   ---------------------

   function Get_Field_Count (Table : Table_Reference) return Natural is
   begin
      return Table_Vector.Element (Table).Fields.Last_Index;
   end Get_Field_Count;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name
     (Reference : Field_Reference)
      return String
   is
   begin
      return -(Field_Vector.Element (Reference).Name);
   end Get_Field_Name;

   -----------------------
   -- Get_Field_Storage --
   -----------------------

   function Get_Field_Storage
     (Rec   : Cached_Record;
      Field : Positive)
      return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Offset;
      Table_Rec : constant Table_Record :=
                    Get_Table (Rec.Reference.Table);
      Field_Ref : constant Field_Reference :=
                    Table_Rec.Fields.Element (Field);
      Field_Rec : constant Field_Record :=
                    Field_Vector.Element (Field_Ref);
      Data : constant System.Storage_Elements.Storage_Array :=
               (if Field_Rec.Base_Index = 0
                then Rec.Data.Last_Element
                else Rec.Data.Element (Field_Rec.Base_Index));
   begin
      return Data
        (Field_Rec.Offset + 1 .. Field_Rec.Offset + Field_Rec.Length);
   end Get_Field_Storage;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type
     (Field     : Field_Reference)
      return Data_Type'Class
   is
   begin
      return Field_Vector.Element (Field).Field_Type.Element;
   end Get_Field_Type;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   function Get_Field_Value
     (Reference : Record_Reference;
      Index     : Positive)
      return System.Storage_Elements.Storage_Array
   is
      Rec : constant Cached_Record := Get_Record (Reference);
      Value : constant System.Storage_Elements.Storage_Array :=
                Get_Field_Storage (Rec, Index);
   begin
      return Value;
   end Get_Field_Value;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   function Get_Field_Value
     (Reference : Record_Reference;
      Name      : String)
      return System.Storage_Elements.Storage_Array
   is
   begin
      return Get_Field_Value
        (Reference,
         Get_Field (Reference.Table, Name));
   end Get_Field_Value;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   function Get_Field_Value
     (Reference : Record_Reference;
      Field     : Field_Reference)
      return System.Storage_Elements.Storage_Array
   is
      Rec : constant Field_Record :=
              Field_Vector.Element (Field);
   begin
      return Get_Field_Value
        (Reference, Rec.Table_Index);
   end Get_Field_Value;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Table : Table_Reference;
      Name  : String)
      return Key_Reference
   is
      T : constant Table_Record := Get_Table (Table);
   begin
      for Reference of T.Keys loop
         declare
            Key : constant Key_Record :=
                      Key_Vector.Element (Reference);
         begin
            if Same (Key.Name, Name) then
               return Reference;
            end if;
         end;
      end loop;
      return No_Key;
   end Get_Key;

   --------------------
   -- Get_Key_Fields --
   --------------------

   function Get_Key_Fields
     (Key : Key_Reference)
      return Field_Reference_Array
   is
      Rec : Key_Record renames Key_Vector (Key);
   begin
      return Fields : Field_Reference_Array (1 .. Rec.Fields.Last_Index) do
         for I in Fields'Range loop
            Fields (I) := Rec.Fields.Element (I);
         end loop;
      end return;
   end Get_Key_Fields;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length
     (Key : Key_Reference)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return Key_Vector (Key).Length;
   end Get_Key_Length;

   -----------------
   -- Get_Maximum --
   -----------------

   function Get_Maximum
     (Key : Key_Reference)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Length : constant Storage_Count := Get_Key_Length (Key) - 8;
   begin
      return Storage : constant Storage_Array (1 .. Length) :=
        (others => Storage_Element'Last);
   end Get_Maximum;

   -----------------
   -- Get_Minimum --
   -----------------

   function Get_Minimum
     (Key : Key_Reference)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Length : constant Storage_Count := Get_Key_Length (Key) - 8;
   begin
      return Storage : constant Storage_Array (1 .. Length) :=
        (others => 0);
   end Get_Minimum;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Table : Table_Reference) return String is
   begin
      return -(Table_Vector.Element (Table).Name);
   end Get_Name;

   ----------------
   -- Get_Record --
   ----------------

   function Get_Record
     (Reference : Record_Reference)
      return Cached_Record
   is
      Cache_Key : constant String := Record_Key (Reference);
   begin
      if not Record_Map.Contains (Cache_Key) then
         Record_List.Append (Read_Record (Reference));
         Record_Map.Insert (Cache_Key, Record_List.Last);
      end if;
      return Cached_Record_Lists.Element
        (Record_Map.Element (Cache_Key));
   end Get_Record;

   --------------------------
   -- Get_Record_Reference --
   --------------------------

   function Get_Record_Reference
     (Table : Table_Reference;
      Index : Marlowe.Database_Index)
      return Record_Reference
   is
   begin
      return (Table, Index);
   end Get_Record_Reference;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table (Name : String) return Table_Reference is
   begin
      if Table_Vector.Is_Empty then
         Ada.Text_IO.Put_Line ("loading schema ...");
         for Rec of Kit.Db.Kit_Record.Scan_By_Table_Index loop
            Create_Table (Rec);
         end loop;
         Ada.Text_IO.Put_Line
           (Table_Vector.Last_Index'Image & " tables;"
            & Key_Vector.Last_Index'Image & " keys;"
            & Field_Vector.Last_Index'Image & " fields.");
      end if;
      return Table_Map.Element (Name);
   end Get_Table;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Reference : Real_Table_Reference)
      return Table_Record
   is
   begin
      if Table_Vector.Is_Empty then
         for Rec of Kit.Db.Kit_Record.Scan_By_Table_Index loop
            Create_Table (Rec);
         end loop;
      end if;
      return Table_Vector.Element (Reference);
   end Get_Table;

   -------------------------
   -- Get_Table_Reference --
   -------------------------

   function Get_Table_Reference
     (From : Kit.Db.Kit_Record_Reference)
      return Table_Reference
   is
      use type Kit.Db.Kit_Record_Reference;
   begin
      for Ref in 1 .. Table_Vector.Last_Index loop
         if Table_Vector.Element (Ref).Schema_Ref = From then
            return Ref;
         end if;
      end loop;
      raise Constraint_Error with
        "no such table: " & Kit.Db.Kit_Record.Get (From).Name;
   end Get_Table_Reference;

   ------------------
   -- Is_Field_Key --
   ------------------

   function Is_Field_Key
     (Key   : Key_Reference;
      Field : Field_Reference)
      return Boolean
   is
      Fields : constant Field_Reference_Array := Get_Key_Fields (Key);
   begin
      return Fields'Length = 1 and then Fields (Fields'First) = Field;
   end Is_Field_Key;

   -----------------
   -- Read_Record --
   -----------------

   function Read_Record
     (Reference : Record_Reference)
      return Cached_Record
   is
      Handle    : constant Marlowe.Data_Stores.Data_Store :=
                    Kit.Db.Marlowe_Keys.Handle;
      Table_Rec : constant Table_Record :=
                    Get_Table (Reference.Table);
      Table_Data : System.Storage_Elements.Storage_Array
        (1 .. Table_Rec.Length);
   begin
      return Result : Cached_Record do
         declare
            Empty : System.Storage_Elements.Storage_Array (1 .. 0);
         begin
            for Base of Table_Rec.Bases loop
               Result.Data.Append (Empty);
            end loop;
         end;

         Handle.Get_Record
           (Index    => Table_Rec.Index,
            Db_Index => Reference.Index,
            Data     => Table_Data'Address);
         for Base of Table_Rec.Bases loop
            declare
               use System.Storage_Elements;
               Base_Rec    : constant Table_Record :=
                               Get_Table (Base.Base);
               Base_Data   : System.Storage_Elements.Storage_Array
                 (1 .. Base_Rec.Length);
               Base_Index  : Marlowe.Database_Index;
               Index_First : constant Storage_Offset :=
                               Storage_Offset
                                 ((Base.Index - 1) * 8 + 1 + 4);
               Index_Last  : constant Storage_Offset :=
                               Index_First + 7;
            begin
               Marlowe.Key_Storage.From_Storage
                 (Value   => Base_Index,
                  Storage => Table_Data (Index_First .. Index_Last));
               Handle.Get_Record
                 (Index    => Base_Rec.Index,
                  Db_Index => Base_Index,
                  Data     => Base_Data'Address);
               Result.Data.Replace_Element
                 (Base.Index, Base_Data);
            end;
         end loop;
         Result.Data.Append (Table_Data);
         Result.Reference := Reference;
      end return;
   end Read_Record;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Table       : Table_Reference;
      Key         : Key_Reference;
      Min_Value   : System.Storage_Elements.Storage_Array;
      Max_Value   : System.Storage_Elements.Storage_Array;
      Min_Closed  : Boolean;
      Max_Closed  : Boolean;
      Callback    : not null access
        procedure (Index : Marlowe.Database_Index))
   is
      pragma Unreferenced (Table);
      use System.Storage_Elements;
      Handle : constant Marlowe.Data_Stores.Data_Store :=
                 Kit.Db.Marlowe_Keys.Handle;
      First_Index : constant Storage_Array :=
                      Marlowe.Key_Storage.To_Storage_Array
                        (Marlowe.Database_Index'First);
      Last_Index  : constant Storage_Array :=
                      Marlowe.Key_Storage.To_Storage_Array
                        (Marlowe.Database_Index'Last);
      First_Key   : constant Storage_Array :=
                    Min_Value & First_Index;
      Last_Key  : constant Storage_Array :=
                      Max_Value & Last_Index;
      Key_Rec     : constant Key_Record :=
                      Key_Vector.Element (Key);
      Mark        : Marlowe.Data_Stores.Data_Store_Cursor :=
                      Handle.Search (Key_Rec.Marlowe_Ref,
                                     First_Key, Last_Key,
                                     (if Min_Closed
                                      then Marlowe.Closed
                                      else Marlowe.Open),
                                     (if Max_Closed
                                      then Marlowe.Closed
                                      else Marlowe.Open),
                                     Marlowe.Forward);
   begin
      while Mark.Valid loop
         declare
            Key   : constant System.Storage_Elements.Storage_Array :=
                      Mark.Get_Key;
            Index : constant Marlowe.Database_Index :=
                      Marlowe.Key_Storage.To_Database_Index (Key);
         begin
            Callback (Index);
            Mark.Next;
         end;
      end loop;
   end Scan;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage
     (With_Type : Data_Type;
      Value     : Float)
      return System.Storage_Elements.Storage_Array
   is
      pragma Unreferenced (Value);
      use System.Storage_Elements;
   begin
      return Result : constant Storage_Array (1 .. With_Type.Size) :=
        (others => 0);
   end To_Storage;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (With_Type : Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String
   is
      pragma Unreferenced (With_Type);
      Result    : String (1 .. Data'Length * 3 - 1);
      Hex_Digit : constant String (1 .. 16) :=
                    "0123456789ABCDEF";
   begin
      for I in Data'Range loop
         declare
            use type System.Storage_Elements.Storage_Offset;
            Index : constant Positive :=
                      Natural (I - Data'First) * 3 + 1;
            D     : constant Natural := Natural (Data (I));
         begin
            Result (Index) := Hex_Digit (Natural (D / 16) + 1);
            Result (Index + 1) := Hex_Digit (Natural (D mod 16) + 1);
            if I < Data'Last then
               Result (Index + 2) := '-';
            end if;
         end;
      end loop;
      return Result;
   end To_String;

end Kit.SQL.Database;
