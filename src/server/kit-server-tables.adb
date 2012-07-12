with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Finalization;

with System.Storage_Elements;

with Kit.Db.Database;

with Kit.Db.Kit_Field;
with Kit.Db.Kit_Key;
with Kit.Db.Kit_Key_Field;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Kit_Type;

with Kit.Server.Storage;

with Marlowe.Key_Storage;

package body Kit.Server.Tables is

   use Kit.Databases;

   type Table_Type is
     new Root_Table_Interface with
      record
         Db     : Database_Access;
         Index  : Marlowe.Table_Index;
      end record;

   overriding function Name (Table : Table_Type) return String;

   overriding function Base_Count
     (Table    : Table_Type)
      return Natural;

   overriding function Base
     (Table    : Table_Type;
      Index    : Positive)
      return Root_Table_Interface'Class;

   overriding function Field_Count
     (Table    : Table_Type)
      return Natural;

   overriding function Field (Table    : Table_Type;
                              Index    : Positive)
                              return Root_Field_Interface'Class;

   overriding function Key_Count (Table    : Table_Type)
                                  return Natural;

   overriding function Key (Table    : Table_Type;
                            Index    : Positive)
                            return Root_Key_Interface'Class;

   type Table_Field_Type is
     new Root_Field_Interface with
      record
         Db         : Database_Access;
         Table      : Marlowe.Table_Index;
         Reference  : Kit.Db.Kit_Field_Reference;
      end record;

   overriding
   function Name (Field : Table_Field_Type) return String;

   overriding
   function Size (Field : Table_Field_Type) return Natural;

   overriding
   function Field_Type (Field : Table_Field_Type)
                        return String;

   type Table_Key_Type is
     new Root_Key_Interface with
      record
         Db        : Database_Access;
         Table     : Marlowe.Table_Index;
         Reference : Kit.Db.Kit_Key_Reference;
      end record;

   overriding function Name (Key : Table_Key_Type) return String;
   overriding function Unique (Key : Table_Key_Type) return Boolean;

   overriding function Field_Count (Key : Table_Key_Type)
                                    return Positive;

   overriding function Field
     (Key : Table_Key_Type;
      Index : Positive)
      return Kit.Databases.Root_Field_Interface'Class;

   package Database_Vectors is
     new Ada.Containers.Vectors (Positive, Database_Access);

   Active_Database_Vector : Database_Vectors.Vector;
   Current_Db : Database_Access;

   package Storage_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive,
        System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Kit_Db_Record (Key_Length : System.Storage_Elements.Storage_Count) is
     new Ada.Finalization.Limited_Controlled
     and Root_Database_Record with
      record
         Exists          : Boolean := False;
         Scanning        : Boolean := False;
         Using_Key       : Boolean := False;
         Using_Key_Value : Boolean := False;
         Table_Index     : Marlowe.Table_Index;
         Record_Index    : Marlowe.Database_Index;
         Mark            : Marlowe.Btree_Handles.Btree_Mark (Key_Length);
         Current_Record  : Storage_Vectors.Vector;
      end record;

   type Kit_Db_Record_Access is access all Kit_Db_Record'Class;

   overriding
   procedure Initialize (Item : in out Kit_Db_Record);

   overriding
   procedure Finalize (Item : in out Kit_Db_Record);

   overriding
   function Index (Item : Kit_Db_Record) return Marlowe.Database_Index;

   overriding
   function Has_Element (Item : in Kit_Db_Record) return Boolean;

   overriding
   procedure Next (Item : in out Kit_Db_Record);

   overriding
   function Get (Item       : Kit_Db_Record;
                 Field_Name : String)
                 return String;

   overriding
   procedure Set (Item       : in out Kit_Db_Record;
                  Field_Name : String;
                  Value      : String);

   overriding
   function Field_Count (Item : Kit_Db_Record) return Natural;

   overriding
   function Field_Name (Item : Kit_Db_Record;
                        Index : Positive)
                        return String;

   procedure Read (Item : in out Kit_Db_Record'Class);

   function Key_To_Storage
     (Table     : Marlowe.Table_Index;
      Key_Name  : String;
      Key_Value : String)
      return System.Storage_Elements.Storage_Array;

   function Get_Key_Reference
     (Tables    : Database_Type'Class;
      Index     : Marlowe.Table_Index;
      Key_Name  : String)
      return Marlowe.Btree_Handles.Btree_Reference;

   ---------------------
   -- Active_Database --
   ---------------------

   function Active_Database return Database_Access is
   begin
      return Current_Db;
   end Active_Database;

   ----------
   -- Base --
   ----------

   overriding function Base
     (Table    : Table_Type;
      Index    : Positive)
      return Root_Table_Interface'Class
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      Base  : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                Kit.Db.Kit_Record_Base.First_By_Derived
                  (Rec.Reference);
   begin
      for I in 1 .. Index - 1 loop
         exit when not Base.Has_Element;
         Base.Next;
      end loop;

      if not Base.Has_Element then
         raise Constraint_Error
           with "asked for illegal base number" & Positive'Image (Index)
           & " in table " & Rec.Name;
      end if;

      declare
         Base_Rec : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                      Kit.Db.Kit_Record.Get (Base.Base);
      begin
         return Table_Type'(Db => Table.Db,
                            Index => Marlowe.Table_Index
                              (Base_Rec.Table_Index));
      end;

   end Base;

   ----------------
   -- Base_Count --
   ----------------

   overriding function Base_Count
     (Table    : Table_Type)
      return Natural
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      Base  : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                Kit.Db.Kit_Record_Base.First_By_Derived
                  (Rec.Reference);
      Result : Natural := 0;
   begin
      while Base.Has_Element loop
         Result := Result + 1;
         Base.Next;
      end loop;
      return Result;
   end Base_Count;

   -----------
   -- Field --
   -----------

   overriding function Field (Table    : Table_Type;
                              Index    : Positive)
                              return Root_Field_Interface'Class
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         F : Kit.Db.Kit_Field.Kit_Field_Type :=
               Kit.Db.Kit_Field.First_By_Kit_Record (Rec.Reference);
      begin
         while F.Has_Element loop
            Result := Result + 1;
            if Result = Index then
               return Table_Field_Type'(Table.Db, Table.Index, F.Reference);
            end if;
            F.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               F        : Kit.Db.Kit_Field.Kit_Field_Type :=
                            Kit.Db.Kit_Field.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while F.Has_Element loop
                  Result := Result + 1;
                  if Result = Index then
                     return Table_Field_Type'(Table.Db, Table.Index,
                                              F.Reference);
                  end if;
                  F.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;

      raise Constraint_Error
        with "asked for illegal field number" & Positive'Image (Index)
        & " in table " & Rec.Name;

   end Field;

   -----------
   -- Field --
   -----------

   overriding function Field
     (Key : Table_Key_Type;
      Index : Positive)
      return Kit.Databases.Root_Field_Interface'Class
   is
      F       : Kit.Db.Kit_Key_Field.Kit_Key_Field_Type :=
                  Kit.Db.Kit_Key_Field.First_By_Kit_Key
                    (Key.Reference);
      Result : Natural := 0;
   begin
      while F.Has_Element loop
         Result := Result + 1;
         if Result = Index then
            return Table_Field_Type'(Db        => Key.Db,
                                     Table     => Key.Table,
                                     Reference => F.Kit_Field);
         end if;
         F.Next;
      end loop;

      declare
         Rec : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                 Kit.Db.Kit_Key.Get (Key.Reference);
      begin
         raise Constraint_Error
           with "asked for illegal field number" & Positive'Image (Index)
           & " in key " & Rec.Name;
      end;

   end Field;

   -----------------
   -- Field_Count --
   -----------------

   overriding
   function Field_Count (Item : Kit_Db_Record) return Natural is
      use System.Storage_Elements;
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Item.Table_Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         F : Kit.Db.Kit_Field.Kit_Field_Type :=
               Kit.Db.Kit_Field.First_By_Kit_Record (Rec.Reference);
      begin
         while F.Has_Element loop
            Result := Result + 1;
            F.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               F        : Kit.Db.Kit_Field.Kit_Field_Type :=
                            Kit.Db.Kit_Field.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while F.Has_Element loop
                  Result := Result + 1;
                  F.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;

      return Result;

   end Field_Count;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count
     (Table    : Table_Type)
      return Natural
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         F : Kit.Db.Kit_Field.Kit_Field_Type :=
               Kit.Db.Kit_Field.First_By_Kit_Record (Rec.Reference);
      begin
         while F.Has_Element loop
            Result := Result + 1;
            F.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               F        : Kit.Db.Kit_Field.Kit_Field_Type :=
                            Kit.Db.Kit_Field.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while F.Has_Element loop
                  Result := Result + 1;
                  F.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;

      return Result;

   end Field_Count;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count (Key : Table_Key_Type) return Positive is
      F       : Kit.Db.Kit_Key_Field.Kit_Key_Field_Type :=
                  Kit.Db.Kit_Key_Field.First_By_Kit_Key
                    (Key.Reference);
      Result : Natural := 0;
   begin
      while F.Has_Element loop
         Result := Result + 1;
         F.Next;
      end loop;
      return Result;
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   overriding
   function Field_Name (Item : Kit_Db_Record;
                        Index : Positive)
                        return String
   is
      use System.Storage_Elements;
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Item.Table_Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         F : Kit.Db.Kit_Field.Kit_Field_Type :=
               Kit.Db.Kit_Field.First_By_Kit_Record (Rec.Reference);
      begin
         while F.Has_Element loop
            Result := Result + 1;
            if Result = Index then
               return F.Name;
            end if;
            F.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               F        : Kit.Db.Kit_Field.Kit_Field_Type :=
                            Kit.Db.Kit_Field.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while F.Has_Element loop
                  Result := Result + 1;
                  if Result = Index then
                     return F.Name;
                  end if;
                  F.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;

      raise Constraint_Error with "index out of range";

   end Field_Name;

   ----------------
   -- Field_Type --
   ----------------

   overriding
   function Field_Type (Field : Table_Field_Type)
                        return String
   is
      use all type Kit.Db.Record_Type;
      Field_Rec      : constant Kit.Db.Kit_Field.Kit_Field_Type :=
        Kit.Db.Kit_Field.Get (Field.Reference);
      Field_Rec_Type : constant Kit.Db.Kit_Type.Kit_Type_Type :=
                         Kit.Db.Kit_Type.Get (Field_Rec.Field_Type);
   begin
      case Field_Rec_Type.Top_Record is
         when R_Kit_Integer =>
            return "integer";
         when R_Kit_Float =>
            return "float";
         when R_Kit_String =>
            return "string";
         when R_Kit_Enumeration =>
            return "integer";
         when R_Kit_Reference =>
            return "reference";
         when others =>
            return "unknown";
      end case;
   end Field_Type;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Item : in out Kit_Db_Record) is
      pragma Unreferenced (Item);
   begin
      null;
   end Finalize;

   ------------------
   -- First_By_Key --
   ------------------

   overriding
   function First_By_Key
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
      return Kit.Database_Record
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Get_Key_Reference (Tables, Table_Index, Key_Name);
      Mark : constant Marlowe.Btree_Handles.Btree_Mark :=
               Marlowe.Btree_Handles.Search (Handle, Ref, Marlowe.Forward);
   begin
      if Marlowe.Btree_Handles.Valid (Mark) then
         declare
            Result : constant Kit_Db_Record_Access :=
                       new Kit_Db_Record (Mark.Key_Length);
         begin
            Result.Table_Index  := Table_Index;
            Result.Record_Index :=
              Marlowe.Key_Storage.To_Database_Index
                (Marlowe.Btree_Handles.Get_Key (Mark));
            Result.Read;
            Result.Mark := Mark;
            Result.Exists := True;
            Result.Scanning := True;
            Result.Using_Key := True;
            Result.Using_Key_Value := False;
            return Database_Record (Result);
         end;
      else
         return null;
      end if;
   end First_By_Key;

   ------------------------
   -- First_By_Key_Value --
   ------------------------

   overriding
   function First_By_Key_Value
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
      return Database_Record
   is
      Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Get_Key_Reference (Tables, Table_Index, Key_Name);
      Key_Storage : constant System.Storage_Elements.Storage_Array :=
        Key_To_Storage (Table_Index, Key_Name, Key_Value);
      Mark : constant Marlowe.Btree_Handles.Btree_Mark :=
                      Marlowe.Btree_Handles.Search
                        (Handle, Ref,
                         Key_Storage, Key_Storage,
                         Marlowe.Closed, Marlowe.Closed,
                         Marlowe.Forward);
   begin
      if Marlowe.Btree_Handles.Valid (Mark) then
         declare
            Result : constant Kit_Db_Record_Access :=
                       new Kit_Db_Record (Mark.Key_Length);
         begin
            Result.Table_Index  := Table_Index;
            Result.Record_Index :=
              Marlowe.Key_Storage.To_Database_Index
                (Marlowe.Btree_Handles.Get_Key (Mark));
            Result.Read;
            Result.Mark := Mark;
            Result.Using_Key := True;
            Result.Using_Key_Value := True;

            return Database_Record (Result);
         end;
      else
         return null;
      end if;
   end First_By_Key_Value;

   ---------
   -- Get --
   ---------

   overriding
   function Get
     (Database     : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Record_Index : Marlowe.Database_Index)
      return Database_Record
   is
      pragma Unreferenced (Database);
      Result : constant Kit_Db_Record_Access :=
                 new Kit_Db_Record (0);
   begin
      Result.Table_Index  := Table_Index;
      Result.Record_Index := Record_Index;
      Result.Read;
      return Database_Record (Result);
   end Get;

   ---------
   -- Get --
   ---------

   overriding
   function Get (Item       : Kit_Db_Record;
                 Field_Name : String)
                 return String
   is
      use System.Storage_Elements;
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Item.Table_Index));
      pragma Assert (Rec.Has_Element);
      Field : Kit.Db.Kit_Field.Kit_Field_Type :=
                Kit.Db.Kit_Field.First_By_Record_Field
                  (Rec.Reference, Field_Name);
      Field_Type  : Kit.Db.Kit_Type_Reference;
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
            Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                     Kit.Db.Kit_Record_Base.First_By_Derived
                       (Rec.Reference);
         begin
            Store_Index := 2;
            while Base.Has_Element loop
               declare
                  Base_Field : Kit.Db.Kit_Field.Kit_Field_Type :=
                                 Kit.Db.Kit_Field.First_By_Record_Field
                                   (Base.Base, Field_Name);
               begin
                  if Base_Field.Has_Element then
                     Start := Storage_Offset (Base_Field.Field_Offset);
                     Last  := Start
                       + Storage_Offset (Base_Field.Field_Length)
                       - 1;
                     Field_Type := Base_Field.Field_Type;
                     exit;
                  end if;
               end;

               Base.Next;
               Store_Index := Store_Index + 1;
            end loop;

            if not Base.Has_Element then
               raise Constraint_Error
                 with "no field '" & Field_Name & "' for record " & Rec.Name;
            end if;
         end;
      end if;

      return Kit.Server.Storage.Storage_To_String
        (Item.Current_Record.Element (Store_Index) (Start .. Last),
         Kit.Db.Kit_Type.Get (Field_Type));
   end Get;

   -----------------------
   -- Get_Key_Reference --
   -----------------------

   function Get_Key_Reference
     (Tables    : Database_Type'Class;
      Index     : Marlowe.Table_Index;
      Key_Name  : String)
      return Marlowe.Btree_Handles.Btree_Reference
   is
      Table_Name : constant String := Tables.Get_Table_Name (Index);
      Key_Full_Name : constant String := Table_Name & "_" & Key_Name;
   begin
      return Marlowe.Btree_Handles.Get_Reference (Handle, Key_Full_Name);
   end Get_Key_Reference;

   --------------------
   -- Get_Table_Name --
   --------------------

   function Get_Table_Name (Db    : Database_Type;
                            Index : Marlowe.Table_Index)
                            return String
   is
      pragma Unreferenced (Db);
      Rec : constant Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Table_Index (Positive (Index));
   begin
      return Rec.Name;
   end Get_Table_Name;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element (Item : in Kit_Db_Record) return Boolean is
   begin
      return Item.Exists;
   end Has_Element;

   -----------
   -- Index --
   -----------

   overriding
   function Index (Item : Kit_Db_Record) return Marlowe.Database_Index is
   begin
      return Item.Record_Index;
   end Index;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item : in out Database_Type;
                         Name : String)
   is
   begin
      Item.Name := new String'(Name);
   end Initialise;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Item : in out Kit_Db_Record) is
      pragma Unreferenced (Item);
   begin
      null;
   end Initialize;

   ---------
   -- Key --
   ---------

   overriding function Key
     (Table : Table_Type;
      Index : Positive)
      return Root_Key_Interface'Class
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         K : Kit.Db.Kit_Key.Kit_Key_Type :=
               Kit.Db.Kit_Key.First_By_Kit_Record (Rec.Reference);
      begin
         while K.Has_Element loop
            Result := Result + 1;
            if Result = Index then
               return Table_Key_Type'(Table.Db, Table.Index, K.Reference);
            end if;

            K.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               K        : Kit.Db.Kit_Key.Kit_Key_Type :=
                            Kit.Db.Kit_Key.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while K.Has_Element loop
                  Result := Result + 1;
                  if Result = Index then
                     return Table_Key_Type'(Table.Db, Table.Index,
                                            K.Reference);
                  end if;
                  K.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;
      raise Constraint_Error
        with "asked for illegal key number" & Positive'Image (Index)
        & " in table " & Rec.Name;

   end Key;

   ---------------
   -- Key_Count --
   ---------------

   overriding function Key_Count
     (Table    : Table_Type)
      return Natural
   is
      Rec   : Kit.Db.Kit_Record.Kit_Record_Type :=
                Kit.Db.Kit_Record.First_By_Table_Index
                  (Positive (Table.Index));
      pragma Assert (Rec.Has_Element);
      Result : Natural := 0;
   begin

      declare
         K : Kit.Db.Kit_Key.Kit_Key_Type :=
               Kit.Db.Kit_Key.First_By_Kit_Record (Rec.Reference);
      begin
         while K.Has_Element loop
            Result := Result + 1;
            K.Next;
         end loop;
      end;

      declare
         Base : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                  Kit.Db.Kit_Record_Base.First_By_Derived
                    (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                            Kit.Db.Kit_Record.Get (Base.Base);
               pragma Assert (Base_Rec.Has_Element);
               K        : Kit.Db.Kit_Key.Kit_Key_Type :=
                            Kit.Db.Kit_Key.First_By_Kit_Record
                              (Base_Rec.Reference);
            begin
               while K.Has_Element loop
                  Result := Result + 1;
                  K.Next;
               end loop;
            end;

            Base.Next;

         end loop;

      end;

      return Result;

   end Key_Count;

   --------------------
   -- Key_To_Storage --
   --------------------

   function Key_To_Storage
     (Table     : Marlowe.Table_Index;
      Key_Name  : String;
      Key_Value : String)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Table_Index
                (Positive (Table));
      pragma Assert (Rec.Has_Element);
      Key : Kit.Db.Kit_Key.Kit_Key_Type :=
              Kit.Db.Kit_Key.First_By_Record_Key
                (Rec.Reference, Rec.Name & "_" & Key_Name);
      pragma Assert (Key.Has_Element);
      Result : Storage_Array (1 .. Storage_Count (Key.Length));
      Start  : Storage_Offset := 1;
      Key_Field : Kit.Db.Kit_Key_Field.Kit_Key_Field_Type :=
                    Kit.Db.Kit_Key_Field.First_By_Kit_Key
                      (Key.Reference);
   begin
      while Key_Field.Has_Element loop
         declare
            Field : Kit.Db.Kit_Field.Kit_Field_Type :=
                      Kit.Db.Kit_Field.Get (Key_Field.Kit_Field);
            Field_Type : Kit.Db.Kit_Type.Kit_Type_Type :=
                           Kit.Db.Kit_Type.Get (Field.Field_Type);
            Last       : constant Storage_Offset :=
                           Start + Storage_Count (Field.Field_Length) - 1;
         begin
            Kit.Server.Storage.String_To_Storage
              (Value      => Key_Value,
               Value_Type => Field_Type,
               Storage    => Result (Start .. Last));
            Start := Last + 1;
         end;
      end loop;
      return Result;
   end Key_To_Storage;

   ----------------------
   -- Last_Table_Index --
   ----------------------

   overriding
   function Last_Table_Index (Db         : Database_Type)
                              return Marlowe.Table_Index
   is
      pragma Unreferenced (Db);
      Result : Natural := 0;
      Rec    : Kit.Db.Kit_Record.Kit_Record_Type :=
                 Kit.Db.Kit_Record.First;
   begin
      while Rec.Has_Element loop
         Result := Result + 1;
         Rec.Next;
      end loop;
      return Marlowe.Table_Index (Result);
   end Last_Table_Index;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Db         : Database_Type) return String is
   begin
      return Db.Name.all;
   end Name;

   ----------
   -- Name --
   ----------

   overriding function Name (Table : Table_Type) return String is
   begin
      return Get_Table_Name (Database_Type (Table.Db.all), Table.Index);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Field : Table_Field_Type) return String is
      Field_Rec : constant Kit.Db.Kit_Field.Kit_Field_Type :=
                    Kit.Db.Kit_Field.Get (Field.Reference);
   begin
      return Field_Rec.Name;
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Key : Table_Key_Type) return String is
      Key_Rec : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                  Kit.Db.Kit_Key.Get (Key.Reference);
   begin
      return Key_Rec.Name;
   end Name;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (Item : in out Kit_Db_Record) is
      use type Marlowe.Database_Index;
      New_Index : Marlowe.Database_Index;
   begin
      if not Item.Scanning then
         raise Constraint_Error with
           "Not currently scanning this table";
      end if;

      if not Item.Using_Key then
         New_Index := Item.Record_Index + 1;
         while
           Marlowe.Btree_Handles.Valid_Index
             (Handle, Item.Table_Index, New_Index)
           and then
             Marlowe.Btree_Handles.Deleted_Record
               (Handle, Item.Table_Index, New_Index)
         loop
            New_Index := New_Index + 1;
         end loop;

         Item.Exists :=
           Marlowe.Btree_Handles.Valid_Index
             (Handle, Item.Table_Index, New_Index);

      else

         Marlowe.Btree_Handles.Next (Item.Mark);
         Item.Exists := Marlowe.Btree_Handles.Valid (Item.Mark);
         if Item.Exists then
            New_Index :=
              Marlowe.Key_Storage.To_Database_Index
                (Marlowe.Btree_Handles.Get_Key (Item.Mark));
         end if;
      end if;

      if Item.Exists then
         Item.Record_Index := New_Index;
         Item.Read;
      end if;

   end Next;

   -------------------
   -- Open_Database --
   -------------------

   procedure Open_Database (Path : String) is
   begin
      Kit.Db.Database.Open (Path);
      declare
         Db_Access : Database_Access;
      begin
         Db_Access := new Database_Type'
           (Name => new String'(Ada.Directories.Base_Name (Path)));
         Active_Database_Vector.Append (Db_Access);
         Current_Db := Db_Access;
      end;
   end Open_Database;

   ----------
   -- Read --
   ----------

   procedure Read (Item : in out Kit_Db_Record'Class) is
      use System.Storage_Elements;
      Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Table_Index
                (Positive (Item.Table_Index));
      Length  : constant Storage_Count :=
                  Storage_Count (Rec.Record_Length);
      Storage : Storage_Array (0 .. Length - 1);
   begin
      Item.Current_Record.Clear;

      Marlowe.Btree_Handles.Get_Record
        (Handle, Item.Table_Index, Item.Record_Index,
         Storage'Address);
      Item.Current_Record.Append (Storage);

      declare
         Base  : Kit.Db.Kit_Record_Base.Kit_Record_Base_Type :=
                   Kit.Db.Kit_Record_Base.First_By_Derived (Rec.Reference);
      begin
         while Base.Has_Element loop
            declare
               Base_Record  : Kit.Db.Kit_Record.Kit_Record_Type :=
                                Kit.Db.Kit_Record.Get (Base.Base);
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
                 (Handle,
                  Marlowe.Table_Index (Base_Record.Table_Index),
                  Index,
                  Base_Storage'Address);
               Item.Current_Record.Append (Base_Storage);
            end;
            Base.Next;
         end loop;
      end;
   end Read;

   ------------------------
   -- Scan_By_Key_Values --
   ------------------------

   overriding
   function Scan_By_Key_Values
     (Tables         : Database_Type;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Database_Record
   is
      pragma Unreferenced (High_Key_Value);
   begin
      return Tables.First_By_Key_Value (Table_Index, Key_Name, Low_Key_Value);
   end Scan_By_Key_Values;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (Item       : in out Kit_Db_Record;
                  Field_Name : String;
                  Value      : String)
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Field_Name);
      pragma Unreferenced (Value);
   begin
      null;
   end Set;

   ----------
   -- Size --
   ----------

   overriding
   function Size (Field : Table_Field_Type) return Natural is
      Field_Rec : constant Kit.Db.Kit_Field.Kit_Field_Type :=
                    Kit.Db.Kit_Field.Get (Field.Reference);
   begin
      return Field_Rec.Field_Length;
   end Size;

   -----------
   -- Table --
   -----------

   overriding
   function Table (Db         : Database_Type;
                   Index      : Marlowe.Table_Index)
                   return Kit.Databases.Root_Table_Interface'Class
   is
   begin
      return Result : Table_Type do
         Result.Db := Db'Unchecked_Access;
         Result.Index := Index;
      end return;
   end Table;

   --------------------
   -- To_Table_Index --
   --------------------

   overriding
   function To_Table_Index
     (Db         : Database_Type;
      Table_Name : String)
      return Marlowe.Table_Index
   is
      Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
              Kit.Db.Kit_Record.First_By_Name (Table_Name);
   begin
      if Rec.Has_Element then
         return Marlowe.Table_Index (Rec.Table_Index);
      else
         raise Constraint_Error
           with Table_Name & ": no such table in database " & Name (Db);
      end if;
   end To_Table_Index;

   ------------
   -- Unique --
   ------------

   overriding
   function Unique (Key : Table_Key_Type) return Boolean is
      Key_Rec : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                  Kit.Db.Kit_Key.Get (Key.Reference);
   begin
      return Key_Rec.Is_Unique;
   end Unique;

end Kit.Server.Tables;
