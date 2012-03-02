with Ada.Strings.Fixed;

with Aquarius.Drys.Expressions;

package body Kit.Tables is

   Current_Table : Marlowe.Table_Index := 1;

   Recursively_Add_Bases : constant Boolean := False;

   function Get_Magic_Number
     (From_Text : String)
      return Natural;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Position : Key_Cursor)
                      return String
   is
      use Field_Vectors;
      Item : constant Table_Field_Access := Element (Cursor (Position));
   begin
      if Item.Is_Compound then
         return Item.Compound_Field.Ada_Name;
      else
         return Item.Field.Ada_Name;
      end if;
   end Ada_Name;

   --------------
   -- Add_Base --
   --------------

   procedure Add_Base
     (Table     : in out Table_Type;
      Item      : in     Table_Type'Class)
   is
   begin
      if Item.Has_String_Type then
         Table.Has_String_Type := True;
      end if;

      if Item.Has_Key_Field then
         Table.Has_Key_Field := True;
      end if;

      Table.Bases.Append (new Table_Type'Class'(Item));
      if Recursively_Add_Bases then
         declare
            It : Base_Cursor := Item.First_Base;
         begin
            while Has_Element (It) loop
               if not Table.Contains_Base (Element (It).Name) then
                  Table.Add_Base (Element (It));
               end if;
               Next (It);
            end loop;
         end;
      end if;
   end Add_Base;

   ----------------------------
   -- Add_Compound_Key_Field --
   ----------------------------

   procedure Add_Compound_Key_Field
     (Table        : in out Table_Type;
      Compound_Key : in out Kit.Fields.Compound_Field_Type;
      Field_Name   : String)
   is
   begin
      for F of Table.Fields loop
         if not F.Is_Compound
           and then F.Field.Name = Field_Name
         then
            Compound_Key.Add_Field (F.Field);
            return;
         end if;
      end loop;
   end Add_Compound_Key_Field;

   ------------
   -- Append --
   ------------

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Fields.Field_Type'Class;
      Is_Key    : in     Boolean;
      Is_Unique : in     Boolean   := False)
   is
      Field : constant Table_Field_Access := new Table_Field (False);
   begin
      Field.Is_Key := Is_Key;
      Field.Is_Unique_Key := Is_Unique;
      Field.Field := new Kit.Fields.Field_Type'Class'(Item);

      if Item.Get_Field_Type.Is_String then
         Table.Has_String_Type := True;
      end if;

      if Is_Key then
         Table.Has_Key_Field := True;
      end if;

      Table.Fields.Append (Field);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Fields.Compound_Field_Type'Class;
      Is_Unique : in     Boolean)
   is
      Field : constant Table_Field_Access := new Table_Field (True);
   begin
      Field.Is_Key := True;
      Field.Is_Unique_Key := Is_Unique;
      Field.Compound_Field :=
        new Kit.Fields.Compound_Field_Type'Class'(Item);

      Table.Has_Key_Field := True;

      Table.Fields.Append (Field);
   end Append;

   -------------------------
   -- Base_Component_Name --
   -------------------------

   function Base_Component_Name
     (Table : Table_Type'Class)
      return String
   is
   begin
      return ".Local.T" & Table.Index_Image & "_Data";
   end Base_Component_Name;

   ---------------------
   -- Base_Field_Name --
   ---------------------

   function Base_Field_Name
     (Table  : Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type'Class;
      Field       : Kit.Fields.Field_Type'Class)
      return String
   is
      pragma Unreferenced (Table);
   begin
      return Object_Name & Base.Base_Component_Name
        & ".Db." & Field.Ada_Name;
   end Base_Field_Name;

   ---------------------
   -- Base_Index_Name --
   ---------------------

   function Base_Index_Name
     (Table : Table_Type'Class)
      return String
   is
   begin
      return ".T" & Table.Index_Image & "_Idx";
   end Base_Index_Name;

   function Compound_Field
     (Key : Key_Cursor;
      Index : Positive)
      return Kit.Fields.Field_Type'Class
   is
      It    : constant Field_Vectors.Cursor :=
                Field_Vectors.Cursor (Key);
      Field : Kit.Fields.Compound_Field_Type'Class
      renames Field_Vectors.Element (It).Compound_Field.all;
   begin
      return Field.Field (Index);
   end Compound_Field;

   --------------------------
   -- Compound_Field_Count --
   --------------------------

   function Compound_Field_Count
     (Key : Key_Cursor)
      return Natural
   is
      It    : constant Field_Vectors.Cursor :=
                Field_Vectors.Cursor (Key);
      Field : Kit.Fields.Compound_Field_Type'Class
      renames Field_Vectors.Element (It).Compound_Field.all;
   begin
      return Field.Field_Count;
   end Compound_Field_Count;

   -------------------
   -- Contains_Base --
   -------------------

   function Contains_Base
     (Table : Table_Type;
      Name     : String)
      return Boolean
   is
   begin
      for T of Table.Bases loop
         if T.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Base;

   --------------------
   -- Contains_Field --
   --------------------

   function Contains_Field
     (Table : Table_Type;
      Name     : String)
      return Boolean
   is
   begin
      for F of Table.Fields loop
         if F.Field.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Field;

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (Item : in out Table_Type;
      Name : in     String)
   is
      use type Marlowe.Table_Index;
   begin
      Kit.Names.Create (Kit.Names.Root_Named_Object (Item), Name);
      Item.Magic := Get_Magic_Number (Name);
      Item.Index := Current_Table;
      Current_Table := Current_Table + 1;
   end Create;

   ------------------------------
   -- Database_Index_Component --
   ------------------------------

   function Database_Index_Component
     (Table       : Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type'Class)
      return String
   is
   begin
      if Table.Ada_Name = Base.Ada_Name then
         return Object_Name & ".Index";
      else
         return Object_Name
           & Table.Base_Component_Name & ".Db"
           & Base.Base_Index_Name;
      end if;
   end Database_Index_Component;

   ------------------------------
   -- Database_Index_Component --
   ------------------------------

   function Database_Index_Component
     (Table       : Table_Type'Class;
      Object_Name : String;
      Base_1      : Table_Type'Class;
      Base_2      : Table_Type'Class)
      return String
   is
      pragma Unreferenced (Table);
   begin
      return Object_Name
        & Base_1.Base_Component_Name & ".Db"
        & Base_2.Base_Index_Name;
   end Database_Index_Component;

   -------------
   -- Element --
   -------------

   function Element
     (Position : Base_Cursor)
      return Table_Type'Class
   is
   begin
      return Table_Vectors.Element (Table_Vectors.Cursor (Position)).all;
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (Position : Field_Cursor)
      return Kit.Fields.Field_Type'Class
   is
   begin
      return Field_Vectors.Element
        (Field_Vectors.Cursor (Position)).Field.all;
   end Element;

   -----------
   -- Field --
   -----------

   function Field (Position : Key_Cursor;
                   Index    : Positive)
                   return Kit.Fields.Field_Type'Class
   is
      F : constant Table_Field_Access :=
            Field_Vectors.Element
              (Field_Vectors.Cursor (Position));
   begin
      if F.Is_Compound then
         return F.Compound_Field.Field (Index);
      else
         pragma Assert (Index = 1);
         return F.Field.all;
      end if;
   end Field;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Position : Key_Cursor) return Natural is
      F : constant Table_Field_Access :=
            Field_Vectors.Element
              (Field_Vectors.Cursor (Position));
   begin
      if F.Is_Compound then
         return F.Compound_Field.Field_Count;
      else
         return 1;
      end if;
   end Field_Count;

   ----------------
   -- First_Base --
   ----------------

   function First_Base (Table : Table_Type) return Base_Cursor is
   begin
      return Base_Cursor (Table.Bases.First);
   end First_Base;

   -----------------
   -- First_Field --
   -----------------

   function First_Field (Table : Table_Type) return Field_Cursor is
   begin
      return Field_Cursor (Table.Fields.First);
   end First_Field;

   ----------------------
   -- Get_Magic_Number --
   ----------------------

   function Get_Magic_Number
     (From_Text : String)
      return Natural
   is
      type Magic_Number is mod 2**31;
      Result : Magic_Number := 312345;
   begin
      for I in From_Text'Range loop
         Result := Result * (117 + Magic_Number (I))
           + Character'Pos (From_Text (I));
      end loop;
      return Natural (Result);
   end Get_Magic_Number;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element
     (Position : Base_Cursor)
      return Boolean
   is
   begin
      return Table_Vectors.Has_Element (Table_Vectors.Cursor (Position));
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element
     (Position : Field_Cursor)
      return Boolean
   is
   begin
      return Field_Vectors.Has_Element (Field_Vectors.Cursor (Position));
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element
     (Position : Key_Cursor)
      return Boolean
   is
   begin
      return Field_Vectors.Has_Element (Field_Vectors.Cursor (Position));
   end Has_Element;

   function Has_Key_Field (Item : Table_Type) return Boolean is
   begin
      return Item.Has_Key_Field;
   end Has_Key_Field;

   ---------------------
   -- Has_String_Type --
   ---------------------

   function Has_String_Type (Item : Table_Type) return Boolean is
   begin
      return Item.Has_String_Type;
   end Has_String_Type;

   -------------------------
   -- Implementation_Name --
   -------------------------

   function Implementation_Name
     (Item : Table_Type)
      return String
   is
   begin
      return Item.Ada_Name & "_Implementation";
   end Implementation_Name;

   --------------------------------
   -- Implementation_Record_Type --
   --------------------------------

   function Implementation_Record_Type (Item : Table_Type) return String is
   begin
      return Item.Ada_Name & "_Database_Record";
   end Implementation_Record_Type;

   -----------------
   -- Index_Image --
   -----------------

   function Index_Image
     (Table : Table_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Fixed.Trim
        (Marlowe.Table_Index'Image (Table.Index),
         Ada.Strings.Left);
   end Index_Image;

   ---------------------
   -- Inherited_Field --
   ---------------------

   function Inherited_Field (Table : Table_Type;
                             Field : Kit.Fields.Field_Type'Class)
                             return Boolean
   is
   begin
      for F of Table.Fields loop
         if not F.Is_Compound
           and then F.Field.Ada_Name = Field.Ada_Name
         then
            return False;
         end if;
      end loop;
      return True;
   end Inherited_Field;

   --------------------
   -- Interface_Name --
   --------------------

   function Interface_Name
     (Item : Table_Type)
      return String
   is
   begin
      return Item.Ada_Name & "_Interface";
   end Interface_Name;

   ---------------------
   -- Is_Compound_Key --
   ---------------------

   function Is_Compound_Key (Position : Key_Cursor)
                             return Boolean
   is
      use Field_Vectors;
      Item : constant Table_Field_Access := Element (Cursor (Position));
   begin
      return Item.Is_Compound;
   end Is_Compound_Key;

   ------------------
   -- Is_Key_Field --
   ------------------

   function Is_Key_Field (Item : Table_Type;
                          Field : Kit.Fields.Field_Type'Class)
                          return Boolean
   is
   begin
      for F of Item.Fields loop
         if not F.Is_Compound then
            if F.Field.Name = Field.Name
              and then F.Is_Key
            then
               return True;
            end if;
         else
            if F.Compound_Field.Contains (Field) then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Key_Field;

   ---------------
   -- Is_Unique --
   ---------------

   function Is_Unique (Position : Key_Cursor)
                       return Boolean
   is
      use Field_Vectors;
   begin
      return Element (Cursor (Position)).Is_Unique_Key;
   end Is_Unique;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Table     : Table_Type;
                      Process   : not null access
                        procedure (Item : Table_Type'Class);
                      Inclusive : Boolean;
                      Table_First : Boolean := False)
   is

      Visited : Table_Vectors.Vector;
      --  Queue   : Table_Vectors.Vector;

      procedure Recurse (Base : Table_Access);

      -------------
      -- Recurse --
      -------------

      procedure Recurse (Base : Table_Access) is
      begin
         if not Visited.Contains (Base) then
            Visited.Append (Base);
            for B of Base.Bases loop
               Recurse (B);
            end loop;
            Process (Base.all);
         end if;
      end Recurse;

   begin
      if Inclusive and Table_First then
         Process (Table);
      end if;

      for B of Table.Bases loop
         Recurse (B);
         --  Queue.Append (B);
      end loop;

--        while not Queue.Is_Empty loop
--           declare
--              Item : constant Table_Access := Queue.Last_Element;
--           begin
--              Queue.Delete_Last;
--              if not Visited.Contains (Item) then
--                 Visited.Append (Item);
--                 for B of Item.Bases loop
--                    Queue.Append (B);
--                 end loop;
--              end if;
--
--              Process (Item.all);
--           end;
--        end loop;

      if Inclusive and not Table_First then
         Process (Table);
      end if;

   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Table : Table_Type;
      Process  : not null access
                        procedure (Item : Kit.Fields.Field_Type'Class))
   is
      procedure Call_Process (Position : Field_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Field_Vectors.Cursor) is
      begin
         if not Field_Vectors.Element (Position).Is_Compound then
            Process (Field_Vectors.Element (Position).Field.all);
         end if;
      end Call_Process;

   begin
      Table.Fields.Iterate (Call_Process'Access);
   end Iterate;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All (Table : Table_Type'Class;
                          Process  : not null access
                            procedure (Table : Table_Type'Class;
                                       Field : Kit.Fields.Field_Type'Class))
   is
      procedure Call_Process (Base   : Table_Type'Class;
                              Field  : Field_Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Base   : Table_Type'Class;
                              Field  : Field_Cursor)
      is
      begin
         Process
           (Base,
            Field_Vectors.Element (Field_Vectors.Cursor (Field)).Field.all);
      end Call_Process;

   begin
      Iterate_All (Table, Call_Process'Access);
   end Iterate_All;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All (Table : Table_Type'Class;
                          Process  : not null access
                            procedure (Table : Table_Type'Class;
                                       Field : Field_Cursor))
   is
      Visited        : Table_Vectors.Vector;
      Visited_Fields : Field_Vectors.Vector;
      Queue          : Table_Vectors.Vector;

      procedure Iterate_Fields (T : Table_Type'Class);

      --------------------
      -- Iterate_Fields --
      --------------------

      procedure Iterate_Fields (T : Table_Type'Class) is
         It : Field_Vectors.Cursor := T.Fields.First;
      begin
         while Field_Vectors.Has_Element (It) loop
            declare
               F : constant Table_Field_Access :=
                     Field_Vectors.Element (It);
            begin
               if not Visited_Fields.Contains (F) then
                  Visited_Fields.Append (F);
                  if not F.Is_Compound then
                     Process (T, Field_Cursor (It));
                  end if;
               end if;
            end;
            Field_Vectors.Next (It);
         end loop;
      end Iterate_Fields;

   begin
      for B of Table.Bases loop
         Queue.Append (B);
      end loop;

      Iterate_Fields (Table);

      while not Queue.Is_Empty loop
         declare
            Item : constant Table_Access := Queue.First_Element;
         begin
            Queue.Delete_First;
            Visited.Append (Item);
            for B of Item.Bases loop
               if not Visited.Contains (B) then
                  Queue.Append (B);
               end if;
            end loop;
            Iterate_Fields (Item.all);
         end;
      end loop;
   end Iterate_All;

   --------------
   -- Key_Size --
   --------------

   function Key_Size (Position : Key_Cursor)
                      return Positive
   is
      use Field_Vectors;
      Item : constant Table_Field_Access := Element (Cursor (Position));
   begin
      return Marlowe.Database_Index'Size / 8
        + (if Item.Is_Compound
           then Item.Compound_Field.Size
           else Item.Field.Size);
   end Key_Size;

   --------------------
   -- Key_To_Storage --
   --------------------

   function Key_To_Storage
     (Table       : Table_Type'Class;
      Key         : Key_Cursor;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Table);
      F : constant Table_Field_Access :=
            Field_Vectors.Element (Field_Vectors.Cursor (Key));
      Prefix : constant String :=
                 (if Object_Name = "" then "" else Object_Name & ".");
   begin
      if F.Is_Compound then
         declare
            use Aquarius.Drys;
            use Aquarius.Drys.Expressions;
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Ada_Name (Key) & "_To_Storage");
         begin
            for I in 1 .. Field_Count (Key) loop
               Result.Add_Actual_Argument
                 (Object (Prefix  & Field (Key, I).Ada_Name));
            end loop;

--              for I in 1 .. F.Compound_Field.Field_Count loop
--                 Result.Add_Actual_Argument
--                   (Object (Prefix & F.Compound_Field.all.Ada_Name (I)));
--              end loop;
            return Result;
         end;
      else
         return Key_Type (Key).To_Storage_Array
           (Prefix & Ada_Name (Key));
      end if;
   end Key_To_Storage;

   --------------
   -- Key_Type --
   --------------

   function Key_Type (Position : Key_Cursor)
                      return Kit.Types.Kit_Type'Class
   is
      use Field_Vectors;
      Item : constant Table_Field_Access := Element (Cursor (Position));
      pragma Assert (not Item.Is_Compound);
      pragma Assert (Item.Is_Key);
   begin
      return Item.Field.Get_Field_Type;
   end Key_Type;

   ------------------
   -- Magic_Number --
   ------------------

   function Magic_Number
     (Item : Table_Type)
      return Natural
   is
   begin
      return Item.Magic;
   end Magic_Number;

   ----------
   -- Name --
   ----------

   function Name (Position : Key_Cursor)
                       return String
   is
      use Field_Vectors;
      Item : constant Table_Field_Access := Element (Cursor (Position));
   begin
      if Item.Is_Compound then
         return Item.Compound_Field.Name;
      else
         return Item.Field.Name;
      end if;
   end Name;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (Position : in out Base_Cursor) is
   begin
      Table_Vectors.Next (Table_Vectors.Cursor (Position));
   end Next;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (Position : in out Field_Cursor) is
   begin
      Field_Vectors.Next (Field_Vectors.Cursor (Position));
   end Next;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name
     (Item : Table_Type)
      return String
   is
   begin
      return Item.Ada_Name;
   end Package_Name;

   ---------------------
   -- Reference_Index --
   ---------------------

   function Reference_Index
     (Item : Table_Type)
      return Marlowe.Table_Index
   is
   begin
      return Item.Index;
   end Reference_Index;

   --------------------
   -- Reference_Type --
   --------------------

   function Reference_Type (Item : Table_Type) return String is
   begin
      return Item.Ada_Name & "_Reference";
   end Reference_Type;

   ----------------
   -- Same_Field --
   ----------------

   function Same_Field (Left, Right : Table_Field_Access) return Boolean is
   begin
      if Left.Is_Compound then
         return Right.Is_Compound
           and then Left.Compound_Field.Ada_Name
             = Right.Compound_Field.Ada_Name;
      else
         return not Right.Is_Compound
           and then Left.Field.Ada_Name = Right.Field.Ada_Name;
      end if;
   end Same_Field;

   ----------------
   -- Same_Table --
   ----------------

   function Same_Table (Left, Right : Table_Access) return Boolean is
   begin
      return Left.Ada_Name = Right.Ada_Name;
   end Same_Table;

   -----------------
   -- Scan_Fields --
   -----------------

   procedure Scan_Fields
     (Table : Table_Type;
      Process  : not null access
        procedure (Field : Kit.Fields.Field_Type'Class))
   is
      procedure Call_Process (Position : Field_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Field_Vectors.Cursor) is
      begin
         if not Field_Vectors.Element (Position).Is_Compound then
            Process (Field_Vectors.Element (Position).Field.all);
         end if;
      end Call_Process;

   begin
      Table.Fields.Iterate (Call_Process'Access);
   end Scan_Fields;

   ---------------
   -- Scan_Keys --
   ---------------

   procedure Scan_Keys
     (Table : Table_Type;
      Process  : not null access
                        procedure (Item : Key_Cursor))
   is
--        procedure Call_Process (Key_Table : Table_Type'Class;
--                                Key       : Key_Cursor);
--
--        ------------------
--        -- Call_Process --
--        ------------------
--
--        procedure Call_Process (Key_Table : Table_Type'Class;
--                                Key       : Key_Cursor)
--        is
--           pragma Unreferenced (Key_Table);
--        begin
--           Process (Key);
--        end Call_Process;

      It : Field_Vectors.Cursor := Table.Fields.First;
   begin
      --        Table.Scan_Keys (Call_Process'Access);
      while Field_Vectors.Has_Element (It) loop
         if Field_Vectors.Element (It).Is_Key then
            Process (Key_Cursor (It));
         end if;
         Field_Vectors.Next (It);
      end loop;

   end Scan_Keys;

   ---------------
   -- Scan_Keys --
   ---------------

   procedure Scan_Keys
     (Table : Table_Type;
      Process          : not null access procedure
        (Base   : Table_Type'Class;
         Key    : Key_Cursor))
   is

      procedure Scan_Base (Base : Table_Type'Class);

      ---------------
      -- Scan_Base --
      ---------------

      procedure Scan_Base (Base : Table_Type'Class) is

         procedure Call_Process (Position : Field_Vectors.Cursor);

         ------------------
         -- Call_Process --
         ------------------

         procedure Call_Process (Position : Field_Vectors.Cursor) is
         begin
            if Field_Vectors.Element (Position).Is_Key then
               Process (Base, Key_Cursor (Position));
            end if;
         end Call_Process;

      begin
         Base.Fields.Iterate (Call_Process'Access);
      end Scan_Base;

   begin
      Table.Iterate (Scan_Base'Access, True, False);
   end Scan_Keys;

   ---------------
   -- Scan_Keys --
   ---------------

   procedure Scan_Keys
     (Table    : Table_Type;
      Containing_Field : Kit.Fields.Field_Type'Class;
      Process          : not null access procedure
        (Table  : Table_Type'Class;
         Base   : Table_Type'Class;
         Key    : Key_Cursor))
   is

      procedure Call_Process (Base  : Table_Type'Class;
                              Key   : Key_Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Base  : Table_Type'Class;
                              Key   : Key_Cursor)
      is
         F : constant Table_Field_Access :=
               Field_Vectors.Element (Field_Vectors.Cursor (Key));
      begin
         if F.Is_Compound then
            if F.Compound_Field.Contains (Containing_Field) then
               Process (Table, Base, Key);
            end if;
         elsif F.Field.Ada_Name = Containing_Field.Ada_Name then
            Process (Table, Base, Key);
         end if;
      end Call_Process;

   begin
      Table.Scan_Keys (Call_Process'Access);
   end Scan_Keys;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage (Table       : Table_Type'Class;
                        Base_Table  : Table_Type'Class;
                        Object_Name : String;
                        Key         : Key_Cursor)
                        return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions;
      F : constant Table_Field_Access :=
            Field_Vectors.Element (Field_Vectors.Cursor (Key));

      Key_Index : constant String :=
                    Table.Database_Index_Component
                      (Object_Name, Base_Table);
      Index_Part : constant Expression'Class :=
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Storage_Array",
                        Object (Key_Index));
   begin
      if F.Is_Compound then
         declare
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Ada_Name (Key) & "_To_Storage");
         begin
            for I in 1 .. F.Compound_Field.Field_Count loop
               Result.Add_Actual_Argument
                 (Object ("Item." & F.Compound_Field.Field (I).Ada_Name));
            end loop;
            return Long_Operator ("&", Result, Index_Part);
         end;
      else
         declare
            Key_Part : constant Expression'Class :=
                         F.Field.Get_Field_Type.To_Storage_Array
                           (Object_Name & "." & Ada_Name (Key));
         begin
            return Long_Operator ("&", Key_Part, Index_Part);
         end;
      end if;
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage (Key_Value_Name   : String;
                        Index_Value_Name : String;
                        Key              : Key_Cursor)
                        return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions;
      F : constant Table_Field_Access :=
            Field_Vectors.Element (Field_Vectors.Cursor (Key));
   begin
      if F.Is_Compound then
         declare
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Ada_Name (Key) & "_To_Storage");
         begin
            Result.Add_Actual_Argument
              (Aquarius.Drys.Object (Key_Value_Name));
            return Result;
         end;
      else
         declare
            Key_Part : constant Expression'Class :=
                         F.Field.Get_Field_Type.To_Storage_Array
                           (Key_Value_Name);
            Index_Part : constant Expression'Class :=
                           New_Function_Call_Expression
                             ("Marlowe.Key_Storage.To_Storage_Array",
                              Object (Index_Value_Name));
         begin
            return Long_Operator ("&", Key_Part, Index_Part);
         end;
      end if;
   end To_Storage;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Item : Table_Type)
      return String
   is
   begin
      return Item.Ada_Name & "_Type";
   end Type_Name;

end Kit.Tables;
