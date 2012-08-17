with Ada.Strings.Fixed;

with Aquarius.Drys.Expressions;

package body Kit.Schema.Tables is

   Current_Table : Marlowe.Table_Index := 1;

   Recursively_Add_Bases : constant Boolean := False;

   function Get_Magic_Number
     (From_Text : String)
      return Natural;

   --------------
   -- Add_Base --
   --------------

   procedure Add_Base
     (Table     : in out Table_Type;
      Item      : in     Table_Type'Class)
   is

      procedure Increment_Base_Length (Base : Table_Type'Class);

      ---------------------------
      -- Increment_Base_Length --
      ---------------------------

      procedure Increment_Base_Length (Base : Table_Type'Class) is
         use type System.Storage_Elements.Storage_Offset;
      begin
         if not Table.Contains_Base (Base.Name) then
            Table.Bases_Length := Table.Bases_Length
              + Marlowe.Database_Index'Size / System.Storage_Unit;
            Table.Base_Layout.Append (Base.Index);
         end if;
      end Increment_Base_Length;

   begin
      if Item.Has_String_Type then
         Table.Has_String_Type := True;
      end if;

      if Item.Has_Key_Field then
         Table.Has_Key_Field := True;
      end if;

      Item.Iterate (Increment_Base_Length'Access,
                    Inclusive => True);

      Table.Bases.Append (new Table_Type'Class'(Item));

      if Recursively_Add_Bases then
         for Base of Item.Bases loop
            if not Table.Contains_Base (Base.Name) then
               Table.Add_Base (Base.all);
            end if;
         end loop;
      end if;
   end Add_Base;

   -------------
   -- Add_Key --
   -------------

   procedure Add_Key
     (Table     : in out Table_Type;
      Key       : in     Kit.Schema.Keys.Key_Type'Class)
   is
      K : constant Table_Key_Access :=
            new Kit.Schema.Keys.Key_Type'Class'(Key);
   begin
      Table.Keys.Append (K);
      Table.Has_Key_Field := True;
   end Add_Key;

   -------------------
   -- Add_Key_Field --
   -------------------

   procedure Add_Key_Field
     (Table      : in out Table_Type'Class;
      Key        : in out Kit.Schema.Keys.Key_Type'Class;
      Field_Name : in String)
   is
      Standard_Field_Name : constant String :=
                              Kit.Names.Standard_Name (Field_Name);
   begin
      for F of Table.Fields loop
         if F.Field.Standard_Name = Standard_Field_Name then
            Key.Add_Field (F.Field);
            return;
         end if;
      end loop;
      for Base of Table.Bases loop
         for F of Base.Fields loop
            if F.Field.Standard_Name = Standard_Field_Name then
               Key.Add_Field (F.Field);
               return;
            end if;
         end loop;
      end loop;

      raise Constraint_Error with
        "key field " & Field_Name
        & " does not exist in table " & Table.Ada_Name;
   end Add_Key_Field;

   ------------
   -- Append --
   ------------

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Schema.Fields.Field_Type'Class)
   is
      use type System.Storage_Elements.Storage_Offset;
      Field : constant Table_Field_Access := new Table_Field;
   begin
      Field.Field := new Kit.Schema.Fields.Field_Type'Class'(Item);
      Field.Start := Table.Fields_Length;
      Field.Length :=
        System.Storage_Elements.Storage_Count (Item.Get_Field_Type.Size);
      Table.Fields_Length := Table.Fields_Length + Field.Length;
      if Item.Get_Field_Type.Is_String then
         Table.Has_String_Type := True;
      end if;

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
      Field       : Kit.Schema.Fields.Field_Type'Class)
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

   ----------------
   -- Base_Start --
   ----------------

   function Base_Start (Table : Table_Type;
                        Base  : Table_Type'Class)
                        return System.Storage_Elements.Storage_Offset
   is
      use type Marlowe.Table_Index;
      use System.Storage_Elements;
      Result : Storage_Offset := Table.Header_Length + Table.Fields_Length;
   begin
      for T of Table.Base_Layout loop
         if Base.Index = T then
            return Result;
         end if;
         Result := Result + Marlowe.Database_Index'Size / System.Storage_Unit;
      end loop;
      raise Constraint_Error with
        "table " & Table.Ada_Name & " is not derived from "
          & Base.Ada_Name;
   end Base_Start;

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
      for B of Table.Bases loop
         if B.Contains_Field (Name) then
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
      Item.Fields_Length := 0;
      Item.Bases_Length := 0;
      Item.Header_Length := 4;
      Item.Bases.Clear;
      Item.Base_Layout.Clear;
      Item.Has_String_Type := False;
      Item.Has_Key_Field := False;
      Item.Has_Compound_Key_Field := False;
      Item.Fields.Clear;
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

   -----------------
   -- Field_Start --
   -----------------

   function Field_Start (Table : Table_Type;
                         Field : Kit.Schema.Fields.Field_Type'Class)
                         return System.Storage_Elements.Storage_Offset
   is
   begin
      for I in 1 .. Table.Fields.Last_Index loop
         declare
            use type System.Storage_Elements.Storage_Offset;
            F : constant Table_Field_Access := Table.Fields.Element (I);
         begin
            if F.Field.Name = Field.Name then
               return Table.Header_Length + F.Start;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "table " & Table.Ada_Name & ": no such field " & Field.Ada_Name;
   end Field_Start;

   function Find_Key
     (Table : Table_Type'Class;
      Property : not null access
        function (K : Kit.Schema.Keys.Key_Type'Class)
      return Boolean)
      return Kit.Schema.Keys.Key_Type'Class
   is

      Visited : Table_Vectors.Vector;
      Result  : Table_Key_Access := null;

      procedure Process (Base : Table_Type'Class);

      procedure Recurse (Base : Table_Access);

      -------------
      -- Process --
      -------------

      procedure Process (Base : Table_Type'Class) is
      begin
         Result := null;
         for K of Base.Keys loop
            if Property (K.all) then
               Result := K;
               exit;
            end if;
         end loop;
      end Process;

      -------------
      -- Recurse --
      -------------

      procedure Recurse (Base : Table_Access) is
      begin
         if not Visited.Contains (Base) then
            Visited.Append (Base);
            for B of Base.Bases loop
               Recurse (B);
               exit when Result /= null;
            end loop;
            if Result = null then
               Process (Base.all);
            end if;
         end if;
      end Recurse;

   begin

      Process (Table);

      if Result = null then
         for B of Table.Bases loop
            Recurse (B);
         end loop;
      end if;

      return Result.all;

   end Find_Key;

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

   ----------------------------
   -- Has_Compound_Key_Field --
   ----------------------------

   function Has_Compound_Key_Field (Item : Table_Type) return Boolean is
   begin
      return Item.Has_Compound_Key_Field;
   end Has_Compound_Key_Field;

   -------------------
   -- Has_Key_Field --
   -------------------

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
                             Field : Kit.Schema.Fields.Field_Type'Class)
                             return Boolean
   is
   begin
      for F of Table.Fields loop
         if F.Field.Ada_Name = Field.Ada_Name then
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
      end loop;

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
                        procedure (Item : Kit.Schema.Fields.Field_Type'Class))
   is
      procedure Call_Process (Position : Field_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Field_Vectors.Cursor) is
      begin
         Process (Field_Vectors.Element (Position).Field.all);
      end Call_Process;

   begin
      Table.Fields.Iterate (Call_Process'Access);
   end Iterate;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Table : Table_Type'Class;
      Process  : not null access
        procedure (Table : Table_Type'Class;
                   Field : Kit.Schema.Fields.Field_Type'Class))
   is
      Visited        : Table_Vectors.Vector;
      Visited_Fields : Field_Vectors.Vector;
      Queue          : Table_Vectors.Vector;

      procedure Iterate_Fields (T : Table_Type'Class);

      --------------------
      -- Iterate_Fields --
      --------------------

      procedure Iterate_Fields (T : Table_Type'Class) is
      begin
         for F of T.Fields loop
            if not Visited_Fields.Contains (F) then
               Visited_Fields.Append (F);
               Process (T, F.Field.all);
            end if;
         end loop;
      end Iterate_Fields;

   begin
      for B of Table.Bases loop
         Queue.Append (B);
      end loop;

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

      Iterate_Fields (Table);

   end Iterate_All;

   ---------
   -- Key --
   ---------

   function Key
     (Table : Table_Type;
      Name  : String)
      return Kit.Schema.Keys.Key_Type'Class
   is

      function Same_Name (K : Kit.Schema.Keys.Key_Type'Class)
                          return Boolean
      is (K.Standard_Name = Name);

   begin
      return Table.Find_Key (Same_Name'Access);

--        for K of Table.Keys loop
--           Ada.Text_IO.Put_Line ("    key: " & K.Standard_Name);
--           if K.Standard_Name = Name then
--              return K.all;
--           end if;
--        end loop;
--        for Base of Table.Bases loop
--           Ada.Text_IO.Put_Line ("  base: " & Base.Ada_Name);
--           for K of Base.Keys loop
--              Ada.Text_IO.Put_Line ("   key: " & K.Standard_Name);
--              if K.Standard_Name = Name then
--                 return K.all;
--              end if;
--           end loop;
--        end loop;
--        raise Constraint_Error with
--          "no such key " & Name & " in table " & Table.Ada_Name;
   end Key;

   ------------------------
   -- Key_Reference_Name --
   ------------------------

   function Key_Reference_Name
     (Table : Table_Type'Class;
      Key   : Kit.Schema.Keys.Key_Type'Class)
      return String
   is
   begin
      return Table.Key_Reference_Name (Key.Ada_Name);
   end Key_Reference_Name;

   ------------------------
   -- Key_Reference_Name --
   ------------------------

   function Key_Reference_Name
     (Table    : Table_Type'Class;
      Key_Name : String)
      return String
   is
   begin
      return "T" & Table.Index_Image & "_"
        & Kit.Names.Ada_Name (Key_Name)
        & "_Ref";
   end Key_Reference_Name;

   --------------------
   -- Key_To_Storage --
   --------------------

   function Key_To_Storage
     (Table       : Table_Type'Class;
      Key         : Kit.Schema.Keys.Key_Type'Class;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Table);
      Prefix : constant String :=
                 (if Object_Name = "" then "" else Object_Name & ".");
   begin
      if Key.Field_Count > 1 then
         declare
            use Aquarius.Drys;
            use Aquarius.Drys.Expressions;
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Key.Ada_Name & "_To_Storage");
         begin
            for I in 1 .. Key.Field_Count loop
               Result.Add_Actual_Argument
                 (Object (Prefix  & Key.Field (I).Ada_Name));
            end loop;

            return Result;
         end;
      else
         return Key.Field (1).Get_Field_Type.To_Storage_Array
           (Prefix & Key.Ada_Name);
      end if;
   end Key_To_Storage;

   ------------
   -- Length --
   ------------

   function Length
     (Item : Table_Type)
      return System.Storage_Elements.Storage_Count
   is
      use type System.Storage_Elements.Storage_Offset;

   begin
      return Item.Header_Length + Item.Bases_Length + Item.Fields_Length;
   end Length;

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

   ----------------------
   -- References_Table --
   ----------------------

   function References_Table (Item    : Table_Type;
                              Other   : Table_Type'Class)
                              return Boolean
   is
   begin
      for I in 1 .. Item.Fields.Last_Index loop
         declare
            F : constant Table_Field_Access :=
                  Item.Fields.Element (I);
         begin
            if F.Field.Get_Field_Type.Is_Reference_To (Other.Name) then
               return True;
            end if;
         end;
      end loop;
      for B of Item.Bases loop
         if B.References_Table (Other) then
            return True;
         end if;
      end loop;
      return False;
   end References_Table;

   ----------------
   -- Same_Field --
   ----------------

   function Same_Field (Left, Right : Table_Field_Access) return Boolean is
   begin
      return Left.Field.Ada_Name = Right.Field.Ada_Name;
   end Same_Field;

   --------------
   -- Same_Key --
   --------------

   function Same_Key (Left, Right : Table_Key_Access) return Boolean is
   begin
      return Left.Standard_Name = Right.Standard_Name;
   end Same_Key;

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
        procedure (Field : Kit.Schema.Fields.Field_Type'Class))
   is
      procedure Call_Process (Position : Field_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Field_Vectors.Cursor) is
      begin
         Process (Field_Vectors.Element (Position).Field.all);
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
        procedure (Item : Kit.Schema.Keys.Key_Type'Class))
   is
   begin
      for K of Table.Keys loop
         Process (K.all);
      end loop;
   end Scan_Keys;

   ---------------
   -- Scan_Keys --
   ---------------

   procedure Scan_Keys
     (Table : Table_Type;
      Process          : not null access procedure
        (Base   : Table_Type'Class;
         Key    : Kit.Schema.Keys.Key_Type'Class))
   is

      procedure Scan_Base (Base : Table_Type'Class);

      ---------------
      -- Scan_Base --
      ---------------

      procedure Scan_Base (Base : Table_Type'Class) is
      begin
         for K of Base.Keys loop
            Process (Base, K.all);
         end loop;
      end Scan_Base;

   begin
      Table.Iterate (Scan_Base'Access, True, False);
   end Scan_Keys;

   ---------------
   -- Scan_Keys --
   ---------------

   procedure Scan_Keys
     (Table    : Table_Type;
      Containing_Field : Kit.Schema.Fields.Field_Type'Class;
      Process          : not null access procedure
        (Table  : Table_Type'Class;
         Base   : Table_Type'Class;
         Key    : Kit.Schema.Keys.Key_Type'Class))
   is

      procedure Call_Process (Base : Table_Type'Class;
                              Key  : Kit.Schema.Keys.Key_Type'Class);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Base : Table_Type'Class;
                              Key  : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         if Key.Contains (Containing_Field) then
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
                        Key_Table   : Table_Type'Class;
                        Object_Name : String;
                        Key         : Kit.Schema.Keys.Key_Type'Class;
                        With_Index  : Boolean)
                        return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions;
      Key_Index : constant String :=
                    Table.Database_Index_Component
                      (Object_Name, Base_Table);
      Index_Part : constant Expression'Class :=
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Storage_Array",
                        Object (Key_Index));
      Object_Component : constant String :=
                           (if Object_Name = ""
                            then ""
                            elsif Object_Name (Object_Name'Last) = '_'
                            then Object_Name
                            else Object_Name & ".");
   begin
      if Key.Field_Count > 1 then
         declare
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Ada_Name (Key_Table)
                          & "_Impl."
                          & Key.Ada_Name & "_To_Storage");
         begin
            for I in 1 .. Key.Field_Count loop
               Result.Add_Actual_Argument
                 (Object
                    (Object_Component
                     & Key.Field (I).Ada_Name));
            end loop;
            if With_Index then
               return Long_Operator ("&", Result, Index_Part);
            else
               return Result;
            end if;
         end;
      else
         declare
            Key_Part : constant Expression'Class :=
                         Key.Field (1).Get_Field_Type.To_Storage_Array
                           (Object_Component & Key.Ada_Name);
         begin
            if With_Index then
               return Long_Operator ("&", Key_Part, Index_Part);
            else
               return Key_Part;
            end if;
         end;
      end if;
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage (Key_Value_Name   : String;
                        Index_Value_Name : String;
                        Key              : Kit.Schema.Keys.Key_Type'Class)
                        return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions;
   begin
      if Key.Field_Count > 1 then
         declare
            Result : Function_Call_Expression :=
                       New_Function_Call_Expression
                         (Key.Ada_Name & "_To_Storage");
         begin
            Result.Add_Actual_Argument
              (Aquarius.Drys.Object (Key_Value_Name));
            return Result;
         end;
      else
         declare
            Key_Part : constant Expression'Class :=
                         Key.Field (1).Get_Field_Type.To_Storage_Array
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

end Kit.Schema.Tables;
