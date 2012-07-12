with Ada.Strings.Unbounded.Hash;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Indefinite_Hashed_Maps;

with Ada.Strings.Fixed;

package body Abydos.Environments is

   package Entry_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Evaluable'Class,
        Hash            => Hash,
        Equivalent_Keys => "=");

   type Environment_Record is
      record
         Db      : Kit.Database_Access;
         Current : Kit.Database_Record;
         Map     : Entry_Map.Map;
      end record;

   type Single_Value is new Evaluable with
      record
         V : Values.Value;
      end record;

   overriding
   function Evaluate (Item : Single_Value;
                      Args : Argument_List'Class;
                      Env  : Environment'Class)
                      return Values.Value;

   overriding
   function Formal_Argument_Count (Item : Single_Value) return Natural;

   overriding
   function Formal_Argument_Name (Item : Single_Value;
                                  Index : Positive)
                                  return String;

   ------------
   -- Append --
   ------------

   procedure Append (Args  : in out Argument_List;
                     Name  : in     String;
                     V     : Values.Value)
   is
   begin
      Args.List.Append ((new String'(Name), V));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Args  : in out Argument_List;
                     V     : Values.Value)
   is
      Name : constant String :=
               Ada.Strings.Fixed.Trim
                 (Positive'Image (Args.List.Last_Index + 1),
                  Ada.Strings.Left);
   begin
      Args.List.Append ((new String'(Name), V));
   end Append;

   -----------
   -- Apply --
   -----------

   function Apply
     (Env : Environment;
      Name : String;
      Args : Argument_List'Class)
      return Values.Value
   is
      Item : constant Evaluable'Class :=
               Env.Env.Map.Element
                 (To_Unbounded_String (Name));
   begin
      return Item.Evaluate (Args, Env);
   end Apply;

   -----------
   -- Close --
   -----------

   procedure Close (Env : in out Environment) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Kit.Root_Database_Record'Class,
                                        Kit.Database_Record);
      procedure Free is
        new Ada.Unchecked_Deallocation (Environment_Record,
                                        Environment_Access);

      use type Kit.Database_Record;
   begin
      if Env.Env.Current /= null then
         Free (Env.Env.Current);
      end if;
      Free (Env.Env);
   end Close;

   --------------
   -- Contains --
   --------------

   function Contains
     (Env   : Environment;
      Name  : String)
      return Boolean
   is
   begin
      return Env.Env.Map.Contains
        (To_Unbounded_String (Name));
   end Contains;

   -----------
   -- Count --
   -----------

   function Count (Args : Argument_List) return Natural is
   begin
      return Args.List.Last_Index;
   end Count;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (Item : Single_Value;
                      Args : Argument_List'Class;
                      Env  : Environment'Class)
                      return Values.Value
   is
      pragma Unreferenced (Args);
      pragma Unreferenced (Env);
   begin
      return Item.V;
   end Evaluate;

   ------------------
   -- First_By_Key --
   ------------------

   overriding
   function First_By_Key
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
      return Kit.Database_Record
   is
   begin
      return Tables.Env.Db.First_By_Key (Table_Index, Key_Name);
   end First_By_Key;

   ------------------------
   -- First_By_Key_Value --
   ------------------------

   overriding
   function First_By_Key_Value
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
      return Kit.Database_Record
   is
   begin
      return Tables.Env.Db.First_By_Key_Value
        (Table_Index, Key_Name, Key_Value);
   end First_By_Key_Value;

   ---------------------------
   -- Formal_Argument_Count --
   ---------------------------

   overriding
   function Formal_Argument_Count (Item : Single_Value) return Natural is
      pragma Unreferenced (Item);
   begin
      return 0;
   end Formal_Argument_Count;

   --------------------------
   -- Formal_Argument_Name --
   --------------------------

   overriding
   function Formal_Argument_Name (Item : Single_Value;
                                  Index : Positive)
                                  return String
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Index);
   begin
      return "";
   end Formal_Argument_Name;

   ---------
   -- Get --
   ---------

   overriding
   function Get
     (Database     : Environment;
      Table_Index  : Marlowe.Table_Index;
      Record_Index : Marlowe.Database_Index)
      return Kit.Database_Record
   is
      Db : constant Kit.Database_Access := Database.Env.Db;
   begin
      return Db.Get (Table_Index, Record_Index);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env  : Environment;
      Name : String)
      return Abydos.Values.Value
   is
      No_Args : Argument_List;
   begin
      return Env.Apply (Name, No_Args);
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Env   : Environment;
      Name  : String;
      Value : Abydos.Values.Value)
   is
      Item : Single_Value;
   begin
      Item.V := Value;
      Env.Insert (Name, Item);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Env   : Environment;
      Name  : String;
      Item  : Evaluable'Class)
   is
   begin
      Env.Env.Map.Insert
        (To_Unbounded_String (Name),
         Item);
   end Insert;

   ----------
   -- Item --
   ----------

   function Item (Args : Argument_List;
                  Index : Positive)
                  return Abydos.Values.Value
   is
   begin
      if Index <= Args.List.Last_Index then
         return Args.List.Element (Index).Argument_Value;
      else
         return Values.Null_Value;
      end if;
   end Item;

   ----------
   -- Item --
   ----------

   function Item (Args : Argument_List;
                  Name : String)
                  return Abydos.Values.Value
   is
   begin
      for Arg of Args.List loop
         if Arg.Argument_Name.all = Name then
            return Arg.Argument_Value;
         end if;
      end loop;
      return Values.Null_Value;
   end Item;

   ----------------------
   -- Last_Table_Index --
   ----------------------

   overriding
   function Last_Table_Index
     (Db         : Environment)
      return Marlowe.Table_Index
   is
   begin
      return Db.Env.Db.Last_Table_Index;
   end Last_Table_Index;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Db         : Environment) return String is
   begin
      return Db.Env.Db.Name;
   end Name;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Database    : Kit.Database_Access)
      return Environment
   is
   begin
      return Result : Environment do
         Result.Env := new Environment_Record;
         Result.Env.Db := Database;
      end return;
   end New_Environment;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Parent      : Environment;
      Table_Name  : in String;
      Table_Index : in Marlowe.Database_Index)
      return Environment
   is
      Result : constant Environment :=
                 New_Environment (Parent.Env.Db);
   begin
      Result.Env.Current :=
        Parent.Get (Table_Name, Table_Index);
      return Result;
   end New_Environment;

   ------------------
   -- No_Arguments --
   ------------------

   function No_Arguments return Argument_List'Class is
      Result : Argument_List;
   begin
      return Result;
   end No_Arguments;

   ------------------------
   -- Scan_By_Key_Values --
   ------------------------

   overriding
   function Scan_By_Key_Values
     (Tables         : Environment;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Kit.Database_Record
   is
   begin
      return Tables.Env.Db.Scan_By_Key_Values
        (Table_Index, Key_Name,
         Low_Key_Value,
         High_Key_Value);
   end Scan_By_Key_Values;

   --------------------
   -- To_Table_Index --
   --------------------

   overriding
   function To_Table_Index
     (Db         : Environment;
      Table_Name : String)
      return Marlowe.Table_Index
   is
   begin
      return Db.Env.Db.To_Table_Index (Table_Name);
   end To_Table_Index;

   ------------
   -- Update --
   ------------

   procedure Update (Env : Environment;
                     Name : String;
                     New_Value : Values.Value)
   is
      pragma Unreferenced (Env);
      pragma Unreferenced (Name);
      pragma Unreferenced (New_Value);
   begin
      null;
   end Update;

end Abydos.Environments;
