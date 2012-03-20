with Ada.Strings.Unbounded.Hash;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Indefinite_Hashed_Maps;

package body Abydos.Environments is

   package Entry_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Evaluable'Class,
        Hash            => Hash,
        Equivalent_Keys => "=");

   type Current_Record_Access is
     access all Kit.Root_Database_Record'Class;

   type Environment_Record is
      record
         Db      : access Kit.Root_Database_Interface'Class;
         Current : Current_Record_Access;
         Map     : Entry_Map.Map;
      end record;

   type Single_Value is new Evaluable with
      record
         V : Values.Value;
      end record;

   function Evaluate (Item : Single_Value;
                      Args : Argument_List'Class;
                      Env  : Environment'Class)
                      return Values.Value;

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
                                        Current_Record_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Environment_Record,
                                        Environment_Access);

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

   function First_By_Key
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
      return Kit.Root_Database_Record'Class
   is
   begin
      return Tables.Env.Db.First_By_Key (Table_Index, Key_Name);
   end First_By_Key;

   ------------------------
   -- First_By_Key_Value --
   ------------------------

   function First_By_Key_Value
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
      return Kit.Root_Database_Record'Class
   is
   begin
      return Tables.Env.Db.First_By_Key_Value
        (Table_Index, Key_Name, Key_Value);
   end First_By_Key_Value;

   ---------
   -- Get --
   ---------

   function Get
     (Database     : Environment;
      Table_Index  : Marlowe.Table_Index;
      Record_Index : Marlowe.Database_Index)
      return Kit.Root_Database_Record'Class
   is
   begin
      return Database.Env.Db.Get (Table_Index, Record_Index);
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
      if Index < Args.List.Last_Index then
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

   ----------
   -- Name --
   ----------

   function Name (Db         : Environment) return String is
   begin
      return Db.Env.Db.Name;
   end Name;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Database    : not null access Kit.Root_Database_Interface'Class)
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
     (Database    : not null access Kit.Root_Database_Interface'Class;
      Table_Name  : in String;
      Table_Index : in Marlowe.Database_Index)
      return Environment
   is
      Result : constant Environment :=
                 New_Environment (Database);
   begin
      Result.Env.Current :=
        new Kit.Root_Database_Record'Class'
          (Database.Get (Table_Name, Table_Index));
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

   function Scan_By_Key_Values
     (Tables         : Environment;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Kit.Root_Database_Record'Class
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

   function To_Table_Index
     (Db         : Environment;
      Table_Name : String)
      return Marlowe.Table_Index
   is
   begin
      return Db.Env.Db.To_Table_Index (Table_Name);
   end To_Table_Index;

end Abydos.Environments;
