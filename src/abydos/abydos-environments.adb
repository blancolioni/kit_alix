with Ada.Strings.Unbounded.Hash;       use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Abydos.Environments is

   package Entry_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Evaluable'Class,
        Hash            => Hash,
        Equivalent_Keys => "=");

   type Environment_Record is
      record
         Db : access Kit.Root_Database_Interface'Class;
         Map : Entry_Map.Map;
      end record;

   type Single_Value is new Evaluable with
      record
         V : Values.Value;
      end record;

   function Evaluate (Item : Single_Value;
                      Args : Values.Array_Of_Values;
                      Env  : Environment'Class)
                      return Values.Value;

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
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Environment unimplemented");
      raise Program_Error;
      return New_Environment (Database, Table_Name, Table_Index);
   end New_Environment;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Parent      : Environment'Class;
      From_Record : Kit.Root_Database_Record'Class)
      return Environment
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Environment unimplemented");
      raise Program_Error;
      return New_Environment (Parent, From_Record);
   end New_Environment;

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

   -----------
   -- Close --
   -----------

   procedure Close (Env : in out Environment) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Close unimplemented");
      raise Program_Error;
   end Close;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Env   : Environment;
      Name  : String;
      Value : Abydos.Values.Value)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error;
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
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error;
   end Insert;

   --------------
   -- Contains --
   --------------

   function Contains
     (Env   : Environment;
      Name  : String)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Contains unimplemented");
      raise Program_Error;
      return Contains (Env, Name);
   end Contains;

   ---------
   -- Get --
   ---------

   function Get
     (Env  : Environment;
      Name : String)
      return Abydos.Values.Value
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      raise Program_Error;
      return Get (Env, Name);
   end Get;

   -----------
   -- Apply --
   -----------

   function Apply
     (Env : Environment;
      Name : String;
      Args : Values.Array_Of_Values)
      return Values.Value
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Apply unimplemented");
      raise Program_Error;
      return Apply (Env, Name, Args);
   end Apply;

end Abydos.Environments;
