private with Ada.Containers.Vectors;

with Abydos.Values;
with Kit;
with Marlowe;

package Abydos.Environments is

   type Environment is new Kit.Root_Database_Interface with private;

   function Name (Db         : Environment) return String;

   function Last_Table_Index (Db         : Environment)
                              return Marlowe.Table_Index;

   function To_Table_Index (Db         : Environment;
                            Table_Name : String)
                           return Marlowe.Table_Index;

   function Get (Database     : Environment;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                 return Kit.Database_Record;

   function First_By_Key
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Kit.Database_Record;

   function First_By_Key_Value
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Kit.Database_Record;

   function Scan_By_Key_Values
     (Tables         : Environment;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Kit.Database_Record;

   function New_Environment
     (Database    : Kit.Database_Access)
     return Environment;

   function New_Environment
     (Parent      : Environment;
      Table_Name  : in String;
      Table_Index : in Marlowe.Database_Index)
     return Environment;

   procedure Close (Env : in out Environment);

   procedure Insert (Env   : Environment;
                     Name  : String;
                     Value : Abydos.Values.Value);

   type Argument_List is tagged private;
   function No_Arguments return Argument_List'Class;
   function Count (Args : Argument_List) return Natural;
   function Item (Args : Argument_List;
                  Index : Positive)
                  return Abydos.Values.Value;
   function Item (Args : Argument_List;
                  Name : String)
                  return Abydos.Values.Value;
   procedure Append (Args  : in out Argument_List;
                     V     : Values.Value);

   procedure Append (Args  : in out Argument_List;
                     Name  : in     String;
                     V     : Values.Value);

   type Evaluable is interface;
   function Evaluate (Item : Evaluable;
                      Args : Argument_List'Class;
                      Env  : Environment'Class)
                      return Values.Value
                      is abstract;

   function Formal_Argument_Count (Item : Evaluable) return Natural
                                   is abstract;

   function Formal_Argument_Name (Item : Evaluable;
                                  Index : Positive)
                                  return String
                                  is abstract;

   procedure Insert (Env   : Environment;
                     Name  : String;
                     Item  : Evaluable'Class);

   function Contains (Env   : Environment;
                      Name  : String)
                     return Boolean;

   function Get (Env  : Environment;
                 Name : String)
                return Abydos.Values.Value;

   function Apply (Env : Environment;
                   Name : String;
                   Args : Argument_List'Class)
                   return Values.Value;

   procedure Update (Env : Environment;
                     Name : String;
                     New_Value : Values.Value);

private

   type Argument is
      record
         Argument_Name  : access String;
         Argument_Value : Values.Value;
      end record;

   package Argument_Vectors is
      new Ada.Containers.Vectors (Positive, Argument);

   type Argument_List is tagged
      record
         List : Argument_Vectors.Vector;
      end record;

   type Environment_Record;
   type Environment_Access is access Environment_Record;

   type Environment is new Kit.Root_Database_Interface with
      record
         Env : Environment_Access;
      end record;

end Abydos.Environments;
