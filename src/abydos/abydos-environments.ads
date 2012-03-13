with Abydos.Values;
with Kit;
with Marlowe;

package Abydos.Environments is

   type Environment is new Kit.Root_Database_Interface with private;

   function To_Table_Index (Db         : Environment;
                            Table_Name : String)
                           return Marlowe.Table_Index;

   function Get (Database     : Environment;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                return Kit.Root_Database_Record'Class;

   function First_By_Key
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Kit.Root_Database_Record'Class;

   function First_By_Key_Value
     (Tables       : Environment;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Kit.Root_Database_Record'Class;

   function Scan_By_Key_Values
     (Tables         : Environment;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Kit.Root_Database_Record'Class;

   function New_Environment
     (Database    : not null access Kit.Root_Database_Interface'Class;
      Table_Name  : in String;
      Table_Index : in Marlowe.Database_Index)
     return Environment;

   function New_Environment
     (From_Record : Kit.Root_Database_Record'Class)
     return Environment;

   procedure Close (Env : in out Environment);

   procedure Insert (Env   : Environment;
                     Name  : String;
                     Value : Abydos.Values.Value);

   function Contains (Env   : Environment;
                      Name  : String)
                     return Boolean;

   function Get (Env  : Environment;
                 Name : String)
                return Abydos.Values.Value;

   function Apply (Env : Environment;
                   Name : String;
                   Args : Values.Array_Of_Values)
                  return Values.Value;

private

   type Environment is new Kit.Root_Database_Interface with null record;

end Abydos.Environments;
