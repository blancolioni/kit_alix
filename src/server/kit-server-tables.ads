with Kit.Databases;

package Kit.Server.Tables is

   procedure Open_Database (Path : String);

   function Active_Database return Kit.Databases.Database_Access;

   type Database_Type is
     new Kit.Databases.Root_Database_Interface with private;

   procedure Initialise (Item : in out Database_Type;
                         Name : String);

   overriding
   function Name (Db : Database_Type) return String;

   overriding
   function Last_Table_Index (Db         : Database_Type)
                              return Marlowe.Table_Index;

   overriding
   function To_Table_Index (Db         : Database_Type;
                            Table_Name : String)
                           return Marlowe.Table_Index;

   overriding
   function Table (Db         : Database_Type;
                   Index      : Marlowe.Table_Index)
                   return Kit.Databases.Root_Table_Interface'Class;

   function Get_Table_Name (Db    : Database_Type;
                            Index : Marlowe.Table_Index)
                           return String;

   overriding
   function Get (Database     : Database_Type;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                 return Database_Record;

   overriding
   function First_By_Key
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Database_Record;

   overriding
   function First_By_Key_Value
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Database_Record;

   overriding
   function Scan_By_Key_Values
     (Tables         : Database_Type;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Database_Record;

private

   type Database_Type is
     new Kit.Databases.Root_Database_Interface with
      record
         Name : access String;
      end record;

end Kit.Server.Tables;
