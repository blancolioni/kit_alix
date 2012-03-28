package Kit.Server.Tables is

   procedure Open_Database (Path : String);

   function Active_Database return Database_Access;

   type Database_Type is
     new Root_Database_Interface with private;

   procedure Initialise (Item : in out Database_Type;
                         Name : String);

   function Name (Db         : Database_Type) return String;

   function Last_Table_Index (Db         : Database_Type)
                              return Marlowe.Table_Index;

   function To_Table_Index (Db         : Database_Type;
                            Table_Name : String)
                           return Marlowe.Table_Index;

   function Get_Table_Name (Db    : Database_Type;
                            Index : Marlowe.Table_Index)
                           return String;

   function Get (Database     : Database_Type;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                 return Database_Record;

   function First_By_Key
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Database_Record;

   function First_By_Key_Value
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Database_Record;

   function Scan_By_Key_Values
     (Tables         : Database_Type;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Database_Record;

private

   type Database_Type is
     new Root_Database_Interface with
      record
         Name : access String;
      end record;

end Kit.Server.Tables;
