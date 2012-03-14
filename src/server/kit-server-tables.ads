package Kit.Server.Tables is

   type Database_Type is
     new Root_Database_Interface with private;

   type Database_Access is access all Database_Type'Class;

   Active_Database : Database_Access;

   procedure Initialise (Item : in out Database_Type;
                         Name : String);

   function Name (Db         : Database_Type) return String;

   function To_Table_Index (Db         : Database_Type;
                            Table_Name : String)
                           return Marlowe.Table_Index;

   function Get (Database     : Database_Type;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                return Root_Database_Record'Class;

   function First_By_Key
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Root_Database_Record'Class;

   function First_By_Key_Value
     (Tables       : Database_Type;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Root_Database_Record'Class;

   function Scan_By_Key_Values
     (Tables         : Database_Type;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Root_Database_Record'Class;

private

   type Database_Type is
     new Root_Database_Interface with
      record
         Name : access String;
      end record;

end Kit.Server.Tables;
