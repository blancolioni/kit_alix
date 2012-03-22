with Marlowe;

package Kit is

   type Search_Interface is limited interface;

   function Has_Element (Item : in     Search_Interface) return Boolean
      is abstract;
   procedure Next (Item : in out Search_Interface)
      is abstract;

   type Root_Database_Record is limited interface and Search_Interface;

   function Index (Item : Root_Database_Record) return Marlowe.Database_Index
                   is abstract;

   function Get (Item       : Root_Database_Record;
                 Field_Name : String)
                return String
      is abstract;

   procedure Set (Item       : in out Root_Database_Record;
                  Field_Name : String;
                  Value      : String)
   is abstract;

   type Database_Record is access all Root_Database_Record'Class;

   procedure Close (Rec : in out Database_Record);

   type Root_Database_Interface is interface;

   type Database_Access is access all Root_Database_Interface'Class;

   function Name (Db : Root_Database_Interface) return String is abstract;

   function To_Table_Index (Db         : Root_Database_Interface;
                            Table_Name : String)
                           return Marlowe.Table_Index
      is abstract;

   function Get (Database     : Root_Database_Interface;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                return Database_Record
      is abstract;

   function First_By_Key
     (Tables       : Root_Database_Interface;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
      return Database_Record
      is abstract;

   function First_By_Key_Value
     (Tables       : Root_Database_Interface;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
      return Database_Record
      is abstract;

   function Scan_By_Key_Values
     (Tables         : Root_Database_Interface;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Database_Record
      is abstract;

   function Get (Database     : Root_Database_Interface'Class;
                 Table_Name   : String;
                 Record_Index : Marlowe.Database_Index)
                 return Database_Record;

   function Scan_By_Key_Values
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Database_Record;

   function Scan_By_Key_Value
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Key_Value      : String)
      return Database_Record;

end Kit;
