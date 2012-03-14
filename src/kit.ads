with Marlowe;

package Kit is

   type Search_Interface is limited interface;

   function Has_Element (Item : in     Search_Interface) return Boolean
      is abstract;
   procedure Next (Item : in out Search_Interface)
      is abstract;

   type Root_Database_Record is limited interface and Search_Interface;

   function Get (Item       : Root_Database_Record;
                 Field_Name : String)
                return String
      is abstract;

   procedure Set (Item       : in out Root_Database_Record;
                  Field_Name : String;
                  Value      : String)
      is abstract;

   type Root_Database_Interface is interface;

   function Name (Db : Root_Database_Interface) return String is abstract;

   function To_Table_Index (Db         : Root_Database_Interface;
                            Table_Name : String)
                           return Marlowe.Table_Index
      is abstract;

   function Get (Database     : Root_Database_Interface;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                return Root_Database_Record'Class
      is abstract;

   function First_By_Key
     (Tables       : Root_Database_Interface;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String)
     return Root_Database_Record'Class
      is abstract;

   function First_By_Key_Value
     (Tables       : Root_Database_Interface;
      Table_Index  : Marlowe.Table_Index;
      Key_Name     : String;
      Key_Value    : String)
     return Root_Database_Record'Class
      is abstract;

   function Scan_By_Key_Values
     (Tables         : Root_Database_Interface;
      Table_Index    : Marlowe.Table_Index;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Root_Database_Record'Class
      is abstract;

   function Get (Database     : Root_Database_Interface'Class;
                 Table_Name   : String;
                 Record_Index : Marlowe.Database_Index)
                return Root_Database_Record'Class;

   function Scan_By_Key_Values
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
     return Root_Database_Record'Class;

   function Scan_By_Key_Value
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Key_Value      : String)
     return Root_Database_Record'Class;

end Kit;
