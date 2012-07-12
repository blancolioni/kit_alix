package Kit.Databases is

   type Root_Database_Interface is interface;

   type Database_Access is access constant Root_Database_Interface'Class;

   function Name (Db : Root_Database_Interface) return String is abstract;

   function To_Table_Index (Db         : Root_Database_Interface;
                            Table_Name : String)
                           return Marlowe.Table_Index
      is abstract;

   function Last_Table_Index (Database : Root_Database_Interface)
                              return Marlowe.Table_Index
                              is abstract;

   type Root_Table_Interface is interface;

   function Table (Database : Root_Database_Interface;
                   Index    : Marlowe.Table_Index)
                   return Root_Table_Interface'Class
                   is abstract;

   function Name (Table : Root_Table_Interface) return String
                  is abstract;

   function Base_Count (Table    : Root_Table_Interface)
                        return Natural
                        is abstract;

   function Base (Table    : Root_Table_Interface;
                  Index    : Positive)
                  return Root_Table_Interface'Class
                  is abstract;

   function Field_Count (Table    : Root_Table_Interface)
                        return Natural
                        is abstract;

   type Root_Field_Interface is interface;

   function Field (Table    : Root_Table_Interface;
                   Index    : Positive)
                   return Root_Field_Interface'Class
                   is abstract;

   function Name (Field : Root_Field_Interface) return String
                  is abstract;

   function Size (Field : Root_Field_Interface) return Natural
                  is abstract;

   function Field_Type (Field : Root_Field_Interface)
                        return String
                        is abstract;

   function Key_Count (Table    : Root_Table_Interface)
                       return Natural
                       is abstract;

   type Root_Key_Interface is interface;

   function Key (Table    : Root_Table_Interface;
                 Index    : Positive)
                 return Root_Key_Interface'Class
                 is abstract;

   function Name (Key : Root_Key_Interface) return String
                  is abstract;

   function Unique (Key : Root_Key_Interface) return Boolean
                    is abstract;

   function Field_Count (Key : Root_Key_Interface)
                         return Positive
                         is abstract;

   function Field (Key : Root_Key_Interface;
                   Index : Positive)
                   return Root_Field_Interface'Class
                   is abstract;

   function Get (Database     : Root_Database_Interface;
                 Table_Index  : Marlowe.Table_Index;
                 Record_Index : Marlowe.Database_Index)
                return Database_Record
      is abstract;

   function First
     (Tables       : Root_Database_Interface;
      Table_Index  : Marlowe.Table_Index)
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

end Kit.Databases;
