package Kit.SQL.Tables is

   type Table_Element is
     new SQL_Element with private;

   function Table_Name
     (Element : Table_Element)
      return String;

   function Table
     (Name : String)
      return Table_Element'Class;

private

   type Table_Element is
     new SQL_Element with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Table_Name
     (Element : Table_Element)
      return String
   is (-Element.Name);

end Kit.SQL.Tables;
