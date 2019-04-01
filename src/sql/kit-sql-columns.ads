package Kit.SQL.Columns is

   type Column_Element is
     abstract new SQL_Element with private;

   function All_Columns return Column_Element'Class;

   function Column
     (Name : String)
      return Column_Element'Class;

   function Table_Column
     (Table_Name  : String;
      Column_Name : String)
      return Column_Element'Class;

   function References_Column
     (Element     : Column_Element;
      Table_Name  : String;
      Column_Name : String)
      return Boolean
      is abstract;

private

   type Column_Element is
     abstract new SQL_Element with null record;

end Kit.SQL.Columns;
