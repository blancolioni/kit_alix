package body Kit.SQL.Columns is

   type All_Columns_Element is new Column_Element with null record;

   overriding function References_Column
     (Element     : All_Columns_Element;
      Table_Name  : String;
      Column_Name : String)
      return Boolean;

   type Named_Column_Element is new Column_Element with
      record
         Table_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Column_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function References_Column
     (Element     : Named_Column_Element;
      Table_Name  : String;
      Column_Name : String)
      return Boolean;

   -----------------
   -- All_Columns --
   -----------------

   function All_Columns return Column_Element'Class is
   begin
      return Result : All_Columns_Element do
         Result.Create;
      end return;
   end All_Columns;

   ------------
   -- Column --
   ------------

   function Column
     (Name : String)
      return Column_Element'Class
   is
   begin
      return Table_Column ("", Name);
   end Column;

   -----------------------
   -- References_Column --
   -----------------------

   overriding function References_Column
     (Element     : All_Columns_Element;
      Table_Name  : String;
      Column_Name : String)
      return Boolean
   is
      pragma Unreferenced (Element, Table_Name, Column_Name);
   begin
      return True;
   end References_Column;

   -----------------------
   -- References_Column --
   -----------------------

   overriding function References_Column
     (Element     : Named_Column_Element;
      Table_Name  : String;
      Column_Name : String)
      return Boolean
   is
   begin
      return (-Element.Table_Name = ""
              or else Same (Element.Table_Name, Table_Name))
        and then Same (Element.Column_Name, Column_Name);
   end References_Column;

   ------------------
   -- Table_Column --
   ------------------

   function Table_Column
     (Table_Name  : String;
      Column_Name : String)
      return Column_Element'Class
   is
   begin
      return Result : Named_Column_Element do
         Result.Create;
         Result.Table_Name := +Table_Name;
         Result.Column_Name := +Column_Name;
      end return;
   end Table_Column;

end Kit.SQL.Columns;
