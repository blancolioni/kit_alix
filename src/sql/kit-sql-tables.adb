package body Kit.SQL.Tables is

   -----------
   -- Table --
   -----------

   function Table
     (Name : String)
      return Table_Element'Class
   is
   begin
      return Result : Table_Element do
         Result.Create;
         Result.Name := +Name;
      end return;
   end Table;

end Kit.SQL.Tables;
