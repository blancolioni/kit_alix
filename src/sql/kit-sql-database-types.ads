with Kit.Db;

private package Kit.SQL.Database.Types is

   function To_Data_Type
     (From : Kit.Db.Kit_Type_Reference)
      return Data_Type'Class;

end Kit.SQL.Database.Types;
