with Kit.Db.Database;

with Kit.SQL.Data_Tables;
with Kit.SQL.Queries;
with Kit.SQL.Reports;

procedure Kit.SQL.Driver is
   Query : Kit.SQL.Queries.Query_Element;
   Data  : Kit.SQL.Data_Tables.Data_Table;
begin
   Kit.Db.Database.Open ("config/samples/harriet.db.marlowe");
   Query.Create ("select tag, initial_price, salary from pop_group;");
   Query.Execute (Data);
   Kit.SQL.Reports.Report (Data);
   Query.Create ("select terrain, resource, chance from terrain_resource;");
   Query.Execute (Data);
   Kit.SQL.Reports.Report (Data);
   Kit.Db.Database.Close;
end Kit.SQL.Driver;
