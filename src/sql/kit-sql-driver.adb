with Kit.Db.Database;

with Kit.SQL.Repl;

procedure Kit.SQL.Driver is
begin
   Kit.Db.Database.Open ("config/samples/harriet.db.marlowe");
   Kit.SQL.Repl.Start_Repl;
   Kit.Db.Database.Close;
end Kit.SQL.Driver;
