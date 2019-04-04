with Ada.Directories;

with WL.Command_Line;

with Kit.Db.Database;

with Kit.SQL.Options;
with Kit.SQL.Repl;

procedure Kit.SQL.Driver is
begin
   if Ada.Directories.Exists (".kitsqlrc") then
      WL.Command_Line.Load_Defaults (".kitsqlrc");
   end if;

   if Kit.SQL.Options.Database /= "" then
      Kit.Db.Database.Open (Kit.SQL.Options.Database);
   end if;

   Kit.SQL.Repl.Start_Repl
     (Kit.SQL.Options.SQL_Script);

   Kit.Db.Database.Close;
end Kit.SQL.Driver;
