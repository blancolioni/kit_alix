with Ada.Command_Line;

with Lith.Library;

with Kit.Db.Database;

with Kit.Server.Lith_Bindings;

with Kit.Paths;

procedure Kit.Lith_Shell is
begin

   Lith.Library.Initialise;

   Kit.Server.Lith_Bindings.Create_Bindings;

   Lith.Library.Load
     (Kit.Paths.Config_Path
      & "/kit.scm");

   if Ada.Command_Line.Argument_Count = 1 then
      Kit.Db.Database.Open
        (Ada.Command_Line.Argument (1));
   end if;

   Lith.Library.Start_Repl;

   if Ada.Command_Line.Argument_Count = 1 then
      Kit.Db.Database.Close;
   end if;

end Kit.Lith_Shell;
