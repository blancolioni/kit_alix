with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with Kit.Db.Database;

with Kit.Bindings;
with Kit.Paths;

with Hero.Modules;

with Leander.Initialise;
with Leander.Modules;
with Leander.Shell;
with Leander.Target;

procedure Kit.Server.Driver is

   Target : Leander.Target.Leander_Target;
   Kit_Module : Leander.Modules.Leander_Module;

begin

   if Argument_Count /= 1 then
      Put_Line (Standard_Error,
                "Usage: kit-server <path to database>");
      Set_Exit_Status (1);
      return;
   end if;

   Leander.Initialise (Target);
   Kit.Bindings.Create_Kit_Bindings
     (Leander.Target.Machine (Target));

   Kit_Module :=
     Hero.Modules.Parse_Module
       (Kit.Paths.Config_Path & "/Kit.hs");
   Kit_Module.Compile (Target);

   Kit.Db.Database.Open (Argument (1));

   Leander.Shell.Start_Shell
     (Target,
      Kit.Paths.Config_Path & "/Kit.hs");

   Kit.Db.Database.Close;

end Kit.Server.Driver;

