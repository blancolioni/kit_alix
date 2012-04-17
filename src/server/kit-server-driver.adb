with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with Kit.Db.Database;
with Kit.Db.Marlowe_Keys;

with Kit.Paths;

with Kit.Server.SK_Bindings;
with Kit.Server.Tables;

with Hero.Modules;

with Leander.Initialise;
with Leander.Modules;
with Leander.Shell;
with Leander.Target;

--  with Leander.Debug;

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

   Kit.Server.Tables.Open_Database (Argument (1));
   Handle := Kit.Db.Marlowe_Keys.Handle;

   --  Leander.Debug.Enable (Leander.Debug.Case_Values);

   Leander.Initialise (Target);

   SK_Bindings.Create_SK_Bindings;

   Kit_Module :=
     Hero.Modules.Parse_Module
       (Kit.Paths.Config_Path & "/Kit.hs");
   Kit_Module.Compile (Target);

   Leander.Shell.Start_Shell
     (Target,
      Kit.Paths.Config_Path & "/Kit.hs");

   Kit.Db.Database.Close;

end Kit.Server.Driver;
