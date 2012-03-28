with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with Kit.Db.Database;
with Kit.Db.Kit_Record;
with Kit.Db.Marlowe_Keys;

with Kit.Paths;

with Kit.Server.Database;
with Kit.Server.Shell;
with Kit.Server.SK_Bindings;
with Kit.Server.System;
with Kit.Server.Tables;

with Hero.Modules;

with Leander.Initialise;
with Leander.Modules;
with Leander.Shell;
with Leander.Target;

procedure Kit.Server.Driver is

   Use_Leander : constant Boolean := True;

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

   Kit.Server.System.Add_System_Commands;

   if False then
      declare
         Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
           Kit.Db.Kit_Record.First_By_Name;
      begin
         while Rec.Has_Element loop
            Ada.Text_IO.Put_Line
              (Rec.Name & " "
                 & Kit.Db.Record_Type'Image (Rec.Top_Record));
            Rec.Next;
         end loop;
      end;
   end if;

   if False then
      declare
         Rec : Kit.Server.Database.Database_Record;
      begin
         Rec.Get (20, 1);
         Rec.Report;
      end;
   end if;

   if False then
      Kit.Server.Database.Report (21);
   end if;

   if Use_Leander then

      Leander.Initialise (Target);
      SK_Bindings.Create_SK_Bindings
        (Leander.Target.Machine (Target));

      Kit_Module :=
        Hero.Modules.Parse_Module
          (Kit.Paths.Config_Path & "/Kit.hs");
      Kit_Module.Compile (Target);

      Leander.Shell.Start_Shell
        (Target,
         Kit.Paths.Config_Path & "/Kit.hs");

   else
      Kit.Server.Shell.Start_Shell;
   end if;

   Kit.Db.Database.Close;

end Kit.Server.Driver;

