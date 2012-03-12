with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;

with Kit.Db.Database;
with Kit.Db.Kit_Record;
with Kit.Db.Marlowe_Keys;

with Kit.Bindings;
with Kit.Paths;

with Kit.Server.Database;
with Kit.Server.Shell;
with Kit.Server.System;

with Hero.Modules;

with Leander.Initialise;
with Leander.Modules;
with Leander.Shell;
with Leander.Target;

procedure Kit.Server.Driver is

   Use_Leander : constant Boolean := False;

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

      Leander.Shell.Start_Shell
        (Target,
         Kit.Paths.Config_Path & "/Kit.hs");

   else

      Kit.Server.Shell.Start_Shell
        (Ada.Directories.Base_Name (Argument (1)));

   end if;

   Kit.Db.Database.Close;

end Kit.Server.Driver;

