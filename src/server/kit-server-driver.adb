with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with Kit.Db.Database;

with Kit.Server.Web_Server;

procedure Kit.Server.Driver is
begin

   if Argument_Count /= 1 then
      Put_Line (Standard_Error,
                "Usage: kit-server <path to database>");
      Set_Exit_Status (1);
      return;
   end if;

   Kit.Db.Database.Open (Argument (1));

   Kit.Server.Web_Server.Start_Web_Server;

   Kit.Db.Database.Close;

end Kit.Server.Driver;
