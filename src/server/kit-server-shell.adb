with Ada.Exceptions;
with Ada.Text_IO;

with Kit.Server.Commands;

package body Kit.Server.Shell is

   -----------------
   -- Start_Shell --
   -----------------

   procedure Start_Shell (Prompt : String) is
   begin
      loop
         Ada.Text_IO.Put (Prompt & "> ");
         Ada.Text_IO.Flush;
         declare
            Line     : constant String := Ada.Text_IO.Get_Line;
            Response : Kit.Server.Commands.Command_Response;
         begin
            exit when Line = "exit";
            if Line /= "" then
               begin
                  Kit.Server.Commands.Execute_Command (Line, Response);
               exception
                  when E : others =>
                     Response.Set_Error
                       (Ada.Exceptions.Exception_Message (E));
               end;

               Response.Write;
            end if;
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error =>
         null;
   end Start_Shell;

end Kit.Server.Shell;
