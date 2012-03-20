with Ada.Exceptions;
with Ada.Text_IO;

with Abydos.Environments;
with Abydos.Parser;
with Abydos.Statements;
with Abydos.System;
with Abydos.Values;

with Kit.Server.Tables;

package body Kit.Server.Shell is

   -----------------
   -- Start_Shell --
   -----------------

   procedure Start_Shell is
      Env : constant Abydos.Environments.Environment :=
              Abydos.Environments.New_Environment
                (Kit.Server.Tables.Active_Database);
   begin
      Abydos.System.Initialise (Env);

      declare
         Result : constant Abydos.Values.Value :=
                    Env.Apply ("listnames", Abydos.Environments.No_Arguments);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      loop
         Ada.Text_IO.Put (Env.Name & "> ");

         Ada.Text_IO.Flush;
         declare
            Line     : constant String := Ada.Text_IO.Get_Line;
            Command  : constant Abydos.Statements.Statement :=
                         Abydos.Parser.Parse_Command (Line);
         begin
            exit when Line = "exit";
            if Line /= "" then
               begin
                  Abydos.Statements.Execute (Command, Env);
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error =>
         null;
   end Start_Shell;

end Kit.Server.Shell;
