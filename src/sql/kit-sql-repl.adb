with Ada.Exceptions;
with Ada.Text_IO;

with Kit.SQL.Tokens;
with Kit.SQL.Lexical;
with Kit.SQL.Parser;

with Kit.SQL.Data_Tables;
with Kit.SQL.Queries;
with Kit.SQL.Reports;

package body Kit.SQL.Repl is

   ----------------
   -- Start_Repl --
   ----------------

   procedure Start_Repl (Script_Path : String := "") is
   begin
      if Script_Path /= "" then
         declare
            use Kit.SQL.Tokens, Kit.SQL.Lexical;
         begin
            Open (Script_Path);
            while Tok /= Tok_End_Of_File loop
               declare
                  Query : Kit.SQL.Queries.Query_Element;
               begin
                  Kit.SQL.Parser.Parse_Query (Query);
                  if not Query.Is_Empty then
                     declare
                        Data : Kit.SQL.Data_Tables.Data_Table;
                     begin
                        Query.Execute (Data);
                        Kit.SQL.Reports.Report (Data);
                     end;
                  end if;
               end;
            end loop;
            Close;
         exception
            when Kit.SQL.Parser.Exit_Command =>
               return;
         end;
      end if;

      loop
         Ada.Text_IO.Put ("SQL> ");
         Ada.Text_IO.Flush;
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
         begin
            if Line = "exit" then
               exit;
            end if;

            declare
               Query : Kit.SQL.Queries.Query_Element;
            begin
               Query.Create (Line);
               if not Query.Is_Empty then
                  declare
                     Data : Kit.SQL.Data_Tables.Data_Table;
                  begin
                     Query.Execute (Data);
                     Kit.SQL.Reports.Report (Data);
                  end;
               end if;
            end;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Exceptions.Exception_Message (E));
         end;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Message (E));

   end Start_Repl;

end Kit.SQL.Repl;
