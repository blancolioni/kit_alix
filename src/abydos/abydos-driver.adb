--  with Ada.Text_IO;

with Abydos.Environments;
pragma Unreferenced (Abydos.Environments);

with Abydos.Programs;
with Abydos.Statements;
with Abydos.Expressions;
with Abydos.Values;

procedure Abydos.Driver is
   use Abydos.Statements, Abydos.Expressions, Abydos.Values;
   Args : Values.Array_Of_Values (1 .. 0);
begin

   Abydos.Programs.Update_Program
     ("test", Args,
      Assign
        (Object ("Result"),
         Function_Call
           ("+",
            (To_Expression (To_Value (1)),
             To_Expression (To_Value (2))))));

   --  Abydos.Parser.Parse_Program ("test.abydos");

   --  declare
   --     V : constant Abydos.Values.Value :=
   --       Abydos.Programs.Execute
   --       ("test", Args,
   --        Abydos.Environments.New_Environment ("test"));
   --  begin
   --     Ada.Text_IO.Put_Line ("Result = " & Abydos.Values.To_String (V));
   --  end;
end Abydos.Driver;


