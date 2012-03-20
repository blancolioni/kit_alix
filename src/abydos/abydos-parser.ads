package Abydos.Parser is

   procedure Parse_Program (Path : String);

   function Parse_Command (Command : String)
                           return Abydos.Statements.Statement;

end Abydos.Parser;
