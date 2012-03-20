with Abydos.Statements;

package Abydos.Parser is

   Parse_Error : exception;

   procedure Parse_Program (Path : String);

   function Parse_Command (Command : String)
                           return Abydos.Statements.Statement;

end Abydos.Parser;
