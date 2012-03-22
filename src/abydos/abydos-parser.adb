with Abydos.Parser.Tokens;             use Abydos.Parser.Tokens;
with Abydos.Parser.Lexical;            use Abydos.Parser.Lexical;

with Abydos.Expressions;
with Abydos.Values;

package body Abydos.Parser is

   function Parse_Statement return Abydos.Statements.Statement;

   function At_Expression return Boolean;
   function Parse_Expression return Abydos.Expressions.Expression;
   function Parse_Atomic_Expression return Abydos.Expressions.Expression;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
   begin
      return Tok = Tok_Identifier or else Tok = Tok_Integer_Constant
        or else Tok = Tok_String_Constant;
   end At_Expression;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression return Abydos.Expressions.Expression is
      use Abydos.Expressions;
      use Abydos.Values;
   begin
      case Tok is
         when Tok_Identifier =>
            declare
               Id   : constant String := Tok_Text;
               Args : Array_Of_Expressions (1 .. 0);
            begin
               Scan;
               return Function_Call (Id, Args);
            end;
         when Tok_String_Constant =>
            declare
               Id   : constant String := Tok_Text;
            begin
               Scan;
               return To_Expression (To_Value (Id));
            end;
         when Tok_Integer_Constant =>
            declare
               Result : constant Integer := Integer'Value (Tok_Text);
            begin
               Scan;
               return To_Expression (To_Value (Result));
            end;
         when others =>
            raise Parse_Error with "expected expression";
      end case;
   end Parse_Atomic_Expression;

   -------------------
   -- Parse_Command --
   -------------------

   function Parse_Command
     (Command : String)
      return Abydos.Statements.Statement
   is
   begin
      Open_String (Command);

      declare
         S : constant Abydos.Statements.Statement := Parse_Statement;
      begin
         Close;
         return S;
      end;
   end Parse_Command;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Abydos.Expressions.Expression is
      use Abydos.Expressions;
   begin
      if Tok = Tok_Identifier then
         declare
            Id    : constant String := Tok_Raw_Text;
            Args  : Array_Of_Expressions (1 .. 20);
            Count : Natural := 0;
         begin
            Scan;
            while At_Expression loop
               Count := Count + 1;
               Args (Count) := Parse_Atomic_Expression;
            end loop;
            return Function_Call (Id, Args (1 .. Count));
         end;
      else
         raise Parse_Error with "expression expected";
      end if;
   end Parse_Expression;

   -------------------
   -- Parse_Program --
   -------------------

   procedure Parse_Program (Path : String) is
      pragma Unreferenced (Path);
   begin
      null;
   end Parse_Program;

   ---------------------
   -- Parse_Statement --
   ---------------------

   function Parse_Statement return Abydos.Statements.Statement is
   begin
      if At_Expression then
         declare
            E : constant Abydos.Expressions.Expression := Parse_Expression;
         begin
            return Abydos.Statements.Expression_Statement (E);
         end;
      else
         raise Parse_Error with "syntax error";
      end if;
   end Parse_Statement;

end Abydos.Parser;
