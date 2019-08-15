with Kit.SQL.Tokens;                   use Kit.SQL.Tokens;
with Kit.SQL.Lexical;                  use Kit.SQL.Lexical;

with Kit.SQL.Columns;
with Kit.SQL.Expressions;
with Kit.SQL.Tables;

package body Kit.SQL.Parser is

   Parse_Error : exception;

   procedure Raise_Error (Message : String);

   function At_Column return Boolean;
   function Parse_Column return Kit.SQL.Columns.Column_Element'Class;

   function At_Table return Boolean;
   function Parse_Table return Kit.SQL.Tables.Table_Element'Class;

   function At_Expression return Boolean;
   function Parse_Expression
     return Kit.SQL.Expressions.Expression_Element'Class;

   function Operator return Kit.SQL.Expressions.Operator_Type;

   ---------------
   -- At_Column --
   ---------------

   function At_Column return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Column;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
      use Set_Of_Tokens;
   begin
      return Tok <= +(Tok_Left_Paren, Tok_Identifier, Tok_String_Constant,
                      Tok_Integer_Constant, Tok_Float_Constant);
   end At_Expression;

   --------------
   -- At_Table --
   --------------

   function At_Table return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Table;

   --------------
   -- Operator --
   --------------

   function Operator return Kit.SQL.Expressions.Operator_Type is
      use all type Kit.SQL.Expressions.Operator_Type;
   begin
      case Tok is
         when Tok_EQ =>
            return Op_EQ;
         when Tok_NE =>
            return Op_NE;
         when Tok_LT =>
            return Op_LT;
         when Tok_GT =>
            return Op_GT;
         when Tok_LE =>
            return Op_LE;
         when Tok_GE =>
            return Op_GE;
         when others =>
            raise Constraint_Error with
              "not an operator: " & Tok'Image;
      end case;
   end Operator;

   ------------------
   -- Parse_Column --
   ------------------

   function Parse_Column return Kit.SQL.Columns.Column_Element'Class is
      pragma Assert (At_Column);
      Name : constant String := Tok_Text;
   begin
      Scan;
      if Tok = Tok_Dot then
         Scan;
         if Tok /= Tok_Identifier then
            Raise_Error ("missing field name");
         end if;

         declare
            Field_Name : constant String := Tok_Text;
         begin
            Scan;
            return Kit.SQL.Columns.Table_Column (Name, Field_Name);
         end;
      else
         return Kit.SQL.Columns.Column (Name);
      end if;
   end Parse_Column;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     return Kit.SQL.Expressions.Expression_Element'Class
   is
      use Kit.SQL.Expressions;

      function Parse_And_Expression return Expression_Element'Class;
      function Parse_Relational_Expression return Expression_Element'Class;
      function Parse_Atomic_Expression return Expression_Element'Class;

      --------------------------
      -- Parse_And_Expression --
      --------------------------

      function Parse_And_Expression return Expression_Element'Class is
         Left : constant Expression_Element'Class :=
                  Parse_Relational_Expression;
      begin
         if Tok = Tok_And then
            declare
               Arguments : Expression_List;
            begin
               Arguments.Append (Left);

               while Tok = Tok_And loop
                  Scan;
                  declare
                     Right : constant Expression_Element'Class :=
                               Parse_Relational_Expression;
                  begin
                     Arguments.Append (Right);
                  end;
               end loop;

               return And_Expression (Arguments);
            end;
         else
            return Left;
         end if;
      end Parse_And_Expression;

      -----------------------------
      -- Parse_Atomic_Expression --
      -----------------------------

      function Parse_Atomic_Expression return Expression_Element'Class is
      begin
         case Tok is
            when Tok_Left_Paren =>
               Scan;
               declare
                  Result : constant Expression_Element'Class :=
                             Parse_Expression;
               begin
                  if Tok = Tok_Right_Paren then
                     Scan;
                  else
                     Raise_Error ("missing ')'");
                  end if;
                  return Result;
               end;
            when Tok_Integer_Constant =>
               return Expression : constant Expression_Element'Class :=
                 Integer_Expression (Integer'Value (Tok_Text))
               do
                  Scan;
               end return;
            when Tok_Float_Constant =>
               return Expression : constant Expression_Element'Class :=
                 Float_Expression (Float'Value (Tok_Text))
               do
                  Scan;
               end return;
            when Tok_String_Constant =>
               return Expression : constant Expression_Element'Class :=
                 String_Expression (Tok_Text)
               do
                  Scan;
               end return;
            when Tok_Character_Constant =>
               return Expression : constant Expression_Element'Class :=
                 String_Expression (Tok_Text)
               do
                  Scan;
               end return;
            when Tok_Identifier =>
               return Expression : constant Expression_Element'Class :=
                 Identifier_Expression (Tok_Text)
               do
                  Scan;
               end return;
            when others =>
               Raise_Error ("expected an expression");
               return Integer_Expression (0);
         end case;
      end Parse_Atomic_Expression;

      ---------------------------------
      -- Parse_Relational_Expression --
      ---------------------------------

      function Parse_Relational_Expression return Expression_Element'Class is
         use type Set_Of_Tokens.Set;
         Ops  : constant Set_Of_Tokens.Set :=
                  +(Tok_EQ, Tok_NE, Tok_LT, Tok_LE, Tok_GT, Tok_GE);
         Left : constant Expression_Element'Class :=
                  Parse_Atomic_Expression;
      begin
         if Tok <= Ops then
            declare
               Op        : constant Operator_Type := Operator;
            begin
               Scan;
               return Operator_Expression (Op, Left, Parse_Atomic_Expression);
            end;
         elsif Tok = Tok_Between then
            Scan;
            declare
               Low : constant Expression_Element'Class :=
                       Parse_Atomic_Expression;
            begin
               if Tok = Tok_And then
                  Scan;
                  declare
                     High : constant Expression_Element'Class :=
                              Parse_Atomic_Expression;
                     Args : Expression_List;
                  begin
                     Args.Append (Operator_Expression (Op_GE, Left, Low));
                     Args.Append (Operator_Expression (Op_LE, Left, High));
                     return And_Expression (Args);
                  end;
               else
                  Raise_Error ("expected 'and'");
                  return Left;
               end if;
            end;
         else
            return Left;
         end if;
      end Parse_Relational_Expression;

      Left : constant Expression_Element'Class :=
               Parse_And_Expression;
   begin
      if Tok = Tok_Or then
         declare
            Arguments : Expression_List;
         begin
            Arguments.Append (Left);

            while Tok = Tok_Or loop
               Scan;
               declare
                  Right : constant Expression_Element'Class :=
                            Parse_And_Expression;
               begin
                  Arguments.Append (Right);
               end;
            end loop;

            return Function_Call_Expression ("or", Arguments);
         end;
      else
         return Left;
      end if;
   end Parse_Expression;

   -----------------
   -- Parse_Query --
   -----------------

   procedure Parse_Query
     (Query : in out Kit.SQL.Queries.Query_Element'Class)
   is
   begin
      if Tok = Tok_Exit then
         raise Exit_Command;
      end if;

      if Tok /= Tok_Select then
         Raise_Error ("expected 'select'");
      end if;

      Scan;

      if Tok = Tok_Asterisk then
         Query.Add_Column (Kit.SQL.Columns.All_Columns);
         Scan;
      else
         while At_Column loop
            Query.Add_Column (Parse_Column);
            if Tok = Tok_Comma then
               Scan;
               if not At_Column then
                  if Tok = Tok_Where
                    or else Tok = Tok_From
                  then
                     Error ("extra ',' ignored");
                  else
                     Raise_Error ("syntax error");
                  end if;
               end if;
            elsif At_Column then
               Error ("missing ','");
            end if;
         end loop;
      end if;

      if Tok /= Tok_From then
         Raise_Error ("expected 'from'");
      end if;

      Scan;

      if not At_Table then
         Raise_Error ("expected a table name");
      end if;

      Query.Add_Table (Parse_Table);

      if Tok = Tok_Where then
         Scan;
         if not At_Expression then
            Raise_Error ("expected an expression");
         end if;
         Query.Set_Predicate (Parse_Expression);
      end if;

      if Tok = Tok_Semicolon then
         Scan;
      end if;

   exception
      when Parse_Error =>
         Query.Clear;

   end Parse_Query;

   -----------------
   -- Parse_Table --
   -----------------

   function Parse_Table return Kit.SQL.Tables.Table_Element'Class is
      pragma Assert (At_Table);
      Name : constant String := Tok_Text;
   begin
      Scan;
      return Kit.SQL.Tables.Table (Name);
   end Parse_Table;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String) is
   begin
      Error (Message);
      raise Parse_Error with Message;
   end Raise_Error;

end Kit.SQL.Parser;
