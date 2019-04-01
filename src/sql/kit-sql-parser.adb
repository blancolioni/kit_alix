with Kit.SQL.Tokens;                   use Kit.SQL.Tokens;
with Kit.SQL.Lexical;                  use Kit.SQL.Lexical;

with Kit.SQL.Columns;
with Kit.SQL.Tables;

package body Kit.SQL.Parser is

   Parse_Error : exception;

   procedure Raise_Error (Message : String);

   function At_Column return Boolean;
   function Parse_Column return Kit.SQL.Columns.Column_Element'Class;

   function At_Table return Boolean;
   function Parse_Table return Kit.SQL.Tables.Table_Element'Class;

   ---------------
   -- At_Column --
   ---------------

   function At_Column return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Column;

   --------------
   -- At_Table --
   --------------

   function At_Table return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Table;

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

   -----------------
   -- Parse_Query --
   -----------------

   procedure Parse_Query
     (Query : in out Kit.SQL.Queries.Query_Element'Class)
   is
   begin
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

      if Tok = Tok_Semicolon then
         Scan;
      end if;

      if Tok /= Tok_End_Of_File then
         Error ("extra tokens ignored");
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
