with GCS.Positions;

with Kit.Parser.Tokens;                use Kit.Parser.Tokens;
with Kit.Parser.Lexical;               use Kit.Parser.Lexical;

with Kit.Tables;
with Kit.Fields;
with Kit.Types;

package body Kit.Parser is

   procedure Read_Package (Db : in out Kit.Databases.Database_Type);

   procedure Parse_Record (Db : in out Kit.Databases.Database_Type)
     with Pre => Tok = Tok_Record;

   procedure Parse_Bases (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type);

   function At_Field return Boolean;
   procedure Parse_Field (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type);

   function At_Type return Boolean;
   function Parse_Type
     (Db : Kit.Databases.Database_Type)
      return Kit.Types.Kit_Type'Class;

   function Parse_Qualified_Identifier return String;

   --------------
   -- At_Field --
   --------------

   function At_Field return Boolean is
   begin
      return Tok = Tok_Identifier
        or else Tok = Tok_Key
        or else Tok = Tok_Compound
        or else Tok = Tok_Unique;
   end At_Field;

   -------------
   -- At_Type --
   -------------

   function At_Type return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Type;

   -----------------
   -- Parse_Bases --
   -----------------

   procedure Parse_Bases (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type)
   is
      use Set_Of_Tokens;
   begin
      while Tok = Tok_Identifier loop
         if not Db.Contains (Tok_Text) then
            Error (Tok_Raw_Text & ": unknown record");
         else
            Table.Add_Base (Db.Element (Tok_Text));
         end if;
         Scan;
         exit when Tok /= Tok_Comma;
         Scan;
         if Tok /= Tok_Identifier then
            Error ("missing base record name");
            Skip_To (+(Tok_Identifier, Tok_Is));
         end if;
      end loop;
   end Parse_Bases;

   -----------------
   -- Parse_Field --
   -----------------

   procedure Parse_Field (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type)
   is
      use Set_Of_Tokens;
      Is_Key    : Boolean := False;
      Is_Unique : Boolean := False;
   begin
      if Tok = Tok_Unique then
         Is_Key := True;
         Is_Unique := True;
         Scan;
         if Tok = Tok_Key then
            Scan;
         else
            Error ("missing 'key'");
         end if;
      elsif Tok = Tok_Key then
         Is_Key := True;
         Scan;
      end if;

      if Tok /= Tok_Identifier then
         Error ("missing field name");
         Skip_To (+(Tok_Semi, Tok_End));
         if Tok = Tok_Semi then
            Scan;
         end if;
         return;
      end if;

      declare
         Field_Name : constant String := Tok_Text;
      begin
         Scan;

         if Tok /= Tok_Colon then

            if Db.Contains (Field_Name) then

               declare
                  Field_Type : constant Kit.Types.Kit_Type'Class :=
                                 Kit.Types.Table_Reference_Type
                                   (Field_Name);
                  Field      : Kit.Fields.Field_Type;
               begin
                  Field.Create_Field (Field_Name, Field_Type);
                  Table.Append (Item      => Field,
                                Is_Key    => Is_Key,
                                Is_Unique => Is_Unique);
               end;
            else
               Error ("unknown table");
            end if;
         else

            Scan;

            if not At_Type then
               Error ("missing field type");
               Skip_To (+(Tok_Semi, Tok_End));
               if Tok = Tok_Semi then
                  Scan;
               end if;
               return;
            end if;

            declare
               Field_Type : constant Kit.Types.Kit_Type'Class :=
                              Parse_Type (Db);
               Field      : Kit.Fields.Field_Type;
            begin
               Field.Create_Field (Field_Name, Field_Type);
               Table.Append (Item      => Field,
                             Is_Key    => Is_Key,
                             Is_Unique => Is_Unique);
            end;
         end if;

      end;

      if Tok = Tok_Semi then
         Scan;
      else
         Error ("missing ';'");
      end if;

   end Parse_Field;

   --------------------------------
   -- Parse_Qualified_Identifier --
   --------------------------------

   function Parse_Qualified_Identifier return String is
      Result : constant String := Tok_Text;
   begin
      Scan;
      if Tok = Tok_Dot then
         Scan;
         return Result & "." & Parse_Qualified_Identifier;
      else
         return Result;
      end if;
   end Parse_Qualified_Identifier;

   ------------------
   -- Parse_Record --
   ------------------

   procedure Parse_Record (Db : in out Kit.Databases.Database_Type) is
      use Set_Of_Tokens;
   begin
      Scan;   --  Tok_Record

      if Tok /= Tok_Identifier then
         Error ("expected record name");
         Skip_To (+Tok_End);
      else
         declare
            Record_Name : constant String := Tok_Text;
            Table       : Kit.Tables.Table_Type;
         begin
            Table.Create (Record_Name);
            Scan;

            if Tok = Tok_Colon then
               Scan;
               Parse_Bases (Db, Table);
            end if;

            if Tok = Tok_Is then
               Scan;
               while At_Field loop
                  Parse_Field (Db, Table);
               end loop;

               Expect (Tok_End, +Tok_End);

               if Tok = Tok_End then
                  Scan;
               end if;

               if Tok = Tok_Identifier then
                  if Tok_Text /= Record_Name then
                     Error ("Expected " & Record_Name);
                  end if;
                  Scan;
               end if;
            end if;

            if Tok = Tok_Semi then
               Scan;
            else
               Error ("missing ';'");
            end if;

            Db.Append (Table);

         end;
      end if;

   end Parse_Record;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Db : Kit.Databases.Database_Type)
      return Kit.Types.Kit_Type'Class
   is
      use Set_Of_Tokens;
      Name     : constant String := Tok_Text;
      Raw_Name : constant String := Tok_Raw_Text;
      Location : constant GCS.Positions.File_Position :=
                   Get_Current_Position;
   begin
      pragma Assert (At_Type);
      Scan;
      if Name = "positive" then
         return Kit.Types.Standard_Positive;
      elsif Name = "natural" then
         return Kit.Types.Standard_Natural;
      elsif Name = "integer" then
         return Kit.Types.Standard_Integer;
      elsif Name = "string" then
         if Tok /= Tok_Left_Paren
           or else Next_Tok /= Tok_Integer_Constant
           or else Next_Tok (2) /= Tok_Right_Paren
         then
            Error ("missing constraint");
            Skip_To (+(Tok_Semi, Tok_End));
            return Kit.Types.Standard_String (32);
         end if;
         Scan;
         declare
            Length : constant Natural := Natural'Value (Tok_Text);
         begin
            Scan;
            Scan;
            return Kit.Types.Standard_String (Length);
         end;
      elsif Db.Contains (Name) then
         return Kit.Types.Table_Reference_Type (Name);
      else
         Error (Location, Raw_Name & ": no such type or record name");
         return Kit.Types.Standard_Integer;
      end if;
   end Parse_Type;

   -------------------
   -- Read_Kit_File --
   -------------------

   procedure Read_Kit_File
     (Path : String;
      Db   : out Kit.Databases.Database_Type)
   is
   begin
      Open (Path);

      Read_Package (Db);

      Close;
   end Read_Kit_File;

   ------------------
   -- Read_Package --
   ------------------

   procedure Read_Package (Db : in out Kit.Databases.Database_Type) is
      use Set_Of_Tokens;
   begin
      if Tok /= Tok_Package then
         Error ("missing package");
      end if;

      Scan;

      declare
         Package_Name : constant String := Parse_Qualified_Identifier;
      begin
         Expect (Tok_Is, +(Tok_Record, Tok_End));

         Db.Create_Database (Package_Name);

         while Tok = Tok_Record loop
            Parse_Record (Db);
         end loop;

         Expect (Tok_End, +Tok_End);

         if Tok = Tok_End then
            Scan;
         end if;

         if Tok = Tok_Identifier then
            declare
               Location     : constant GCS.Positions.File_Position :=
                                Get_Current_Position;
               Closing_Name : constant String := Parse_Qualified_Identifier;
            begin
               if Closing_Name /= Package_Name then
                  Error (Location, "expected " & Package_Name);
               end if;
            end;
         end if;

         if Tok = Tok_Semi then
            Scan;
         else
            Error ("missing ';'");
         end if;

         if Tok /= Tok_End_Of_File then
            Error ("extra tokens ignored");
         end if;
      end;
   end Read_Package;

end Kit.Parser;
