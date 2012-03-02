with GCS.Positions;

with Kit.Parser.Tokens;                use Kit.Parser.Tokens;
with Kit.Parser.Lexical;               use Kit.Parser.Lexical;
with Kit.Paths;

with Kit.Tables;
with Kit.Fields;
with Kit.Types;
with Kit.Types.Enumerated;

package body Kit.Parser is

   System_Db : Kit.Databases.Database_Type;
   Got_System_Db : Boolean := False;

   procedure Read_Package (Db          : in out Kit.Databases.Database_Type;
                           With_System : Boolean := True);

   function At_Declaration return Boolean;

   procedure Parse_Record (Db : in out Kit.Databases.Database_Type)
     with Pre => Tok = Tok_Record;

   procedure Parse_Type_Declaration (Db : in out Kit.Databases.Database_Type)
     with Pre => Tok = Tok_Type;

   procedure Parse_Bases (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type);

   function At_Field return Boolean;
   procedure Parse_Field (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type);

   function At_Type return Boolean;
   function Parse_Type
     (Db           : Kit.Databases.Database_Type;
      Table_Name   : String;
      Context_Name : String)
      return Kit.Types.Kit_Type'Class;

   function Parse_Qualified_Identifier return String;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
   begin
      return Tok = Tok_Record or else Tok = Tok_Type;
   end At_Declaration;

   --------------
   -- At_Field --
   --------------

   function At_Field return Boolean is
   begin
      return Tok = Tok_Identifier
        or else Tok = Tok_Key
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
            Skip_To ((Tok_Identifier, Tok_Is));
         end if;
      end loop;
   end Parse_Bases;

   -----------------
   -- Parse_Field --
   -----------------

   procedure Parse_Field (Db    : Kit.Databases.Database_Type;
                          Table : in out Kit.Tables.Table_Type)
   is
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
         Skip_To ((Tok_Semi, Tok_End));
         if Tok = Tok_Semi then
            Scan;
         end if;
         return;
      end if;

      declare
         Field_Name : constant String := Tok_Text;
      begin
         Scan;

         if Tok = Tok_Is then
            Scan;

            declare
               Field : Kit.Fields.Compound_Field_Type;
            begin
               Field.Create_Field (Field_Name);
               loop
                  if Table.Contains_Field (Tok_Text) then
                     Table.Add_Compound_Key_Field
                       (Field, Tok_Text);
                  else
                     Error ("table " & Table.Ada_Name
                            & " does not contain field "
                            & Tok_Raw_Text);
                  end if;
                  Scan;
                  exit when Tok /= Tok_Comma;
                  Scan;
                  if Tok /= Tok_Identifier then
                     Error ("extra ',' ignored");
                     exit;
                  end if;
               end loop;

               Table.Append (Field, Is_Unique);
            end;

         elsif Tok /= Tok_Colon then

            if Field_Name = Table.Name
              or else Db.Contains (Field_Name)
            then

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
               Skip_To (Tok_Semi, Tok_End);
               if Tok = Tok_Semi then
                  Scan;
               end if;
               return;
            end if;

            declare
               Field_Type : constant Kit.Types.Kit_Type'Class :=
                              Parse_Type (Db, Table.Name, Field_Name);
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
   begin
      Scan;   --  Tok_Record

      if Tok /= Tok_Identifier then
         Error ("expected record name");
         Skip_To (Tok_End);
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

               Expect (Tok_End, Tok_End);

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
     (Db           : Kit.Databases.Database_Type;
      Table_Name   : String;
      Context_Name : String)
      return Kit.Types.Kit_Type'Class
   is
      Location : constant GCS.Positions.File_Position :=
                   Get_Current_Position;
   begin
      pragma Assert (At_Type);

      if Tok = Tok_Identifier then
         declare
            Name     : constant String := Tok_Text;
            Raw_Name : constant String := Tok_Raw_Text;
         begin
            Scan;

            if Kit.Types.Is_Type_Name (Name) then
               return Kit.Types.Get_Type (Name);
            elsif Name = "string" then
               if Tok /= Tok_Left_Paren
                 or else Next_Tok /= Tok_Integer_Constant
                 or else Next_Tok (2) /= Tok_Right_Paren
               then
                  Error ("missing constraint");
                  Skip_To (Tok_Semi, Tok_End);
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
            elsif Name = Table_Name or else Db.Contains (Name) then
               return Kit.Types.Table_Reference_Type (Name);
            else
               Error (Location, Raw_Name & ": no such type or record name");
               return Kit.Types.Standard_Integer;
            end if;
         end;
      elsif Tok = Tok_Left_Paren then
         declare
            Result : Kit.Types.Enumerated.Enumerated_Type;
         begin
            Result.Create (Context_Name);
            Scan;
            loop
               if Tok = Tok_Identifier then
                  Result.Add_Literal (Tok_Text);
                  Scan;
                  exit when Tok /= Tok_Comma;
                  Scan;
               else
                  Error ("missing enumerator literal");
                  Skip_To (Tok_Right_Paren, Tok_Semi, Tok_End);
                  exit;
               end if;
            end loop;

            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;

            return Result;
         end;
      else
         raise Program_Error with "expected to be at a type";
      end if;

   end Parse_Type;

   ----------------------------
   -- Parse_Type_Declaration --
   ----------------------------

   procedure Parse_Type_Declaration
     (Db : in out Kit.Databases.Database_Type)
   is
   begin
      Scan;  --  Tok_Type

      if Tok /= Tok_Identifier then
         Error ("expected a type name");
         Skip_To (Skip_To_And_Parse => (1 => Tok_Semi),
                  Skip_To_And_Stop  =>  (1 => Tok_End));
         return;
      end if;

      declare
         Name : constant String := Tok_Text;
      begin
         Scan;
         if Tok /= Tok_Is then
            Error ("missing 'is'");
            Skip_To (Skip_To_And_Parse => (1 => Tok_Semi),
                     Skip_To_And_Stop  =>  (1 => Tok_End));
            return;
         end if;

         Scan;

         declare
            New_Type : constant Kit.Types.Kit_Type'Class :=
                         Parse_Type (Db, "", Name);
         begin
            Kit.Types.New_Type (New_Type);
         end;

         if Tok = Tok_Semi then
            Scan;
         else
            Error ("missing ';'");
         end if;
      end;


   end Parse_Type_Declaration;

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

   procedure Read_Package (Db          : in out Kit.Databases.Database_Type;
                           With_System : Boolean := True)
   is
   begin
      if Tok /= Tok_Package then
         Error ("missing package");
      end if;

      Scan;

      declare
         Package_Name : constant String := Parse_Qualified_Identifier;
      begin
         Expect (Tok_Is, (Tok_Record, Tok_End));

         Db.Create_Database (Package_Name);

         if With_System then
            if not Got_System_Db then
               Open (Kit.Paths.Config_Path & "/kit.kit");
               Read_Package (System_Db, With_System => False);
               Close;
               Got_System_Db := True;
            end if;
            Db.With_Database (System_Db);
         end if;

         while At_Declaration loop
            if Tok = Tok_Record then
               Parse_Record (Db);
            elsif Tok = Tok_Type then
               Parse_Type_Declaration (Db);
            else
               pragma Assert (False);
            end if;
         end loop;

         Expect (Tok_End, Tok_End);

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
