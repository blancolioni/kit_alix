with Kit.Fields;
with Kit.Tables;
with Kit.Types;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

package body Kit.Generate.Database_Package is

   type Database_Operation is (Create, Open);

   function Operation_Name (Op : Database_Operation) return String;

   procedure Initialise_Database_Structure
     (Db  : Kit.Databases.Database_Type;
      Seq : in out Aquarius.Drys.Statement_Sequencer'Class);

   -------------------------------
   -- Generate_Database_Package --
   -------------------------------

   function Generate_Database_Package
     (Db : in out Kit.Databases.Database_Type)
      return Aquarius.Drys.Declarations.Package_Type
   is
      Result : Aquarius.Drys.Declarations.Package_Type :=
                 Aquarius.Drys.Declarations.New_Package_Type
                   (Db.Name & ".Database");

      procedure Add_Implementation_With
        (Table : Kit.Tables.Table_Type'Class);

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Aquarius.Drys.Declarations.Subprogram_Declaration'Class;

      function Create_Close_Procedure
        return Aquarius.Drys.Declarations.Subprogram_Declaration'Class;

      procedure Add_Implementation_With
        (Table : Kit.Tables.Table_Type'Class)
      is
      begin
         Result.With_Package
           (Withed       => Db.Ada_Name & "." & Table.Ada_Name & "_Impl",
            Private_With => False,
            Body_With    => True);
      end Add_Implementation_With;

      ----------------------------
      -- Create_Close_Procedure --
      ----------------------------

      function Create_Close_Procedure
        return Aquarius.Drys.Declarations.Subprogram_Declaration'Class
      is
         use Aquarius.Drys;
         Block : Aquarius.Drys.Blocks.Block_Type;
      begin
         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Kit.Cache.Close"));
         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Lock"));
         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Marlowe.Btree_Handles.Close",
               Object ("Marlowe_Keys.Handle")));
         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Unlock"));
         return Aquarius.Drys.Declarations.New_Procedure ("Close", Block);
      end Create_Close_Procedure;

      -------------------------------
      -- Create_Database_Procedure --
      -------------------------------

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Aquarius.Drys.Declarations.Subprogram_Declaration'Class
      is
         use Aquarius.Drys.Declarations;
         Block : Aquarius.Drys.Blocks.Block_Type;

         procedure Create_Table
           (Table : Kit.Tables.Table_Type'Class);

         procedure Open_Table
           (Table : Kit.Tables.Table_Type'Class);

         ------------------
         -- Create_Table --
         ------------------

         procedure Create_Table
           (Table : Kit.Tables.Table_Type'Class)
         is
            use Aquarius.Drys.Statements;

            procedure Create_Key
              (Base     : Kit.Tables.Table_Type'Class;
               Position : Kit.Tables.Key_Cursor);

            ----------------
            -- Create_Key --
            ----------------

            procedure Create_Key
              (Base     : Kit.Tables.Table_Type'Class;
               Position : Kit.Tables.Key_Cursor)
            is
               pragma Unreferenced (Base);
               use Aquarius.Drys;
               use Aquarius.Drys.Expressions;
               use Kit.Tables;

               Call_Add_Key : Function_Call_Expression :=
                                New_Function_Call_Expression
                                  (Procedure_Name =>
                                      "Marlowe.Btree_Handles.Add_Key");
            begin
               Call_Add_Key.Add_Actual_Argument
                 (Object ("Marlowe_Keys.Handle"));
               Call_Add_Key.Add_Actual_Argument
                 (Literal (Table.Name & "_" & Name (Position)));
               Call_Add_Key.Add_Actual_Argument
                 (Object (Table.Ada_Name & "_Table_Index"));
               Call_Add_Key.Add_Actual_Argument
                 (Literal (Key_Size (Position)));

               Block.Add_Statement
                 (New_Assignment_Statement
                    ("Marlowe_Keys." &
                     Table.Ada_Name & "_" &
                     Kit.Tables.Ada_Name (Position) & "_Ref",
                     Call_Add_Key));
            end Create_Key;

            Proc  : Procedure_Call_Statement :=
                      New_Procedure_Call_Statement
                        ("Marlowe.Btree_Handles.Add_Table");
         begin
            Proc.Add_Actual_Argument
              (Aquarius.Drys.Object ("Marlowe_Keys.Handle"));
            Proc.Add_Actual_Argument
              (Aquarius.Drys.Literal (Table.Ada_Name));
            Proc.Add_Actual_Argument
              (Aquarius.Drys.Literal (Natural (Table.Length)));
            Block.Add_Statement (Proc);

            Table.Scan_Keys (Create_Key'Access);

         end Create_Table;

         ----------------
         -- Open_Table --
         ----------------

         procedure Open_Table
           (Table : Kit.Tables.Table_Type'Class)
         is
            use Aquarius.Drys.Statements;

            procedure Open_Key
              (Base     : Kit.Tables.Table_Type'Class;
               Position : Kit.Tables.Key_Cursor);

            --------------
            -- Open_Key --
            --------------

            procedure Open_Key
              (Base     : Kit.Tables.Table_Type'Class;
               Position : Kit.Tables.Key_Cursor)
            is
               pragma Unreferenced (Base);
               use Aquarius.Drys.Expressions;
               use Kit.Tables;

               Call_Open_Key : Function_Call_Expression :=
                                 New_Function_Call_Expression
                                   (Procedure_Name =>
                                      "Marlowe.Btree_Handles.Get_Reference");
            begin
               Call_Open_Key.Add_Actual_Argument
                 (Aquarius.Drys.Object ("Marlowe_Keys.Handle"));
               Call_Open_Key.Add_Actual_Argument
                 (Aquarius.Drys.Literal (Table.Name & "_" & Name (Position)));

               Block.Add_Statement
                 (New_Assignment_Statement
                    ("Marlowe_Keys." &
                     Table.Ada_Name & "_" &
                     Kit.Tables.Ada_Name (Position) & "_Ref",
                     Call_Open_Key));
            end Open_Key;

         begin

            Table.Scan_Keys (Open_Key'Access);

         end Open_Table;

         Access_Db  : Aquarius.Drys.Statements.Procedure_Call_Statement :=
                        Aquarius.Drys.Statements.New_Procedure_Call_Statement
                          ("Marlowe.Btree_Handles." &
                           Operation_Name (Operation));
      begin

         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              (Procedure_Name => "Kit.Cache.Start_Cache"));

         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Lock"));

         Access_Db.Add_Actual_Argument
           (Aquarius.Drys.Object ("Marlowe_Keys.Handle"));
         Access_Db.Add_Actual_Argument
           (Aquarius.Drys.Object ("Path"));
         Access_Db.Add_Actual_Argument
           (Aquarius.Drys.Object ("Database_Magic_Number"));
         Block.Add_Statement (Access_Db);

         case Operation is
            when Create =>
               Db.Iterate (Create_Table'Access);
            when Open =>
               Db.Iterate (Open_Table'Access);
         end case;

         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Unlock"));

         if Operation = Create then
            Initialise_Database_Structure (Db, Block);
         end if;

         declare
            Result : Subprogram_Declaration'Class :=
                       New_Procedure (Operation_Name (Operation),
                                      Block);
         begin
            Result.Add_Formal_Argument
              (Arg_Name    => "Path",
               Arg_Type    => "String",
               Arg_Default =>
                 Aquarius.Drys.Literal (Db.Name & ".marlowe"));

            return Result;
         end;

      end Create_Database_Procedure;

   begin

      Result.With_Package ("Marlowe.Btree_Handles", Body_With => True);
      Result.With_Package ("Kit.Cache",    Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                           Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Kit_Record",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Record_Base",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Enumeration",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Field",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Type",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Float",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Integer",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Literal",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Long_Float",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_String",
                           Body_With => True);

      if True then
         Db.Iterate (Add_Implementation_With'Access);
      end if;

      for I in Database_Operation loop
         declare
            use Aquarius.Drys.Declarations;
            Proc : constant Subprogram_Declaration'Class :=
                     Create_Database_Procedure (I);
         begin
            Result.Append (Proc);
         end;
      end loop;

      Result.Append (Create_Close_Procedure);

      return Result;
   end Generate_Database_Package;

   -----------------------------------
   -- Initialise_Database_Structure --
   -----------------------------------

   procedure Initialise_Database_Structure
     (Db  : Kit.Databases.Database_Type;
      Seq : in out Aquarius.Drys.Statement_Sequencer'Class)
   is

      procedure Create_Table
        (Table : Kit.Tables.Table_Type'Class);

      procedure Create_Type (T  : Kit.Types.Kit_Type'Class);

      ------------------
      -- Create_Table --
      ------------------

      procedure Create_Table
        (Table : Kit.Tables.Table_Type'Class)
      is
         use Aquarius.Drys;
         use Aquarius.Drys.Expressions;
         use Aquarius.Drys.Statements;
         Create : constant Statement'Class :=
                    New_Procedure_Call_Statement
                      ("Kit_Record.Create",
                       Literal (Table.Ada_Name),
                       New_Function_Call_Expression
                         ("Natural",
                          Object (Table.Ada_Name
                            & "_Impl.Disk_Storage_Units")));

         procedure Create_Field
           (Field       : Kit.Fields.Field_Type'Class);

         procedure Create_Base (Base  : Kit.Tables.Table_Type'Class);

         -----------------
         -- Create_Base --
         -----------------

         procedure Create_Base (Base  : Kit.Tables.Table_Type'Class) is
            New_Base : Procedure_Call_Statement'Class :=
                          New_Procedure_Call_Statement
                            ("Kit_Record_Base.Create");
         begin
            New_Base.Add_Actual_Argument
              (Object
                 ("Kit_Record.First_By_Name ("""
                  & Table.Ada_Name
                  & """).Reference"));
            New_Base.Add_Actual_Argument
              (Object
                 ("Kit_Record.First_By_Name ("""
                  & Base.Ada_Name
                  & """).Reference"));
            Seq.Append (New_Base);
         end Create_Base;

         ------------------
         -- Create_Field --
         ------------------

         procedure Create_Field
           (Field       : Kit.Fields.Field_Type'Class)
         is
            New_Field : Procedure_Call_Statement'Class :=
                          New_Procedure_Call_Statement
                            ("Kit_Field.Create");
         begin
            New_Field.Add_Actual_Argument (Literal (Field.Ada_Name));
            New_Field.Add_Actual_Argument
              (Object
                 ("Kit_Record.First_By_Name ("""
                  & Table.Ada_Name
                  & """).Reference"));
            New_Field.Add_Actual_Argument
              (Field.Get_Field_Type.Reference_Database_Type);

            New_Field.Add_Actual_Argument
              (Literal (Natural (Table.Field_Start (Field))));
            New_Field.Add_Actual_Argument
              (Literal (Field.Get_Field_Type.Size));
            Seq.Append (New_Field);
         end Create_Field;

      begin
         Seq.Append (Create);
         Table.Scan_Fields (Create_Field'Access);
         Table.Iterate (Create_Base'Access, Inclusive => False);
      end Create_Table;

      -----------------
      -- Create_Type --
      -----------------

      procedure Create_Type (T  : Kit.Types.Kit_Type'Class) is
      begin
         Seq.Append (T.Create_Database_Record);
      end Create_Type;

   begin
      Kit.Types.Iterate_All_Types (Create_Type'Access);
      Db.Iterate (Create_Table'Access);
   end Initialise_Database_Structure;

   --------------------
   -- Operation_Name --
   --------------------

   function Operation_Name (Op : Database_Operation) return String is
   begin
      case Op is
         when Create =>
            return "Create";
         when Open =>
            return "Open";
      end case;
   end Operation_Name;

end Kit.Generate.Database_Package;
