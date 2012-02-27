with Kit.Tables;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

package body Kit.Generate.Database_Package is

   type Database_Operation is (Create, Open);

   function Operation_Name (Op : Database_Operation) return String;

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
         return Aquarius.Drys.Declarations.Subprogram_Declaration;

      function Create_Close_Procedure
        return Aquarius.Drys.Declarations.Subprogram_Declaration;

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
        return Aquarius.Drys.Declarations.Subprogram_Declaration
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
         return Aquarius.Drys.Declarations.Subprogram_Declaration
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
              (Aquarius.Drys.Object (Table.Ada_Name & "_Impl." &
              "Disk_Storage_Units"));
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
           (Aquarius.Drys.Literal (Db.Name & ".marlowe"));
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

         return New_Procedure (Operation_Name (Operation),
                               Block);

      end Create_Database_Procedure;

   begin

      Result.With_Package ("Marlowe.Btree_Handles", Body_With => True);
      Result.With_Package ("Kit.Cache",    Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                           Body_With => True);

      if True then
         Db.Iterate (Add_Implementation_With'Access);
      end if;

      for I in Database_Operation loop
         declare
            use Aquarius.Drys.Declarations;
            Proc : constant Subprogram_Declaration :=
                     Create_Database_Procedure (I);
         begin
            Result.Append (Proc);
         end;
      end loop;

      Result.Append (Create_Close_Procedure);

      return Result;
   end Generate_Database_Package;

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
