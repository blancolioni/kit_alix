with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

with Kit.Generate.Fetch;
with Kit.Generate.Public_Get;

with Kit.Options;
with Kit.String_Maps;

package body Kit.Generate.Public_Interface is

   procedure Create_Control_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Field_Constants
     (Table : in Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Locking_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Overrides
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Search_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Field_Store_Procedure
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Base  : in     Kit.Schema.Tables.Table_Type'Class;
      Field : in     Kit.Schema.Fields.Field_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Identity_Function
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Generic_Get
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Generic_Set
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   -------------------------------
   -- Create_Control_Procedures --
   -------------------------------

   procedure Create_Control_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;

      Argument     : constant Formal_Argument'Class :=
                       New_Inout_Argument
                         ("Item",
                          Aquarius.Drys.Named_Subtype
                            (Table.Ada_Name & "_Implementation"));

      procedure Create_Initialize;
      procedure Create_Finalize;

      ---------------------
      -- Create_Finalize --
      ---------------------

      procedure Create_Finalize is
         use Aquarius.Drys.Statements;
         Insert_Keys : Sequence_Of_Statements;
         Finalize_Block : Aquarius.Drys.Blocks.Block_Type;

         procedure Insert_Table_Keys
           (Base  : Kit.Schema.Tables.Table_Type'Class);


         -----------------------
         -- Insert_Table_Keys --
         -----------------------

         procedure Insert_Table_Keys
           (Base  : Kit.Schema.Tables.Table_Type'Class)
         is
            procedure Insert_Key
              (Key_Table : Kit.Schema.Tables.Table_Type'Class;
               Key       : Kit.Schema.Keys.Key_Type'Class);

            ----------------
            -- Insert_Key --
            ----------------

            procedure Insert_Key
              (Key_Table : Kit.Schema.Tables.Table_Type'Class;
               Key       : Kit.Schema.Keys.Key_Type'Class)
            is
               Insert : Procedure_Call_Statement :=
                          New_Procedure_Call_Statement
                            ("Marlowe.Btree_Handles.Insert");
            begin
               Insert.Add_Actual_Argument ("Marlowe_Keys.Handle");
               Insert.Add_Actual_Argument ("Marlowe_Keys."
                                           & Base.Key_Reference_Name
                                             (Key));
               Insert.Add_Actual_Argument
                 (Kit.Schema.Tables.To_Storage (Table       => Table,
                                                Base_Table  => Base,
                                                Key_Table   => Key_Table,
                                                Object_Name => "Item",
                                                Key         => Key,
                                                With_Index  => True));
               Insert_Keys.Append (Insert);
            end Insert_Key;

         begin
            Base.Scan_Keys (Insert_Key'Access);
         end Insert_Table_Keys;

      begin

         Finalize_Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));
         Finalize_Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("Marlowe.Database_Index"));

         Finalize_Block.Add_Statement
           (If_Statement
              (Operator ("=", Object ("Item.Index"), Literal (0)),
               New_Return_Statement));

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

         if Kit.Options.Generate_Debug then
            Finalize_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Operator
                    ("&",
                     Literal
                       ("Finalize " & Table.Ada_Name & ":"),
                     New_Function_Call_Expression
                       ("Marlowe.Database_Index'Image",
                        Object ("Item.Index")))));
         end if;

         Table.Iterate (Insert_Table_Keys'Access,
                        Inclusive   => True,
                        Table_First => False);

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));

         declare
            X_Locked : constant Expression'Class :=
                         Object ("Item.Link.X_Locked");
            Created  : constant Expression'Class :=
                         Object ("Item.Created");
            X_L_And_C : constant Expression'Class :=
                          Operator ("and then", X_Locked, Created);
            If_X_And_C : constant Statement'Class :=
                           If_Statement (X_L_And_C, Insert_Keys);
         begin
            Finalize_Block.Add_Statement (If_X_And_C);
         end;

         declare
            S_Locked : constant Expression'Class :=
                         Object ("Item.Link.S_Locked");
            X_Locked : constant Expression'Class :=
                         Object ("Item.Link.X_Locked");
            S_And_X  : constant Expression'Class :=
                         Operator ("or else", S_Locked, X_Locked);
            Same_Context  : constant Expression'Class :=
                              Operator ("not", Object ("Item.Subclassed"));
            Do_Unlock     : constant Expression'Class :=
                              Long_Operator ("and then",
                                             S_And_X, Same_Context);
            Unlock        : constant Statement'Class :=
                              New_Procedure_Call_Statement
                                ("Item.Local.Unlock");
            If_Do_Unlock  : constant Statement'Class :=
                              If_Statement (Do_Unlock, Unlock);
         begin
            Finalize_Block.Add_Statement (If_Do_Unlock);
         end;

         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.Index", Literal (0)));
         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.Created", Object ("False")));
         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.Local", Object ("null")));
         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.Link", Object ("null")));

--           Finalize_Block.Add_Statement
--             (New_Procedure_Call_Statement
--                ("Free", Object ("Item.Mark")));

         if Kit.Options.Generate_Debug then
            Finalize_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Literal
                    ("Finalize " & Table.Ada_Name & ": complete")));
         end if;
         declare
            Finalize : Subprogram_Declaration'Class :=
                         New_Procedure ("Finalize",
                                        Argument, Finalize_Block);
         begin
            Finalize.Set_Overriding;
            Top.Append_To_Body (Finalize);
         end;
      end Create_Finalize;

      -----------------------
      -- Create_Initialize --
      -----------------------

      procedure Create_Initialize is
         Initialize_Block : Aquarius.Drys.Blocks.Block_Type;

         procedure Set_Field (Field_Name : String;
                              Value      : String);

         ---------------
         -- Set_Field --
         ---------------

         procedure Set_Field (Field_Name : String;
                              Value      : String)
         is
         begin
            Initialize_Block.Add_Statement
              (Aquarius.Drys.Statements.New_Assignment_Statement
                 ("Item." & Field_Name,
                  Aquarius.Drys.Object (Value)));
         end Set_Field;

      begin
         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Memory_Mutex.Lock"));
         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Assignment_Statement
              ("Item.Local",
               Aquarius.Drys.Expressions.New_Allocation_Expression
                 ("Local_Lock_Context_Record")));
         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Memory_Mutex.Unlock"));

         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Assignment_Statement
              ("Item.Link",
               Aquarius.Drys.Object
                 ("Lock_Context (Item.Local)")));

         Set_Field ("Finished", "True");
         Set_Field ("Forward", "True");
         Set_Field ("Created", "False");
         Set_Field ("Deleted", "False");
         Set_Field ("Scanning", "False");
         Set_Field ("Has_Finish", "False");
         Set_Field ("Subclassed", "False");
         Set_Field ("Using_Key", "False");
         Set_Field ("Index", "0");

         Set_Field ("Local.X_Locked", "False");
         Set_Field ("Local.S_Locked", "False");

         declare
            Initialize : Subprogram_Declaration'Class :=
                           New_Procedure ("Initialize",
                                          Argument, Initialize_Block);
         begin
            Initialize.Set_Overriding;
            Top.Append_To_Body (Initialize);
         end;
      end Create_Initialize;

   begin
      Create_Initialize;
      Create_Finalize;
   end Create_Control_Procedures;

   ----------------------------
   -- Create_Field_Constants --
   ----------------------------

   procedure Create_Field_Constants
     (Table : in Kit.Schema.Tables.Table_Type'Class;
      Top : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      procedure Add_Field_Constant
        (Base : Kit.Schema.Tables.Table_Type'Class;
         Item : Kit.Schema.Fields.Field_Type'Class);

      ------------------------
      -- Add_Field_Constant --
      ------------------------

      procedure Add_Field_Constant
        (Base : Kit.Schema.Tables.Table_Type'Class;
         Item : Kit.Schema.Fields.Field_Type'Class)
      is
         pragma Unreferenced (Base);
         Name : constant String := Item.Ada_Name;
         Value : constant String := Item.Standard_Name;
         Defn : constant Aquarius.Drys.Declaration'Class :=
                  Aquarius.Drys.Declarations.New_Constant_Declaration
                    (Name        => "F_" & Name,
                     Object_Type => "String",
                     Value       => Aquarius.Drys.Literal (Value));
      begin
         Top.Append (Defn);
      end Add_Field_Constant;

   begin
      Table.Iterate_All (Add_Field_Constant'Access);
      Top.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Field_Constants;

   ----------------------------------
   -- Create_Field_Store_Procedure --
   ----------------------------------

   procedure Create_Field_Store_Procedure
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Base  : in     Kit.Schema.Tables.Table_Type'Class;
      Field : in     Kit.Schema.Fields.Field_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      Store_Block : Aquarius.Drys.Blocks.Block_Type;

      procedure Create_Abstract_Store;

      procedure Lock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class);

      procedure Unlock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class);

      procedure Perform_Lock
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class;
         Lock     : Boolean);

      procedure Delete_Old_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class);

      procedure Insert_New_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class);

      procedure Key_Operation
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class;
         Operation : String);

      procedure Release_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class);
      pragma Unreferenced (Release_Key);

      procedure Process_Keys
        (Process : not null access
           procedure (Table_Base : Kit.Schema.Tables.Table_Type'Class;
                      Key_Base : Kit.Schema.Tables.Table_Type'Class;
                      Key        : Kit.Schema.Keys.Key_Type'Class));

      ---------------------------
      -- Create_Abstract_Store --
      ---------------------------

      procedure Create_Abstract_Store is
         use Aquarius.Drys.Declarations;
         Store : Subprogram_Declaration'Class :=
                   New_Abstract_Procedure
                     ("Set_" & Field.Ada_Name,
                      New_Inout_Argument ("Item",
                        Aquarius.Drys.Named_Subtype
                          (Table.Interface_Name)));
      begin
         Store.Add_Formal_Argument
           (New_Formal_Argument ("Value",
            Aquarius.Drys.Named_Subtype
              (Field.Get_Field_Type.Argument_Subtype)));

         Top.Append (Store);

         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Type_Name : constant String :=
                             Field.Get_Field_Type.Referenced_Table_Name;
               Store : Subprogram_Declaration'Class :=
                         New_Abstract_Procedure
                           ("Set_" & Field.Ada_Name,
                            New_Inout_Argument ("Item",
                              Aquarius.Drys.Named_Subtype
                                (Table.Interface_Name)));
            begin
               Store.Add_Formal_Argument
                 (New_Formal_Argument ("Value",
                  Aquarius.Drys.Named_Subtype
                    (Db.Ada_Name & "." & Type_Name & "."
                     & Type_Name & "_Type")));

               Top.Append (Store);
            end;
         end if;

         Top.Append (New_Separator);
      end Create_Abstract_Store;

      --------------------
      -- Delete_Old_Key --
      --------------------

      procedure Delete_Old_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Delete");
      end Delete_Old_Key;

      --------------------
      -- Insert_New_Key --
      --------------------

      procedure Insert_New_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Insert");
      end Insert_New_Key;

      -------------------
      -- Key_Operation --
      -------------------

      procedure Key_Operation
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class;
         Operation : String)
      is
         S : Aquarius.Drys.Statements.Procedure_Call_Statement :=
               Aquarius.Drys.Statements.New_Procedure_Call_Statement
                 ("Marlowe.Btree_Handles." & Operation);
         Key_Storage : constant Aquarius.Drys.Expression'Class :=
                         Table.To_Storage
                           (Base_Table  => Table_Base,
                            Key_Table   => Key_Base,
                            Object_Name => "Item",
                            Key         => Key,
                            With_Index  => False);
      begin

         S.Add_Actual_Argument
           ("Marlowe_Keys.Handle");
         S.Add_Actual_Argument
           ("Marlowe_Keys."
            & Table_Base.Key_Reference_Name (Key));
         S.Add_Actual_Argument
           (Aquarius.Drys.Expressions.Operator
              ("&",
               Key_Storage,
               Aquarius.Drys.Expressions.New_Function_Call_Expression
                 ("Marlowe.Key_Storage.To_Storage_Array",
                  Aquarius.Drys.Expressions.New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Table.Database_Index_Component ("Item", Table_Base)))));

         Store_Block.Add_Statement
           (Aquarius.Drys.Statements.If_Statement
              (Aquarius.Drys.Expressions.Operator
                 ("not", Aquarius.Drys.Object ("Item.Created")),
               S));
      end Key_Operation;

      --------------
      -- Lock_Key --
      --------------

      procedure Lock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         Perform_Lock (Table_Base, Key_Base, Key, True);
      end Lock_Key;

      ------------------
      -- Perform_Lock --
      ------------------

      procedure Perform_Lock
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class;
         Lock     : Boolean)
      is
         pragma Unreferenced (Key_Base);
         Lock_Name : constant String :=
                       (if Lock then "Lock" else "Unlock");
         S : constant Aquarius.Drys.Statement'Class :=
               Aquarius.Drys.Statements.New_Procedure_Call_Statement
                 (Table_Base.Ada_Name
                  & "_Impl." & Key.Ada_Name
                  & "_Key_Mutex." & Lock_Name);
      begin
         Store_Block.Add_Statement
           (Aquarius.Drys.Statements.If_Statement
              (Aquarius.Drys.Expressions.Operator
                 ("not", Aquarius.Drys.Object ("Item.Created")),
               S));
      end Perform_Lock;

      ------------------
      -- Process_Keys --
      ------------------

      procedure Process_Keys
        (Process : not null access
           procedure (Table_Base : Kit.Schema.Tables.Table_Type'Class;
                      Key_Base : Kit.Schema.Tables.Table_Type'Class;
                      Key        : Kit.Schema.Keys.Key_Type'Class))
      is

         procedure Process_Base
           (Base : Kit.Schema.Tables.Table_Type'Class);

         ------------------
         -- Process_Base --
         ------------------

         procedure Process_Base
           (Base : Kit.Schema.Tables.Table_Type'Class)
         is
         begin
            Base.Scan_Keys (Field, Process);
         end Process_Base;

      begin
         Table.Iterate (Process_Base'Access,
                        Inclusive   => True,
                        Table_First => False);
      end Process_Keys;

      -----------------
      -- Release_Key --
      -----------------

      procedure Release_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class)
      is
         pragma Unreferenced (Key_Base);
         use Aquarius.Drys;
         use Aquarius.Drys.Expressions;
         use Aquarius.Drys.Statements;
         Not_Created    : constant Expression'Class :=
                            Operator
                              ("not", Aquarius.Drys.Object ("Item.Created"));
         Using_Key      : constant Expression'Class :=
                            Object ("Item.Scanning");
         Key_Ref        : constant String :=
                            "Marlowe_Keys."
                            & Table_Base.Key_Reference_Name (Key);
         This_Key       : constant Expression'Class :=
                            Operator
                              ("=",
                               Object ("Item.Key_Ref"),
                               Object (Key_Ref));
         Release        : constant Statement'Class :=
                            New_Procedure_Call_Statement
                              ("Marlowe.Btree_Handles.Release",
                               Object ("Item.Mark.all"));
      begin
         Store_Block.Add_Statement
           (If_Statement
              (Long_Operator
                 ("and then",
                  Operator ("and then", Not_Created, Using_Key),
                  This_Key),
               Release));

      end Release_Key;

      ----------------
      -- Unlock_Key --
      ----------------

      procedure Unlock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type'Class;
         Key_Base   : Kit.Schema.Tables.Table_Type'Class;
         Key        : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         Perform_Lock (Table_Base, Key_Base, Key, False);
      end Unlock_Key;

   begin

      if not Table.Inherited_Field (Field) then
         Create_Abstract_Store;
      end if;

      Store_Block.Add_Declaration
        (Aquarius.Drys.Declarations.Use_Type
           ("System.Storage_Elements.Storage_Array"));
      if Table.Has_Key_Field then
         Store_Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("Marlowe.Btree_Handles.Btree_Reference"));
      end if;

      Store_Block.Add_Declaration
        (Aquarius.Drys.Declarations.Renaming_Declaration
           ("Target",
            Field.Get_Field_Type.Unconstrained_Record_Subtype,
            Aquarius.Drys.Object
              (Table.Base_Field_Name ("Item", Base, Field))));

      declare
         use Aquarius.Drys.Statements;
      begin
         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Item.X_Lock"));
      end;

      Process_Keys (Lock_Key'Access);
      --  Table.Scan_Keys (Field, Release_Key'Access);
      Process_Keys (Delete_Old_Key'Access);

      Field.Get_Field_Type.Set_Value
        (Target_Name => "Target",
         Value_Name  => "Value",
         Sequence    => Store_Block);

      Process_Keys (Insert_New_Key'Access);

      declare
         use Aquarius.Drys.Statements;
      begin
         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));
      end;

      Process_Keys (Unlock_Key'Access);

      declare
         use Aquarius.Drys.Declarations;
         Store : Subprogram_Declaration'Class :=
                   New_Procedure
                     ("Set_" & Field.Ada_Name,
                      New_Inout_Argument ("Item",
                        Aquarius.Drys.Named_Subtype
                          (Table.Ada_Name & "_Implementation")),
                      Store_Block);
      begin
         Store.Add_Formal_Argument
           (New_Formal_Argument ("Value",
            Aquarius.Drys.Named_Subtype
              (Field.Get_Field_Type.Argument_Subtype)));

         Store.Set_Overriding;
         Top.Append_To_Body (Store);
      end;

      if Field.Get_Field_Type.Is_Table_Reference then
         declare
            use Aquarius.Drys;
            use Aquarius.Drys.Declarations;
            use Aquarius.Drys.Statements;
            Type_Name : constant String :=
                          Field.Get_Field_Type.Referenced_Table_Name;
            Store     : Subprogram_Declaration'Class :=
                          New_Procedure
                            ("Set_" & Field.Ada_Name,
                             New_Inout_Argument ("Item",
                               Aquarius.Drys.Named_Subtype
                                 (Table.Ada_Name & "_Implementation")),
                             Aquarius.Drys.Blocks.Create_Block
                               (New_Procedure_Call_Statement
                                  ("Item.Set_" & Field.Ada_Name,
                                   Object ("Value.Reference"))));
         begin
            Store.Add_Formal_Argument
              (New_Formal_Argument ("Value",
               Aquarius.Drys.Named_Subtype
                 (Db.Ada_Name & "." & Type_Name & "."
                  & Type_Name & "_Type")));
            Store.Set_Overriding;
            Top.Append_To_Body (Store);
         end;
      end if;

   end Create_Field_Store_Procedure;

   ------------------------
   -- Create_Generic_Get --
   ------------------------

   procedure Create_Generic_Get
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;

      Got_Field : Boolean := False;

      Block  : Aquarius.Drys.Blocks.Block_Type;
      Choose : If_Statement_Record'Class :=
                 If_Statement
                   (Operator ("=", Object ("Field"), Literal ("")),
                    Raise_Statement
                      ("Constraint_Error",
                       "missing field name"),
                    Raise_Statement
                      ("Constraint_Error",
                       "no such field"));

      procedure Select_Field
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class);

      ------------------
      -- Select_Field --
      ------------------

      procedure Select_Field
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         if Field.Readable then
            Seq.Append
              (New_Return_Statement
                 (Kit.Schema.Types.Convert_To_String
                    (Field.Get_Field_Type,
                     "Item." & Field.Ada_Name)));
            Got_Field := True;
         else
            Seq.Append
              (Raise_Statement
                 ("Constraint_Error",
                  "field " & Field.Ada_Name
                  & " is not readable"));
         end if;

         Choose.Add_Elsif
           (Operator
              ("=", Object ("Field"), Literal (Field.Standard_Name)),
            Seq);

      end Select_Field;

   begin

      Table.Iterate_All (Select_Field'Access);

      if not Got_Field then
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Item"));
      end if;

      Block.Append (Choose);

      declare
         Fetch : Subprogram_Declaration'Class :=
                   New_Function
                     ("Get",
                      Named_Subtype ("String"),
                      Block);
      begin

         Fetch.Add_Formal_Argument
           ("Item",
            Table.Ada_Name & "_Implementation");
         Fetch.Add_Formal_Argument
           ("Field",
            "String");

         Fetch.Set_Overriding;
         Top.Append_To_Body (Fetch);

      end;

   end Create_Generic_Get;

   ------------------------
   -- Create_Generic_Set --
   ------------------------

   procedure Create_Generic_Set
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;

      Got_Field : Boolean := False;

      Block  : Aquarius.Drys.Blocks.Block_Type;
      Choose : If_Statement_Record'Class :=
                 If_Statement
                   (Operator ("=", Object ("Field"), Literal ("")),
                    Raise_Statement
                      ("Constraint_Error",
                       "missing field name"),
                    Raise_Statement
                      ("Constraint_Error",
                       "no such field"));

      procedure Set_Field
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class);

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         if Field.Writeable then
            Seq.Append
              (New_Procedure_Call_Statement
                 ("Item.Set_" & Field.Ada_Name,
                  Kit.Schema.Types.Convert_From_String
                    (Field.Get_Field_Type,
                     "Value")));
            Got_Field := True;
         else
            Seq.Append
              (Raise_Statement
                 ("Constraint_Error",
                  "field " & Field.Ada_Name
                  & " is not writable"));
         end if;

         Choose.Add_Elsif
           (Operator
              ("=", Object ("Field"), Literal (Field.Standard_Name)),
            Seq);

      end Set_Field;

   begin

      Table.Iterate_All (Set_Field'Access);

      Block.Append (Choose);

      if not Got_Field then
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Item"));
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Value"));
      end if;

      declare
         Store : Subprogram_Declaration'Class :=
                   New_Procedure
                     ("Set",
                      Block);
      begin

         Store.Add_Formal_Argument
           ("Item",
            Inout_Argument,
            Table.Ada_Name & "_Implementation");
         Store.Add_Formal_Argument
           ("Field",
            "String");
         Store.Add_Formal_Argument
           ("Value",
            "String");

         Store.Set_Overriding;
         Top.Append_To_Body (Store);
      end;

   end Create_Generic_Set;

   ------------------------------
   -- Create_Identity_Function --
   ------------------------------

   procedure Create_Identity_Function
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;

      Block  : Aquarius.Drys.Blocks.Block_Type;

   begin

      Block.Add_Statement
        (New_Return_Statement
           (Operator
              ("&",
               Literal (Table.Name),
               New_Function_Call_Expression
                 (Table.Ada_Name & "_Reference'Image",
                  "Item.Reference"))));

      declare
         Identity : Subprogram_Declaration'Class :=
                   New_Function
                     ("Identity",
                      Named_Subtype ("String"),
                      Block);
      begin

         Identity.Add_Formal_Argument
           ("Item",
            Table.Ada_Name & "_Implementation");

         Identity.Set_Overriding;
         Top.Append_To_Body (Identity);

      end;

   end Create_Identity_Function;

   -------------------------------
   -- Create_Locking_Procedures --
   -------------------------------

   procedure Create_Locking_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys.Declarations;
      Argument     : constant Formal_Argument'Class :=
                       New_Inout_Argument
                         ("Item",
                          Aquarius.Drys.Named_Subtype
                            ("Local_Lock_Context_Record"));

      Unlock_Block : Aquarius.Drys.Blocks.Block_Type;
      S_Lock_Block : Aquarius.Drys.Blocks.Block_Type;
      X_Lock_Block : Aquarius.Drys.Blocks.Block_Type;

      procedure Add_Locker (Name     : String;
                            Block    : Aquarius.Drys.Blocks.Block_Type);

      procedure Unlock (T : Kit.Schema.Tables.Table_Type'Class);
      procedure S_Lock (T : Kit.Schema.Tables.Table_Type'Class);
      procedure X_Lock (T : Kit.Schema.Tables.Table_Type'Class);

      ----------------
      -- Add_Locker --
      ----------------

      procedure Add_Locker (Name     : String;
                            Block    : Aquarius.Drys.Blocks.Block_Type)
      is
         P : Subprogram_Declaration'Class :=
               New_Procedure (Name, Argument, Block);
      begin
         P.Set_Overriding;
         Top.Append_To_Body (P);
      end Add_Locker;

      ------------
      -- S_Lock --
      ------------

      procedure S_Lock (T : Kit.Schema.Tables.Table_Type'Class) is
      begin
         S_Lock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.S_Lock"));
      end S_Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (T : Kit.Schema.Tables.Table_Type'Class) is
      begin
         Unlock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.Unlock"));
      end Unlock;

      ------------
      -- X_Lock --
      ------------

      procedure X_Lock (T : Kit.Schema.Tables.Table_Type'Class) is
      begin
         X_Lock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.X_Lock"));
      end X_Lock;

   begin

      --  Unlock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (Unlock'Access,
                     Inclusive => True,
                     Table_First => True);
      --  Unlock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      Unlock_Block.Add_Statement ("Item.S_Locked := False");
      Unlock_Block.Add_Statement ("Item.X_Locked := False");

      Add_Locker ("Unlock", Unlock_Block);

      --  S_Lock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (S_Lock'Access,
                     Inclusive => True,
                     Table_First => True);
      --  S_Lock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      S_Lock_Block.Add_Statement ("Item.S_Locked := True");

      Add_Locker ("S_Lock", S_Lock_Block);

      --  X_Lock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (X_Lock'Access,
                     Inclusive => True,
                     Table_First => True);
      --  X_Lock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      X_Lock_Block.Add_Statement ("Item.X_Locked := True");

      Add_Locker ("X_Lock", X_Lock_Block);

   end Create_Locking_Procedures;

   ----------------------
   -- Create_Overrides --
   ----------------------

   procedure Create_Overrides
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;
      Locking_Sequence : Sequence_Of_Statements;
      X_Lock_Block : Aquarius.Drys.Blocks.Block_Type;
   begin
      Locking_Sequence.Append
        (New_Procedure_Call_Statement ("Item.Link.Unlock"));
      Locking_Sequence.Append
        (New_Procedure_Call_Statement ("Item.Link.X_Lock"));

      X_Lock_Block.Add_Statement
        (If_Statement
           (Operator
              ("not",
               Object ("Item.Link.X_Locked")),
            Locking_Sequence));

      declare
         use Aquarius.Drys.Declarations;
         X_Lock : Subprogram_Declaration'Class :=
                         New_Procedure
                           ("X_Lock",
                            X_Lock_Block);
      begin
         X_Lock.Add_Formal_Argument
           ("Item",
            Inout_Argument,
            Table.Ada_Name & "_Implementation");
         X_Lock.Set_Overriding;
         Top.Append_To_Body (X_Lock);
      end;
   end Create_Overrides;

   ------------------------------
   -- Create_Search_Procedures --
   ------------------------------

   procedure Create_Search_Procedures
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;

      procedure Create_Has_Element;
      procedure Create_Next;
      pragma Unreferenced (Create_Next);

      ------------------------
      -- Create_Has_Element --
      ------------------------

      procedure Create_Has_Element is

         Has_Element_Block : Aquarius.Drys.Blocks.Block_Type;
         --  Next_Block        : Aquarius.Drys.Blocks.Block_Type;
      begin
         Has_Element_Block.Add_Declaration
           (Use_Type ("Marlowe.Database_Index"));
         Has_Element_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Return_Statement
              (Operator ("/=", Object ("Item.Index"),
               Object ("0"))));

         declare
            Has_Element : Subprogram_Declaration'Class :=
                            New_Function
                              ("Has_Element",
                               "Boolean",
                               Has_Element_Block);
         begin
            Has_Element.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");
            Has_Element.Set_Overriding;
            Top.Append_To_Body (Has_Element);
         end;
      end Create_Has_Element;

      -----------------
      -- Create_Next --
      -----------------

      procedure Create_Next is
         use Aquarius.Drys.Statements;
         Next_Block        : Aquarius.Drys.Blocks.Block_Type;
      begin
         Next_Block.Add_Declaration
           (Use_Type ("Marlowe.Database_Index"));
         Next_Block.Add_Declaration
           (New_Object_Declaration
              ("Got_Valid_Index", "Boolean"));

         Next_Block.Add_Statement
           (If_Statement
              (Operator
                 ("not",
                  Object ("Item.Scanning")),
               Raise_Statement
                 ("Constraint_Error",
                  "Not currently scanning this table")));

         Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                     & ".Shared_Lock");
         Next_Block.Add_Statement ("Item.Local.Unlock");

         Next_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Marlowe.Btree_Handles.Next",
               Object ("Item.Mark.all")));
         Next_Block.Add_Statement
           (New_Assignment_Statement
              ("Got_Valid_Index",
               New_Function_Call_Expression
                 ("Marlowe.Btree_Handles.Valid", "Item.Mark.all")));
         Next_Block.Add_Statement
           (If_Statement
              (Object ("Got_Valid_Index"),
               New_Assignment_Statement
                 ("Item.Index",
                  New_Function_Call_Expression
                    ("Marlowe.Key_Storage.To_Database_Index",
                     New_Function_Call_Expression
                       ("Marlowe.Btree_Handles.Get_Key",
                        "Item.Mark.all")))));

         Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                   & ".Shared_Unlock");

         declare
            Fetch_Found : Sequence_Of_Statements;
            Not_Found   : Sequence_Of_Statements;
         begin
            Fetch.Fetch_From_Index (Table, "Item", Fetch_Found);
            Not_Found.Append ("Item.Index := 0");
            Next_Block.Add_Statement
              (If_Statement
                 (Object ("Got_Valid_Index"),
                  Fetch_Found,
                  Not_Found));
         end;

         declare
            Next : Subprogram_Declaration'Class :=
                            New_Procedure
                              ("Next",
                               Next_Block);
         begin
            Next.Add_Formal_Argument
              ("Item",
               Inout_Argument,
               Table.Ada_Name & "_Implementation");
            Next.Set_Overriding;
            Top.Append_To_Body (Next);
         end;
      end Create_Next;

   begin
      Create_Has_Element;
      --  Create_Next;
   end Create_Search_Procedures;

   -------------------------------
   -- Generate_Public_Interface --
   -------------------------------

   function Generate_Public_Interface
     (Db    : in out Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in     Aquarius.Drys.Declarations.Package_Type'Class)
      return Aquarius.Drys.Declarations.Package_Type'Class
   is
      use Aquarius.Drys, Aquarius.Drys.Declarations;
      Database_Package       : constant String :=
                                 Table.Ada_Name & "_Impl";
--        Database_Type          : constant String :=
--                                   Database_Package & "."
--                                     & Table.Ada_Name & "_Database_Record";
      Cache_Package          : constant String :=
                                 Table.Ada_Name & "_Cache";
      Implementation_Type    : constant String :=
                                 Table.Ada_Name & "_Implementation";

      Withed_Tables : Kit.String_Maps.String_Map;

      Table_Package : Aquarius.Drys.Declarations.Package_Type'Class :=
        Top.New_Child_Package (Table.Ada_Name);
      Table_Interface : Aquarius.Drys.Interface_Type_Definition;

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class);
      procedure Add_Base_With (It : Kit.Schema.Tables.Table_Type'Class);
      procedure Add_Base (It : Kit.Schema.Tables.Table_Type'Class);

      procedure Add_Fetch (Base  : Kit.Schema.Tables.Table_Type'Class;
                           Field : Kit.Schema.Fields.Field_Type'Class);
      procedure Add_Store (Base  : Kit.Schema.Tables.Table_Type'Class;
                           Field : Kit.Schema.Fields.Field_Type'Class);

      procedure Create_Key_Get
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Key   : Kit.Schema.Keys.Key_Type'Class);
      procedure Create_Reference_Get
        (Base : Kit.Schema.Tables.Table_Type'Class);

      procedure Add_Create_Function;

      procedure Create_Implementation_Type;

      procedure Create_Selection_Type;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base (It : Kit.Schema.Tables.Table_Type'Class) is
      begin
         Table_Interface.Add_Parent
           (Db.Ada_Name & "." & It.Ada_Name & "." &
              It.Ada_Name & "_Interface");
      end Add_Base;

      -------------------
      -- Add_Base_With --
      -------------------

      procedure Add_Base_With (It : Kit.Schema.Tables.Table_Type'Class) is
      begin
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name,
            Body_With =>  False);
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name & "_Cache",
            Body_With =>  True);
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name & "_Impl",
            Body_With =>  True);
         Withed_Tables.Insert (It.Ada_Name);
      end Add_Base_With;

      -------------------------
      -- Add_Create_Function --
      -------------------------

      procedure Add_Create_Function is

         use Aquarius.Drys.Statements;

         Sequence : Sequence_Of_Statements;

         procedure Add_Create_Arguments
           (Sub : in out Subprogram_Declaration'Class);

         procedure Allocate_Context
           (Base : Kit.Schema.Tables.Table_Type'Class);

         procedure Database_Insert
           (Base : Kit.Schema.Tables.Table_Type'Class);

         procedure Set_Field (Field_Name : String;
                              Value      : String);

         Create_Ref_Block : Aquarius.Drys.Blocks.Block_Type;
         Got_Field        : Boolean := False;

         procedure Initialise_Field
           (Base     : Kit.Schema.Tables.Table_Type'Class;
            Field    : Kit.Schema.Fields.Field_Type'Class);

         --------------------------
         -- Add_Create_Arguments --
         --------------------------

         procedure Add_Create_Arguments
           (Sub : in out Subprogram_Declaration'Class)
         is
            procedure Add_Formal_Argument
              (Base     : Kit.Schema.Tables.Table_Type'Class;
               Field    : Kit.Schema.Fields.Field_Type'Class);

            -------------------------
            -- Add_Formal_Argument --
            -------------------------

            procedure Add_Formal_Argument
              (Base     : Kit.Schema.Tables.Table_Type'Class;
               Field    : Kit.Schema.Fields.Field_Type'Class)
            is
               pragma Unreferenced (Base);
            begin
               if Field.Created then
                  Sub.Add_Formal_Argument
                    (Field.Ada_Name,
                     Field.Get_Field_Type.Argument_Subtype);
               end if;
            end Add_Formal_Argument;

         begin
            Table.Iterate_All (Add_Formal_Argument'Access);
         end Add_Create_Arguments;

         ----------------------
         -- Allocate_Context --
         ----------------------

         procedure Allocate_Context
           (Base : Kit.Schema.Tables.Table_Type'Class)
         is
            use Aquarius.Drys.Expressions;
         begin
            Sequence.Append
              (New_Assignment_Statement
                 ("Result" & Base.Base_Component_Name,
                  New_Allocation_Expression
                    (Base.Ada_Name & "_Cache.Cache_Record")));
            if Base.Name = "kit_root_record" then
               Sequence.Append
                 (New_Assignment_Statement
                    ("Result" & Base.Base_Component_Name
                     & ".Db.Top_Record",
                     Object ("R_" & Table.Ada_Name)));
            end if;
         end Allocate_Context;

         ---------------------
         -- Database_Insert --
         ---------------------

         procedure Database_Insert
           (Base : Kit.Schema.Tables.Table_Type'Class)
         is
            use Aquarius.Drys.Expressions;

            Index_Field : constant String :=
                            Table.Database_Index_Component
                              ("Result", Base);

            procedure Set_Base_Index
              (Meta_Base : Kit.Schema.Tables.Table_Type'Class);

            --------------------
            -- Set_Base_Index --
            --------------------

            procedure Set_Base_Index
              (Meta_Base : Kit.Schema.Tables.Table_Type'Class)
            is
            begin
               Sequence.Append
                 (New_Assignment_Statement
                    (Table.Database_Index_Component
                       ("Result", Base, Meta_Base),
                        Object
                          (Table.Database_Index_Component
                             ("Result",
                              Meta_Base))));
            end Set_Base_Index;

         begin
            Sequence.Append (Base.Ada_Name & "_Impl.File_Mutex.Lock");

            Sequence.Append
              (New_Assignment_Statement
                 (Index_Field,
                  New_Function_Call_Expression
                    (Base.Reference_Type,
                     New_Function_Call_Expression
                       ("Marlowe.Btree_Handles.Insert_Record",
                        Object ("Marlowe_Keys.Handle"),
                        Literal (Integer (Base.Reference_Index))))));
            if Base.Ada_Name /= Table.Ada_Name then
               Base.Iterate (Set_Base_Index'Access, Inclusive   => False);
            end if;

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Result" & Base.Base_Component_Name & ".Initialise",
                  Literal (Integer (Base.Reference_Index)),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Object (Index_Field))));

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Kit.Cache.Insert",
                  New_Function_Call_Expression
                    ("Kit.Cache.Cache_Entry",
                     "Result" & Base.Base_Component_Name)));

            Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl.Write",
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Object (Index_Field)),
                  Object ("Result" & Base.Base_Component_Name & ".Db")));

            Sequence.Append
              ("Result" & Base.Base_Component_Name & ".X_Lock");

            Sequence.Append (Base.Ada_Name & "_Impl.File_Mutex.Unlock");
         end Database_Insert;

         ----------------------
         -- Initialise_Field --
         ----------------------

         procedure Initialise_Field
           (Base     : Kit.Schema.Tables.Table_Type'Class;
            Field    : Kit.Schema.Fields.Field_Type'Class)
         is
            pragma Unreferenced (Base);
         begin
            if Field.Created then
               Create_Ref_Block.Append
                 (New_Procedure_Call_Statement
                    ("Result.Set_" & Field.Ada_Name,
                     Object (Field.Ada_Name)));
               Got_Field := True;
            end if;
         end Initialise_Field;

         ---------------
         -- Set_Field --
         ---------------

         procedure Set_Field (Field_Name : String;
                              Value      : String)
         is
         begin
            Sequence.Append
              (Aquarius.Drys.Statements.New_Assignment_Statement
                 ("Result." & Field_Name,
                  Aquarius.Drys.Object (Value)));
         end Set_Field;

      begin

         Set_Field ("Finished", "False");
         Set_Field ("Created", "True");
         Set_Field ("Deleted", "False");
         Set_Field ("Scanning", "False");
         Set_Field ("Link.X_Locked", "True");
         Set_Field ("Link.S_Locked", "False");
         Set_Field ("Using_Key", "False");
         Set_Field ("Index", "0");

         Sequence.Append
           (New_Procedure_Call_Statement ("Memory_Mutex.Lock"));

         Table.Iterate (Allocate_Context'Access,
                        Inclusive => True,
                        Table_First => False);

         Sequence.Append
           (New_Procedure_Call_Statement ("Memory_Mutex.Unlock"));

         Table.Iterate (Database_Insert'Access,
                        Inclusive => True,
                        Table_First => False);

         Sequence.Append ("Result.Local.X_Locked := True");

         declare
            Block : Aquarius.Drys.Blocks.Block_Type;
         begin
            Block.Append
              (New_Return_Statement
                 ("Result", Implementation_Type, Sequence));
            Table_Package.Append
              (New_Function ("Create", Table.Type_Name,
               Block));
         end;

         Create_Ref_Block.Add_Declaration
           (New_Object_Declaration
              ("Result",
               Table.Type_Name,
               Object ("Create")));

         Table.Iterate_All (Initialise_Field'Access);

         if Got_Field then
            declare
               Create_Ref_Proc : Subprogram_Declaration'Class :=
                                   New_Procedure
                                     (Name        => "Create",
                                      Block       => Create_Ref_Block);
            begin
               Add_Create_Arguments (Create_Ref_Proc);
               Table_Package.Append (Create_Ref_Proc);
            end;
         end if;

         Create_Ref_Block.Append
           (New_Return_Statement
              (Object ("Result.Reference")));

         declare
            Create_Ref_Fn : Subprogram_Declaration'Class :=
                              New_Function
                                (Name        => "Create",
                                 Result_Type => Table.Reference_Type,
                                 Block       => Create_Ref_Block);
         begin
            Add_Create_Arguments (Create_Ref_Fn);
            Table_Package.Append (Create_Ref_Fn);
         end;

         Table_Package.Append (New_Separator);

      end Add_Create_Function;

      ---------------
      -- Add_Fetch --
      ---------------

      procedure Add_Fetch (Base  : Kit.Schema.Tables.Table_Type'Class;
                           Field : Kit.Schema.Fields.Field_Type'Class)
      is
      begin
         if not Field.Readable then
            return;
         end if;

         if Base.Name = Table.Name then
            declare
               Fetch : constant Subprogram_Declaration'Class :=
                         New_Abstract_Function
                           (Field.Safe_Ada_Name ("Get_"),
                            New_Formal_Argument ("Item",
                              Named_Subtype
                                (Table.Ada_Name & "_Interface")),
                            Named_Subtype
                              (Field.Get_Field_Type.Return_Subtype));
            begin
               Table_Package.Append (Fetch);
               Table_Package.Append (New_Separator);
            end;
         end if;

         declare
            use Aquarius.Drys.Statements;
            Block  : Aquarius.Drys.Blocks.Block_Type;
         begin

            Block.Add_Declaration
              (Aquarius.Drys.Declarations.Renaming_Declaration
                 ("Result",
                  Field.Get_Field_Type.Unconstrained_Record_Subtype,
                  Object
                    (Table.Base_Field_Name ("Item", Base, Field))));

            Block.Add_Statement
              (New_Return_Statement
                 (Field.Get_Field_Type.Return_Value ("Result")));

            declare
               Fetch  : Subprogram_Declaration'Class :=
                          New_Function
                            (Field.Ada_Name,
                             Field.Get_Field_Type.Return_Subtype,
                             Block);
            begin
               Fetch.Add_Formal_Argument
                 ("Item",
                  Table.Ada_Name & "_Implementation");

               Fetch.Set_Overriding;
               Table_Package.Append_To_Body (Fetch);
            end;
         end;

      end Add_Fetch;

      -------------------------
      -- Add_Field_Type_With --
      -------------------------

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Field : Kit.Schema.Fields.Field_Type'Class)
      is
      begin
         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Table_Name : constant String :=
                              Field.Get_Field_Type.Referenced_Table_Name;
            begin
               if not Withed_Tables.Contains (Table_Name)
                 and then Table_Name /= Table.Ada_Name
               then
                  Table_Package.With_Package
                    (Db.Ada_Name & "." & Table_Name,
                     Body_With => Base.Standard_Name /= Table.Standard_Name);
                  Withed_Tables.Insert (Table_Name);
               end if;
            end;
         end if;
      end Add_Field_Type_With;

      ---------------
      -- Add_Store --
      ---------------

      procedure Add_Store (Base  : Kit.Schema.Tables.Table_Type'Class;
                           Field : Kit.Schema.Fields.Field_Type'Class)
      is
      begin
         if Field.Writeable then
            Create_Field_Store_Procedure
              (Db    => Db,
               Table => Table,
               Base  => Base,
               Field => Field,
               Top   => Table_Package);
         end if;
      end Add_Store;

      --------------------------------
      -- Create_Implementation_Type --
      --------------------------------

      procedure Create_Implementation_Type is

         Record_Defn : Aquarius.Drys.Types.Record_Type_Definition;
         Context_Defn : Aquarius.Drys.Types.Record_Type_Definition;

         procedure Add_Base_Component
           (It : Kit.Schema.Tables.Table_Type'Class);

         ------------------------
         -- Add_Base_Component --
         ------------------------

         procedure Add_Base_Component
           (It : Kit.Schema.Tables.Table_Type'Class)
         is
            Name : constant String := "T" & It.Index_Image;
         begin
            Context_Defn.Add_Component
              (Name & "_Data",
               It.Ada_Name & "_Cache.Cache_Access");
         end Add_Base_Component;

      begin
         Context_Defn.Add_Parent ("Lock_Context_Record");
--           Context_Defn.Add_Component ("Handle",
--                                       "Kit.Access_Control.Access_Handle");
         Table.Iterate (Add_Base_Component'Access,
                        Inclusive => True, Table_First => False);

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              ("Local_Lock_Context_Record", Context_Defn));

         Create_Locking_Procedures (Db, Table, Table_Package);

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              ("Local_Lock_Context",
               New_Access_Type
                 ("Local_Lock_Context_Record'Class",
                  Access_All => True)));

         Record_Defn.Set_Limited;
         Record_Defn.Add_Parent ("Ada.Finalization.Limited_Controlled");
         Record_Defn.Add_Parent (Table.Ada_Name & "_Interface");
         Record_Defn.Add_Component ("Finished", "Boolean");
         Record_Defn.Add_Component ("Forward", "Boolean");
         Record_Defn.Add_Component ("Created", "Boolean");
         Record_Defn.Add_Component ("Deleted", "Boolean");
         Record_Defn.Add_Component ("Scanning", "Boolean");
         Record_Defn.Add_Component ("Has_Finish", "Boolean");
         Record_Defn.Add_Component ("Start_Closed", "Boolean");
         Record_Defn.Add_Component ("Finish_Closed", "Boolean");
         Record_Defn.Add_Component ("Subclassed", "Boolean");
         Record_Defn.Add_Component ("Using_Key", "Boolean");
         Record_Defn.Add_Component ("Using_Key_Value", "Boolean");
         --  Record_Defn.Add_Component ("Mark", "Mark_Access");
         Record_Defn.Add_Component ("Key_Ref",
                                    "Marlowe.Btree_Handles.Btree_Reference");
         Record_Defn.Add_Component ("Index", Table.Reference_Type);
         Record_Defn.Add_Component ("Local",
                                    "Local_Lock_Context");
         Record_Defn.Add_Component ("Link",
                                    "Lock_Context");

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              (Implementation_Type, Record_Defn));

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              ("Implementation_Access",
               New_Access_Type
                 (Access_To  => Implementation_Type,
                  Access_All => True)));

         Create_Control_Procedures (Db, Table, Table_Package);
         Create_Search_Procedures (Db, Table, Table_Package);

      end Create_Implementation_Type;

      --------------------
      -- Create_Key_Get --
      --------------------

      procedure Create_Key_Get
        (Base  : Kit.Schema.Tables.Table_Type'Class;
         Key   : Kit.Schema.Keys.Key_Type'Class)
      is
      begin
         if Key.Field_Count = 1
           and then Key.Field (1).Base_Reference
         then
            Public_Get.Create_Unique_Get_Function
              (Table         => Table,
               Key_Table     => Base,
               Table_Package => Table_Package,
               Key_Name      => Key.Standard_Name);
         else
            for Use_Key_Value in Boolean loop
               if Use_Key_Value then
                  Public_Get.Create_Unique_Get_Function
                    (Table         => Table,
                     Key_Table     => Base,
                     Table_Package => Table_Package,
                     Key_Name      => Key.Standard_Name);
               end if;

               Public_Get.Create_Selection_Function
                 (Db            => Db,
                  Table         => Table,
                  Key_Table     => Base,
                  Table_Package => Table_Package,
                  Key_Name      => Key.Standard_Name,
                  Key_Value     => Use_Key_Value,
                  Bounds        => False);
            end loop;

            Public_Get.Create_Selection_Function
              (Db            => Db,
               Table         => Table,
               Key_Table     => Base,
               Table_Package => Table_Package,
               Key_Name      => Key.Standard_Name,
               Key_Value     => True,
               Bounds        => True);

            if Base.Ada_Name = Table.Ada_Name
            --  and then Key.Ada_Name = Table.Ada_Name
              and then Key.Unique
            then
               --  a unique key with the same name as its table is
               --  understood to be a default key
               Public_Get.Create_Default_Key_Functions
                 (Table, Table_Package, Key);

            end if;
         end if;

      end Create_Key_Get;

      --------------------------
      -- Create_Reference_Get --
      --------------------------

      procedure Create_Reference_Get
        (Base : Kit.Schema.Tables.Table_Type'Class)
      is
         use Aquarius.Drys.Expressions;
      begin

         if Base.Ada_Name = Table.Ada_Name then
            declare
               Abstract_Get : constant Subprogram_Declaration'Class :=
                                New_Abstract_Function
                                  ("Reference",
                                   New_Formal_Argument ("Item",
                                     Named_Subtype
                                       (Table.Ada_Name & "_Interface")),
                                   Named_Subtype
                                     (Base.Ada_Name & "_Reference"));
            begin
               Table_Package.Append (Abstract_Get);
               Table_Package.Append (New_Separator);
            end;
         end if;

         declare
            Index_Expression : constant String :=
                                 Table.Database_Index_Component
                                   ("Item", Base);
            Get              : Subprogram_Declaration'Class :=
                                 New_Function
                                   ("Reference",
                                    Base.Ada_Name & "_Reference",
                                    Object (Index_Expression));
         begin

            Get.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");

            Get.Set_Overriding;
            Table_Package.Append_To_Body (Get);
         end;

      end Create_Reference_Get;

      ---------------------------
      -- Create_Selection_Type --
      ---------------------------

      procedure Create_Selection_Type is

         Selection          : Aquarius.Drys.Types.Record_Type_Definition;
         Cursor             : Aquarius.Drys.Types.Record_Type_Definition;
         Constant_Reference : Aquarius.Drys.Types.Record_Type_Definition;
         Variable_Reference : Aquarius.Drys.Types.Record_Type_Definition;
         Iterator_Package   : Aquarius.Drys.Declarations.Package_Type :=
                                Aquarius.Drys.Declarations.New_Package_Type
                                  ("Selection_Iterator_Interfaces");
      begin

         for Mark_Package in Boolean loop
            declare
               Name : constant String :=
                        (if Mark_Package then "Mark" else "Element");
               Type_Name : constant String :=
                             (if Mark_Package
                              then "Marlowe.Btree_Handles.Btree_Mark"
                              else Table.Type_Name);
               List_Package   : Aquarius.Drys.Declarations.Package_Type :=
                                  Aquarius.Drys.Declarations.New_Package_Type
                                    ("List_Of_" & Name & "s");
               Access_Type    : Declaration'Class :=
                                  New_Full_Type_Declaration
                                    (Name & "_Access",
                                     New_Access_Type
                                       (Type_Name,
                                        not Mark_Package));
            begin
               Access_Type.Set_Private_Spec;
               Table_Package.Append (Access_Type);

               List_Package.Set_Generic_Instantiation
                 ("Ada.Containers.Doubly_Linked_Lists");
               List_Package.Add_Generic_Actual_Argument
                 (Name & "_Access");
               List_Package.Set_Private_Spec;
               Table_Package.Append (List_Package);
            end;
         end loop;

         Cursor.Add_Component
           ("Current_Element", "List_Of_Elements.Cursor");
         Cursor.Add_Component
           ("Current_Mark", "List_Of_Marks.Cursor");
         Table_Package.Append
           (New_Private_Type_Declaration
              ("Cursor", Cursor));

         declare
            use Aquarius.Drys.Expressions;
            use Aquarius.Drys.Statements;
            Has_Element_Block : Aquarius.Drys.Blocks.Block_Type;
         begin
--        return List_Of_Marks.Has_Element (Item.Current_Mark)
--          and then Marlowe.Btree_Handles.Valid
--            (List_Of_Marks.Element (Item.Current_Mark).all);


            Has_Element_Block.Add_Statement
              (New_Return_Statement
                 (Operator
                    (Name  => "and then",
                     Left  =>
                       New_Function_Call_Expression
                         ("List_Of_Marks.Has_Element",
                          Object ("Item.Current_Mark")),
                     Right =>
                       New_Function_Call_Expression
                         ("Marlowe.Btree_Handles.Valid",
                          Object
                            ("List_Of_Marks.Element (Item.Current_Mark).all")
                         )
                    )
                 )
              );

            Table_Package.Append
              (New_Function
                 (Name => "Has_Element",
                  Argument =>
                    New_Formal_Argument ("Item", Named_Subtype ("Cursor")),
                  Result_Type => "Boolean",
                  Block    => Has_Element_Block));
         end;

         Constant_Reference.Add_Variant
           ("Element",
            "not null access constant " & Table.Type_Name);

         declare
            Ref_Type : Aquarius.Drys.Declaration'Class :=
                         New_Private_Type_Declaration
                           ("Constant_Reference_Type", Constant_Reference);
         begin
            Ref_Type.Add_Aspect ("Implicit_Dereference",
                                 Object ("Element"));
            Table_Package.Append (Ref_Type);
         end;

         Variable_Reference.Add_Variant
           ("Element",
            "not null access " & Table.Type_Name);
         declare
            Ref_Type : Aquarius.Drys.Declaration'Class :=
                         New_Private_Type_Declaration
                           ("Reference_Type", Variable_Reference);
         begin
            Ref_Type.Add_Aspect ("Implicit_Dereference",
                                 Object ("Element"));
            Table_Package.Append (Ref_Type);
         end;

         declare
            Selection_State : Aquarius.Drys.Types.Record_Type_Definition;
            State_Type      : Aquarius.Drys.Declarations.Type_Declaration;
         begin
            Selection_State.Add_Component
              ("Elements",
               "List_Of_Elements.List");
            Selection_State.Add_Component
              ("Marks",
               "List_Of_Marks.List");
            Selection_State.Add_Component
              ("Mutex",
               "Kit.Mutex.Mutex_Type");
            Selection_State.Set_Limited;

            State_Type :=
              New_Private_Type_Declaration
                ("Selection_State", Selection_State);

            Table_Package.Append (State_Type);
            Table_Package.Append
              (New_Private_Type_Declaration
                 ("Selection_State_Access",
                     New_Access_Type
                       (Access_To  => "Selection_State",
                        Access_All => False)));

         end;

         Selection.Set_Limited;
         Selection.Add_Variant ("Key_Length",
                                "System.Storage_Elements.Storage_Count");
         Selection.Add_Parent
           ("Ada.Finalization.Limited_Controlled");
         Selection.Add_Component
           ("First_Key",
            "System.Storage_Elements.Storage_Array (1 .. Key_Length)");
         Selection.Add_Component
           ("Last_Key",
            "System.Storage_Elements.Storage_Array (1 .. Key_Length)");
         Selection.Add_Component
           ("Key_Ref",
            "Marlowe.Btree_Handles.Btree_Reference");
         Selection.Add_Component
           ("State", "Selection_State_Access");

         Iterator_Package.Set_Generic_Instantiation
           ("Ada.Iterator_Interfaces");
         Iterator_Package.Add_Generic_Actual_Argument ("Cursor");
         Iterator_Package.Add_Generic_Actual_Argument ("Has_Element");
         Table_Package.Append (Iterator_Package);

         declare
            Selection_Type : Type_Declaration :=
                               New_Private_Type_Declaration
                                 ("Selection", Selection, Indefinite => True);
         begin
            Selection_Type.Add_Aspect ("Constant_Indexing",
                                       "Constant_Reference");
            Selection_Type.Add_Aspect ("Variable_Indexing",
                                       "Variable_Reference");
            Selection_Type.Add_Aspect ("Default_Iterator",
                                       "Iterate");
            Selection_Type.Add_Aspect ("Iterator_Element",
                                       Table.Ada_Name & "_Type");

            Table_Package.Append (Selection_Type);
         end;

         declare
            use Aquarius.Drys.Statements;
            Iterate_Block : Aquarius.Drys.Blocks.Block_Type;
            Return_Sequence : Sequence_Of_Statements;
         begin
            Return_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Container",
                  Object ("Container'Unrestricted_Access")));
            Iterate_Block.Add_Statement
              (New_Return_Statement
                 (Return_Variable   => "Result",
                  Variable_Type     => "Iterator",
                  Return_Statements => Return_Sequence));
            Table_Package.Append
              (New_Function
                 (Name => "Iterate",
                  Argument =>
                    New_In_Argument
                      ("Container",
                       Named_Subtype ("Selection")),
                  Result_Type =>
                    "Selection_Iterator_Interfaces.Reversible_Iterator'Class",
                  Block       => Iterate_Block));
         end;

         for Is_Variable in Boolean loop
            declare
               Ref_Block : Aquarius.Drys.Blocks.Block_Type;
            begin
               Ref_Block.Add_Declaration
                 (Aquarius.Drys.Declarations.New_Pragma
                    ("Unreferenced", "Container"));
               Ref_Block.Add_Statement
                 (Aquarius.Drys.Statements.New_Return_Statement
                    (Result =>
                       Object
                         ("(Element => "
                          & "List_Of_Elements.Element "
                          & "(Position.Current_Element))")));
               declare
                  Fn_Name : constant String :=
                              (if Is_Variable
                               then "Variable_"
                               else "Constant_")
                              & "Reference";
                  Ref_Fn : Subprogram_Declaration'Class :=
                             New_Function
                               (Fn_Name,
                                Named_Subtype
                                  ((if Is_Variable
                                   then ""
                                   else "Constant_")
                                   & "Reference_Type"),
                                Ref_Block);
                  Container : Formal_Argument'Class :=
                                New_Formal_Argument
                                  ("Container",
                                   (if Is_Variable
                                    then Inout_Argument
                                    else In_Argument),
                                   Named_Subtype
                                     ("Selection"));
                  Position : constant Formal_Argument'Class :=
                                New_In_Argument
                                  ("Position",
                                   Named_Subtype ("Cursor"));
                  Inline    : Declaration'Class :=
                                New_Pragma ("Inline", Fn_Name);
               begin
                  Container.Set_Aliased;
                  Ref_Fn.Add_Formal_Argument (Container);
                  Ref_Fn.Add_Formal_Argument (Position);
                  Table_Package.Append (Ref_Fn);

                  Inline.Set_Private_Spec;
                  Table_Package.Append (Inline);
               end;
            end;
         end loop;

         declare
            use Aquarius.Drys.Statements;
            Free_Element_Sequence  : Sequence_Of_Statements;
            Free_Mark_Sequence     : Sequence_Of_Statements;
            Finalize_Block         : Aquarius.Drys.Blocks.Block_Type;
            Free_Element           : Subprogram_Declaration'Class :=
                                       Instantiate_Generic_Procedure
                                         (Instantiated_Name => "Free",
                                          Generic_Name      =>
                                            "Ada.Unchecked_Deallocation");
            Free_State             : Subprogram_Declaration'Class :=
                                       Instantiate_Generic_Procedure
                                         (Instantiated_Name => "Free",
                                          Generic_Name      =>
                                            "Ada.Unchecked_Deallocation");
         begin
            Free_Element.Add_Generic_Actual_Argument
              (Table.Ada_Name & "_Type");
            Free_Element.Add_Generic_Actual_Argument
              ("Element_Access");
            Finalize_Block.Add_Declaration (Free_Element);

            Free_State.Add_Generic_Actual_Argument
              ("Selection_State");
            Free_State.Add_Generic_Actual_Argument
              ("Selection_State_Access");
            Finalize_Block.Add_Declaration (Free_State);

            Free_Element_Sequence.Append
              (New_Procedure_Call_Statement
                 ("Free", Object ("X")));
            Free_Mark_Sequence.Append
              (New_Procedure_Call_Statement
                 ("Free", Object ("X")));
            Finalize_Block.Add_Statement
              (Item =>
                 Aquarius.Drys.Statements.Iterate
                   (Loop_Variable  => "X",
                    Container_Name => "Object.State.Elements",
                    Iterate_Body   => Free_Element_Sequence));
            Finalize_Block.Add_Statement
              (Item =>
                 Aquarius.Drys.Statements.Iterate
                   (Loop_Variable  => "X",
                    Container_Name => "Object.State.Marks",
                    Iterate_Body   => Free_Mark_Sequence));
            Finalize_Block.Add_Statement
              (Aquarius.Drys.Statements.New_Procedure_Call_Statement
                 ("Free", Object ("Object.State")));

            declare
               Finalize : Subprogram_Declaration'Class :=
                            New_Procedure
                              (Name     => "Finalize",
                               Argument =>
                                 New_Inout_Argument
                                   (Name          => "Object",
                                    Argument_Type =>
                                      Named_Subtype ("Selection")),
                               Block    => Finalize_Block);
            begin
               Finalize.Set_Overriding;
               Finalize.Set_Private_Spec;
               Table_Package.Append (Finalize);
            end;
         end;

         declare
            use Aquarius.Drys.Expressions;
            use Aquarius.Drys.Statements;
            Is_Empty : constant Subprogram_Declaration'Class :=
                         New_Function
                           (Name        => "Is_Empty",
                            Argument    =>
                              New_In_Argument
                                (Name          => "Container",
                                 Argument_Type =>
                                   Named_Subtype ("Selection")),
                            Result_Type => "Boolean",
                            Block       =>
                              Aquarius.Drys.Blocks.Create_Block
                                (New_Return_Statement
                                     (New_Function_Call_Expression
                                          ("Has_Element",
                                           Object ("Container.First"))
                                     )
                                )
                           );
         begin
            Table_Package.Append (Is_Empty);
         end;

         declare
            use Aquarius.Drys.Expressions;
            use Aquarius.Drys.Statements;
            Element : constant Subprogram_Declaration'Class :=
                         New_Function
                           (Name        => "Element",
                            Argument    =>
                              New_In_Argument
                                (Name          => "Item",
                                 Argument_Type =>
                                   Named_Subtype ("Cursor")),
                            Result_Type => Table.Ada_Name & "_Type",
                            Block       =>
                              Aquarius.Drys.Blocks.Create_Block
                                (New_Return_Statement
                                     (Object
                                          ("List_Of_Elements.Element "
                                           & "(Item.Current_Element).all")
                                     )
                                )
                           );
         begin
            Table_Package.Append (Element);
         end;

         declare
            use Aquarius.Drys.Expressions;
            use Aquarius.Drys.Statements;
            Block : Aquarius.Drys.Blocks.Block_Type;
         begin
            Block.Add_Declaration
              (New_Object_Declaration
                 ("Result", "Natural", Literal (0)));
            Block.Add_Declaration
              (New_Object_Declaration
                 ("It", "Cursor", Object ("Container.First")));

            declare
               While_Sequence : Sequence_Of_Statements;
            begin
               While_Sequence.Append
                 (New_Assignment_Statement
                    ("Result",
                     Operator ("+", Object ("Result"), Literal (1))));
               While_Sequence.Append
                 (New_Procedure_Call_Statement
                    ("Next", Object ("It")));
               Block.Add_Statement
                 (Aquarius.Drys.Statements.While_Statement
                    (Condition  =>
                       New_Function_Call_Expression
                         ("Has_Element", "It"),
                     While_Body => While_Sequence));
            end;

            Block.Add_Statement
              (New_Return_Statement
                 (Object ("Result")));

            Table_Package.Append
              (New_Function
                 (Name        => "Length",
                  Argument    =>
                    New_In_Argument
                      (Name          => "Container",
                       Argument_Type =>
                         Named_Subtype ("Selection")),
                  Result_Type => "Natural",
                  Block       => Block));
         end;

      end Create_Selection_Type;

   begin

      Table_Package.With_Package ("Ada.Containers.Doubly_Linked_Lists",
                                  Private_With => True);
      Table_Package.With_Package ("Ada.Finalization", Private_With => True);

      Table_Package.With_Package ("Ada.Unchecked_Deallocation",
                                  Body_With => True);

      Table_Package.With_Package ("System.Storage_Elements",
                                  Private_With => True);

      Table_Package.With_Package ("Marlowe.Btree_Handles",
                                  Private_With => True);

      Table_Package.With_Package ("Ada.Iterator_Interfaces");
      if Table.Has_Key_Field then
         Table_Package.With_Package ("Marlowe.Key_Storage",
                                     Body_With => True);
      end if;

      Table_Package.With_Package ("Kit.Cache",
                                  Body_With => True);

      if Table.Has_String_Type then
         Table_Package.With_Package ("Kit.Strings",
                                     Body_With => True);
      end if;

      Table_Package.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                                  Body_With => True);

      Table.Iterate (Add_Base_With'Access, Inclusive => False);
      Table.Iterate_All (Add_Field_Type_With'Access,
                         Table_First => True);

      Table_Interface.Set_Limited;
      Table_Interface.Add_Parent ("Record_Interface");
      Table_Interface.Add_Parent ("Search_Interface");

      Table.Iterate (Add_Base'Access, Inclusive => False);

      Table_Package.With_Package
        (Db.Ada_Name & "." & Database_Package,
         Body_With => True);

      Table_Package.With_Package
        (Db.Ada_Name & "." & Cache_Package,
         Body_With => True);

      if Kit.Options.Generate_Debug then
         Table_Package.With_Package
           ("Ada.Text_IO",
            Body_With => True);
      end if;

      Create_Field_Constants (Table, Table_Package);

      Table_Package.Append
        (New_Full_Type_Declaration
           (Table.Interface_Name,
            Table_Interface));
      Table_Package.Append
        (New_Separator);

      Table_Package.Append
        (New_Subtype_Declaration
           (Table.Type_Name,
            Aquarius.Drys.Class_Wide_Subtype
              (Table.Interface_Name)));

      --  Create_Key_Marks (Db, Table, Table_Package);
      --  Create_Key_Context_Type (Db, Table, Table_Package);

      declare
--           Mark_Access : constant Access_Type_Definition :=
--                           New_Access_Type
--                             ("Marlowe.Btree_Handles.Btree_Mark",
--                              False);
         Free        : Subprogram_Declaration'Class :=
                         Instantiate_Generic_Procedure
                           (Instantiated_Name => "Free",
                            Generic_Name      => "Ada.Unchecked_Deallocation");
      begin

--           Table_Package.Append_To_Body
--             (New_Full_Type_Declaration
--                ("Mark_Access", Mark_Access));

         Free.Add_Generic_Actual_Argument
           ("Marlowe.Btree_Handles.Btree_Mark");
         Free.Add_Generic_Actual_Argument
           ("Mark_Access");

         Table_Package.Append_To_Body (Free);
      end;

      Create_Implementation_Type;

      Table_Package.Append
        (New_Separator);

      Create_Overrides (Db, Table, Table_Package);

      Table.Iterate (Create_Reference_Get'Access,
                     Inclusive   => True,
                     Table_First => False);

      Table.Iterate_All (Add_Fetch'Access);
      Table.Iterate_All (Add_Store'Access);

      Add_Create_Function;

      Create_Selection_Type;

      Public_Get.Create_Get_From_Index (Table, Table_Package);

      Create_Identity_Function (Table, Table_Package);
      Create_Generic_Get (Table, Table_Package);
      Create_Generic_Set (Table, Table_Package);

      Table.Scan_Keys (Create_Key_Get'Access,
                       Include_Base_Keys => True);

--        if Table.Has_Key_Field then
--           Public_Get.Create_Generic_Get_Function
--             (Db, Table, Table_Package, Key_Value => False);
--           Public_Get.Create_Generic_Get_Function
--             (Db, Table, Table_Package, Key_Value => True);
--        end if;

      Public_Get.Create_Reference_Get_Function
        (Db, Table, Table_Package);

      Public_Get.Create_Iterator (Table, Table_Package);

      return Table_Package;
   end Generate_Public_Interface;

end Kit.Generate.Public_Interface;
