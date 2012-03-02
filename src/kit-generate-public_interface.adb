with Ada.Strings.Fixed;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Kit.Fields;
with Kit.Generate.Fetch;
with Kit.Generate.Public_Get;
with Kit.Types;

package body Kit.Generate.Public_Interface is

   procedure Create_Control_Procedures
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Key_Context_Type
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Key_Marks
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Locking_Procedures
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Overrides
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Search_Procedures
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Compound_Key_To_Storage_Functions
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Field_Store_Procedure
     (Table : in     Kit.Tables.Table_Type'Class;
      Base  : in     Kit.Tables.Table_Type'Class;
      Field : in     Kit.Fields.Field_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Generic_Get
     (Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Generic_Set
     (Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   ----------------------------------------------
   -- Create_Compound_Key_To_Storage_Functions --
   ----------------------------------------------

   procedure Create_Compound_Key_To_Storage_Functions
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);

      procedure Create_Key_To_Storage (Key : Kit.Tables.Key_Cursor);
      function To_Storage_Expression
        (Key   : Kit.Tables.Key_Cursor;
         Index : Positive)
         return Aquarius.Drys.Expression'Class;

      ---------------------------
      -- Create_Key_To_Storage --
      ---------------------------

      procedure Create_Key_To_Storage (Key : Kit.Tables.Key_Cursor) is
         use Kit.Tables;
      begin
         if Is_Compound_Key (Key) then
            declare
               use Aquarius.Drys, Aquarius.Drys.Declarations;
               Fn : Subprogram_Declaration'Class :=
                      Aquarius.Drys.Declarations.New_Function
                        (Ada_Name (Key) & "_To_Storage",
                         "System.Storage_Elements.Storage_Array",
                         To_Storage_Expression
                           (Key, Compound_Field_Count (Key)));
            begin
               for I in 1 .. Compound_Field_Count (Key) loop
                  Fn.Add_Formal_Argument
                    (Compound_Field (Key, I).Ada_Name,
                     Compound_Field (Key, I).Get_Field_Type.Argument_Subtype);
               end loop;
               Fn.Add_Local_Declaration
                 (Use_Type ("System.Storage_Elements.Storage_Array"));
               Top.Append_To_Body (Fn);
            end;
         end if;
      end Create_Key_To_Storage;

      ---------------------------
      -- To_Storage_Expression --
      ---------------------------

      function To_Storage_Expression
        (Key   : Kit.Tables.Key_Cursor;
         Index : Positive)
         return Aquarius.Drys.Expression'Class
      is
         use Aquarius.Drys.Expressions;
         Field : constant Kit.Fields.Field_Type'Class :=
                   Kit.Tables.Compound_Field (Key, Index);
         This  : constant Aquarius.Drys.Expression'Class :=
                   Field.Get_Field_Type.To_Storage_Array
                     (Field.Ada_Name);
      begin
         if Index = 1 then
            return This;
         else
            return Operator ("&", This,
                             To_Storage_Expression (Key, Index - 1));
         end if;
      end To_Storage_Expression;

   begin
      Table.Scan_Keys (Create_Key_To_Storage'Access);
   end Create_Compound_Key_To_Storage_Functions;

   -------------------------------
   -- Create_Control_Procedures --
   -------------------------------

   procedure Create_Control_Procedures
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
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

         procedure Insert_Table_Keys (Base  : Kit.Tables.Table_Type'Class);


         -----------------------
         -- Insert_Table_Keys --
         -----------------------

         procedure Insert_Table_Keys
           (Base  : Kit.Tables.Table_Type'Class)
         is
            procedure Insert_Key (Key_Table : Kit.Tables.Table_Type'Class;
                                  Key       : Kit.Tables.Key_Cursor);

            ----------------
            -- Insert_Key --
            ----------------

            procedure Insert_Key (Key_Table : Kit.Tables.Table_Type'Class;
                                  Key       : Kit.Tables.Key_Cursor)
            is
               pragma Unreferenced (Key_Table);
               Insert : Procedure_Call_Statement :=
                          New_Procedure_Call_Statement
                            ("Marlowe.Btree_Handles.Insert");
            begin
               Insert.Add_Actual_Argument ("Marlowe_Keys.Handle");
               Insert.Add_Actual_Argument ("Marlowe_Keys."
                                           & Base.Ada_Name
                                           & "_"
                                           & Kit.Tables.Ada_Name (Key)
                                           & "_Ref");
               Insert.Add_Actual_Argument
                 (Kit.Tables.To_Storage (Table       => Table,
                                         Base_Table  => Base,
                                         Object_Name => "Item",
                                         Key         => Key));
               Insert_Keys.Append (Insert);
            end Insert_Key;

         begin
            Base.Scan_Keys (Insert_Key'Access);
         end Insert_Table_Keys;

      begin

         Finalize_Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

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

         declare
            Finalize : Subprogram_Declaration :=
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
            Initialize : Subprogram_Declaration :=
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

   ----------------------------------
   -- Create_Field_Store_Procedure --
   ----------------------------------

   procedure Create_Field_Store_Procedure
     (Table : in     Kit.Tables.Table_Type'Class;
      Base  : in     Kit.Tables.Table_Type'Class;
      Field : in     Kit.Fields.Field_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      Store_Block : Aquarius.Drys.Blocks.Block_Type;

      procedure Create_Abstract_Store;

      procedure Lock_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key        : Kit.Tables.Key_Cursor);

      procedure Unlock_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key        : Kit.Tables.Key_Cursor);

      procedure Perform_Lock
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor;
         Lock     : Boolean);

      procedure Delete_Old_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor);

      procedure Insert_New_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor);

      procedure Key_Operation
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key       : Kit.Tables.Key_Cursor;
         Operation : String);

      procedure Release_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor);

      procedure Process_Keys
        (Process : not null access
           procedure (Table_Base : Kit.Tables.Table_Type'Class;
                      Key_Base : Kit.Tables.Table_Type'Class;
                      Key : Kit.Tables.Key_Cursor));

      ---------------------------
      -- Create_Abstract_Store --
      ---------------------------

      procedure Create_Abstract_Store is
         use Aquarius.Drys.Declarations;
         Store : Subprogram_Declaration :=
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
         Top.Append (New_Separator);
      end Create_Abstract_Store;

      --------------------
      -- Delete_Old_Key --
      --------------------

      procedure Delete_Old_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Delete");
      end Delete_Old_Key;

      --------------------
      -- Insert_New_Key --
      --------------------

      procedure Insert_New_Key
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Insert");
      end Insert_New_Key;

      -------------------
      -- Key_Operation --
      -------------------

      procedure Key_Operation
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key       : Kit.Tables.Key_Cursor;
         Operation : String)
      is
         S : Aquarius.Drys.Statements.Procedure_Call_Statement :=
               Aquarius.Drys.Statements.New_Procedure_Call_Statement
                 ("Marlowe.Btree_Handles." & Operation);
      begin

         S.Add_Actual_Argument
           ("Marlowe_Keys.Handle");
         S.Add_Actual_Argument
           ("Marlowe_Keys."
            & Table_Base.Ada_Name
            & "_"
            & Kit.Tables.Ada_Name (Key)
            & "_Ref");
         S.Add_Actual_Argument
           (Aquarius.Drys.Expressions.Operator
              ("&",
               Key_Base.Key_To_Storage (Key, "Item"),
               Aquarius.Drys.Expressions.New_Function_Call_Expression
                 ("Marlowe.Key_Storage.To_Storage_Array",
                  Table.Database_Index_Component ("Item", Key_Base))));

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
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor)
      is
      begin
         Perform_Lock (Table_Base, Key_Base, Key, True);
      end Lock_Key;

      ------------------
      -- Perform_Lock --
      ------------------

      procedure Perform_Lock
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor;
         Lock     : Boolean)
      is
         pragma Unreferenced (Key_Base);
         Lock_Name : constant String :=
                       (if Lock then "Lock" else "Unlock");
         S : constant Aquarius.Drys.Statement'Class :=
               Aquarius.Drys.Statements.New_Procedure_Call_Statement
                 (Table_Base.Ada_Name
                  & "_Impl." & Kit.Tables.Ada_Name (Key)
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
           procedure (Table_Base : Kit.Tables.Table_Type'Class;
                      Key_Base : Kit.Tables.Table_Type'Class;
                      Key : Kit.Tables.Key_Cursor))
      is

         procedure Process_Base
           (Base : Kit.Tables.Table_Type'Class);

         ------------------
         -- Process_Base --
         ------------------

         procedure Process_Base
           (Base : Kit.Tables.Table_Type'Class)
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
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor)
      is
         pragma Unreferenced (Table_Base);
         pragma Unreferenced (Key_Base);
         use Aquarius.Drys;
         use Aquarius.Drys.Expressions;
         use Aquarius.Drys.Statements;
         Not_Created    : constant Expression'Class :=
                            Operator
                              ("not", Aquarius.Drys.Object ("Item.Created"));
         Using_Key      : constant Expression'Class :=
                            Object ("Item.Using_Key");
         This_Key       : constant Expression'Class :=
                            Operator
                              ("=",
                               Object ("Item.Key_Value.K"),
                               Object ("K_" & Table.Ada_Name & "_"
                                 & Kit.Tables.Ada_Name (Key)));
         Release        : constant Statement'Class :=
                            New_Procedure_Call_Statement
                              ("Marlowe.Btree_Handles.Release",
                               Object
                                 ("Item.Key_Value."
                                  & Kit.Tables.Ada_Name (Key)
                                  & "_Context"));
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
        (Table_Base : Kit.Tables.Table_Type'Class;
         Key_Base   : Kit.Tables.Table_Type'Class;
         Key      : Kit.Tables.Key_Cursor)
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
      Table.Scan_Keys (Field, Release_Key'Access);
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
         Store : Subprogram_Declaration :=
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
   end Create_Field_Store_Procedure;

   ------------------------
   -- Create_Generic_Get --
   ------------------------

   procedure Create_Generic_Get
     (Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Statements;

      Fetch  : Subprogram_Declaration;
      Block  : Aquarius.Drys.Blocks.Block_Type;
      Choose : Case_Statement_Record'Class :=
                 Case_Statement ("Field");

      procedure Select_Field
        (Base  : Kit.Tables.Table_Type'Class;
         Field : Kit.Fields.Field_Type'Class);

      ------------------
      -- Select_Field --
      ------------------

      procedure Select_Field
        (Base  : Kit.Tables.Table_Type'Class;
         Field : Kit.Fields.Field_Type'Class)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         Seq.Append
           (New_Return_Statement
              (Kit.Types.Convert_To_String
                 (Field.Get_Field_Type,
                  "Item." & Field.Ada_Name)));
         Choose.Add_Case_Option
           (Value => "F_" & Field.Ada_Name,
            Stats => Seq);
      end Select_Field;

   begin

      Table.Iterate_All (Select_Field'Access);

      declare
         Others_Seq : Sequence_Of_Statements;
      begin
         Others_Seq.Append
           (Raise_Statement
              ("Constraint_Error",
               "no such field"));
         Choose.Add_Others_Option (Others_Seq);
      end;

      Block.Append (Choose);

      Fetch := New_Function
        ("Get",
         Named_Subtype ("String"),
         Block);

      Fetch.Add_Formal_Argument
        ("Item",
         Table.Ada_Name & "_Implementation");
      Fetch.Add_Formal_Argument
        ("Field",
         "Database_Field");

      Fetch.Set_Overriding;
      Top.Append_To_Body (Fetch);
   end Create_Generic_Get;

   procedure Create_Generic_Set
     (Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Statements;

      Store  : Subprogram_Declaration;
      Block  : Aquarius.Drys.Blocks.Block_Type;
      Choose : Case_Statement_Record'Class :=
                 Case_Statement ("Field");

      procedure Set_Field
        (Base  : Kit.Tables.Table_Type'Class;
         Field : Kit.Fields.Field_Type'Class);

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Base  : Kit.Tables.Table_Type'Class;
         Field : Kit.Fields.Field_Type'Class)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         Seq.Append
           (New_Procedure_Call_Statement
              ("Item.Set_" & Field.Ada_Name,
               Kit.Types.Convert_From_String
                 (Field.Get_Field_Type,
                  "Value")));
         Choose.Add_Case_Option
           (Value => "F_" & Field.Ada_Name,
            Stats => Seq);
      end Set_Field;

   begin

      Table.Iterate_All (Set_Field'Access);

      declare
         Others_Seq : Sequence_Of_Statements;
      begin
         Others_Seq.Append
           (Raise_Statement
              ("Constraint_Error",
               "no such field"));
         Choose.Add_Others_Option (Others_Seq);
      end;

      Block.Append (Choose);

      Store := New_Procedure
        ("Set",
         Block);

      Store.Add_Formal_Argument
        ("Item",
         Inout_Argument,
         Table.Ada_Name & "_Implementation");
      Store.Add_Formal_Argument
        ("Field",
         "Database_Field");
      Store.Add_Formal_Argument
        ("Value",
         "String");

      Store.Set_Overriding;
      Top.Append_To_Body (Store);
   end Create_Generic_Set;

   -----------------------------
   -- Create_Key_Context_Type --
   -----------------------------

   procedure Create_Key_Context_Type
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);

      Context_Type : Aquarius.Drys.Types.Record_Type_Definition;

      procedure Create_Mark_Component (Base : Kit.Tables.Table_Type'Class;
                                       Key  : Kit.Tables.Key_Cursor);

      ---------------------------
      -- Create_Mark_Component --
      ---------------------------

      procedure Create_Mark_Component (Base : Kit.Tables.Table_Type'Class;
                                       Key  : Kit.Tables.Key_Cursor)
      is
         pragma Unreferenced (Base);
      begin
         Context_Type.Next_Case_Option
           ("K_" & Table.Ada_Name & "_" & Kit.Tables.Ada_Name (Key));
         Context_Type.Add_Component
           (Kit.Tables.Ada_Name (Key) & "_Context",
            Kit.Tables.Ada_Name (Key) & "_Mark");
      end Create_Mark_Component;

   begin
      Context_Type.Add_Variant ("K",
                                Table.Ada_Name & "_Key",
                                "K_None");
      Context_Type.Start_Case ("K_None");
      Table.Scan_Keys (Create_Mark_Component'Access);
      Context_Type.End_Case;

      Top.Append_To_Body
        (Aquarius.Drys.Declarations.New_Full_Type_Declaration
           ("Context_Key_Value",
            Context_Type));

   end Create_Key_Context_Type;

   ----------------------
   -- Create_Key_Marks --
   ----------------------

   procedure Create_Key_Marks
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      procedure Create_Mark (Base : Kit.Tables.Table_Type'Class;
                             Key  : Kit.Tables.Key_Cursor);

      procedure Create_Mark (Base : Kit.Tables.Table_Type'Class;
                             Key  : Kit.Tables.Key_Cursor)
      is
         pragma Unreferenced (Base);
         use Aquarius.Drys.Declarations;
         Mark_Subtype : constant Subtype_Declaration :=
                          New_Subtype_Declaration
                            (Kit.Tables.Ada_Name (Key) & "_Mark",
                             Aquarius.Drys.Named_Subtype
                               ("Marlowe.Btree_Handles.Btree_Mark ("
                                & Ada.Strings.Fixed.Trim
                                  (Positive'Image (Kit.Tables.Key_Size (Key)),
                                   Ada.Strings.Left)
                                & ")"));
      begin
         Top.Append_To_Body (Mark_Subtype);
      end Create_Mark;

   begin
      Table.Scan_Keys (Create_Mark'Access);
   end Create_Key_Marks;

   -------------------------------
   -- Create_Locking_Procedures --
   -------------------------------

   procedure Create_Locking_Procedures
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
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

      procedure Unlock (T : Kit.Tables.Table_Type'Class);
      procedure S_Lock (T : Kit.Tables.Table_Type'Class);
      procedure X_Lock (T : Kit.Tables.Table_Type'Class);

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

      procedure S_Lock (T : Kit.Tables.Table_Type'Class) is
      begin
         S_Lock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.S_Lock"));
      end S_Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (T : Kit.Tables.Table_Type'Class) is
      begin
         Unlock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.Unlock"));
      end Unlock;

      ------------
      -- X_Lock --
      ------------

      procedure X_Lock (T : Kit.Tables.Table_Type'Class) is
      begin
         X_Lock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item.T" & T.Index_Image
               & "_Data.X_Lock"));
      end X_Lock;

   begin

      Unlock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (Unlock'Access,
                     Inclusive => True,
                     Table_First => False);
      Unlock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      Unlock_Block.Add_Statement ("Item.S_Locked := False");
      Unlock_Block.Add_Statement ("Item.X_Locked := False");

      Add_Locker ("Unlock", Unlock_Block);

      S_Lock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (S_Lock'Access,
                     Inclusive => True,
                     Table_First => False);
      S_Lock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      S_Lock_Block.Add_Statement ("Item.S_Locked := True");

      Add_Locker ("S_Lock", S_Lock_Block);

      X_Lock_Block.Add_Statement ("Kit.Cache.Lock_Cache");
      Table.Iterate (X_Lock'Access,
                     Inclusive => True,
                     Table_First => False);
      X_Lock_Block.Add_Statement ("Kit.Cache.Unlock_Cache");
      X_Lock_Block.Add_Statement ("Item.X_Locked := True");

      Add_Locker ("X_Lock", X_Lock_Block);

   end Create_Locking_Procedures;

   ----------------------
   -- Create_Overrides --
   ----------------------

   procedure Create_Overrides
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
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
     (Db    : in     Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;

      procedure Create_Has_Element;
      procedure Create_Next;

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
         Key_Case          : Case_Statement_Record'Class :=
                               Case_Statement ("Item.Key_Value.K");

         procedure Add_Key_Case (Base : Kit.Tables.Table_Type'Class;
                                 Key  : Kit.Tables.Key_Cursor);

         ------------------
         -- Add_Key_Case --
         ------------------

         procedure Add_Key_Case (Base : Kit.Tables.Table_Type'Class;
                                 Key  : Kit.Tables.Key_Cursor)
         is
            pragma Unreferenced (Base);
            Key_Context : constant String :=
                            "Item.Key_Value."
                              & Tables.Ada_Name (Key) & "_Context";
            Sequence : Sequence_Of_Statements;
         begin
            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Marlowe.Btree_Handles.Next",
                  Object (Key_Context)));
            Sequence.Append
              (New_Assignment_Statement
                 ("Got_Valid_Index",
                  New_Function_Call_Expression
                    ("Marlowe.Btree_Handles.Valid", Key_Context)));
            Sequence.Append
              (If_Statement
                 (Object ("Got_Valid_Index"),
                  New_Assignment_Statement
                    ("Item.Index",
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Database_Index",
                        New_Function_Call_Expression
                          ("Marlowe.Btree_Handles.Get_Key",
                           Key_Context)))));
            Key_Case.Add_Case_Option
              ("K_" & Table.Ada_Name & "_" & Tables.Ada_Name (Key),
               Sequence);
         end Add_Key_Case;

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

         declare
            Index_Scan : Sequence_Of_Statements;
         begin

            declare
               Valid_Index : constant Expression'Class :=
                               New_Function_Call_Expression
                                 ("Marlowe.Btree_Handles.Valid_Index",
                                  "Marlowe_Keys.Handle",
                                  Table.Ada_Name & "_Table_Index",
                                  "Item.Index");
               Is_Deleted  : constant Expression'Class :=
                               New_Function_Call_Expression
                                 ("Marlowe.Btree_Handles.Deleted_Record",
                                  "Marlowe_Keys.Handle",
                                  Table.Ada_Name & "_Table_Index",
                                  "Item.Index");
               Condition   : constant Expression'Class :=
                               Operator ("and then",
                                         Valid_Index, Is_Deleted);
               Increment   : constant Statement'Class :=
                     New_Assignment_Statement
                                 ("Item.Index",
                                  Operator
                                    ("+",
                                     Object ("Item.Index"),
                                     Literal (1)));
            begin
               Index_Scan.Append (Increment);
               Index_Scan.Append
                 (While_Statement
                    (Condition, Increment));
               Index_Scan.Append
                 (New_Assignment_Statement
                    ("Got_Valid_Index",
                     Valid_Index));
            end;

            Key_Case.Add_Case_Option
              ("K_None", Index_Scan);

            Table.Scan_Keys (Add_Key_Case'Access);

            Next_Block.Add_Statement (Key_Case);
--
--                (If_Statement
--                   (Operator ("not", Object ("Item.Using_Key")),
--                    Index_Scan,
--                    Key_Scan));
         end;

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
      Create_Next;
   end Create_Search_Procedures;

   -------------------------------
   -- Generate_Public_Interface --
   -------------------------------

   function Generate_Public_Interface
     (Db    : in out Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
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

      Table_Package : Aquarius.Drys.Declarations.Package_Type'Class :=
        Top.New_Child_Package (Table.Ada_Name);
      Table_Interface : Aquarius.Drys.Interface_Type_Definition;

      procedure Add_Base_With (It : Kit.Tables.Table_Type'Class);
      procedure Add_Base (It : Kit.Tables.Table_Type'Class);

      procedure Add_Fetch (Base  : Kit.Tables.Table_Type'Class;
                           Field : Kit.Fields.Field_Type'Class);
      procedure Add_Store (Base  : Kit.Tables.Table_Type'Class;
                           Field : Kit.Fields.Field_Type'Class);

      procedure Create_Key_Get (Base  : Kit.Tables.Table_Type'Class;
                                Key   : Kit.Tables.Key_Cursor);
      procedure Create_Reference_Get (Base : Kit.Tables.Table_Type'Class);
      procedure Create_Actual_Type_Fetch;

      procedure Add_Create_Function;

      procedure Create_Implementation_Type;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base (It : Kit.Tables.Table_Type'Class) is
      begin
         Table_Interface.Add_Parent
           (Db.Ada_Name & "." & It.Ada_Name & "." &
              It.Ada_Name & "_Interface");
      end Add_Base;

      -------------------
      -- Add_Base_With --
      -------------------

      procedure Add_Base_With (It : Kit.Tables.Table_Type'Class) is
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
      end Add_Base_With;

      -------------------------
      -- Add_Create_Function --
      -------------------------

      procedure Add_Create_Function is

         use Aquarius.Drys.Statements;

         Sequence : Sequence_Of_Statements;

         procedure Allocate_Context (Base : Kit.Tables.Table_Type'Class);

         procedure Database_Insert (Base : Kit.Tables.Table_Type'Class);

         procedure Set_Field (Field_Name : String;
                              Value      : String);

         ----------------------
         -- Allocate_Context --
         ----------------------

         procedure Allocate_Context (Base : Kit.Tables.Table_Type'Class) is
            use Aquarius.Drys.Expressions;
         begin
            Sequence.Append
              (New_Assignment_Statement
                 ("Result" & Base.Base_Component_Name,
                  New_Allocation_Expression
                    (Base.Ada_Name & "_Cache.Cache_Record")));
            Sequence.Append
              (New_Assignment_Statement
                 ("Result" & Base.Base_Component_Name
                  & ".Db.Actual_Type",
                  Object ("R_" & Table.Ada_Name)));
         end Allocate_Context;

         ---------------------
         -- Database_Insert --
         ---------------------

         procedure Database_Insert (Base : Kit.Tables.Table_Type'Class) is
            use Aquarius.Drys.Expressions;

            Index_Field : constant String :=
                            Table.Database_Index_Component
                              ("Result", Base);

            procedure Set_Base_Index
              (Meta_Base : Kit.Tables.Table_Type'Class);

            --------------------
            -- Set_Base_Index --
            --------------------

            procedure Set_Base_Index
              (Meta_Base : Kit.Tables.Table_Type'Class)
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
                    ("Marlowe.Btree_Handles.Insert_Record",
                     Object ("Marlowe_Keys.Handle"),
                     Literal (Integer (Base.Reference_Index)))));
            if Base.Ada_Name /= Table.Ada_Name then
               Base.Iterate (Set_Base_Index'Access, Inclusive   => False);
            end if;

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Result" & Base.Base_Component_Name & ".Initialise",
                  Literal (Integer (Base.Reference_Index)),
                  Object (Index_Field)));

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Kit.Cache.Insert",
                  New_Function_Call_Expression
                    ("Kit.Cache.Cache_Entry",
                     "Result" & Base.Base_Component_Name)));

            Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl.Write",
                  Object (Index_Field),
                  Object ("Result" & Base.Base_Component_Name & ".Db")));

            Sequence.Append
              ("Result" & Base.Base_Component_Name & ".X_Lock");

            Sequence.Append (Base.Ada_Name & "_Impl.File_Mutex.Unlock");
         end Database_Insert;

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

         Table_Package.Append (New_Separator);
      end Add_Create_Function;

      ---------------
      -- Add_Fetch --
      ---------------

      procedure Add_Fetch (Base  : Kit.Tables.Table_Type'Class;
                           Field : Kit.Fields.Field_Type'Class)
      is
      begin
         if Base.Name = Table.Name then
            declare
               Fetch : constant Subprogram_Declaration :=
                         New_Abstract_Function
                           (Field.Ada_Name,
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
            Fetch  : Subprogram_Declaration;
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

            Fetch := New_Function
              (Field.Ada_Name,
               Field.Get_Field_Type.Return_Subtype,
               Block);

            Fetch.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");

            Fetch.Set_Overriding;
            Table_Package.Append_To_Body (Fetch);
         end;

      end Add_Fetch;

      ---------------
      -- Add_Store --
      ---------------

      procedure Add_Store (Base  : Kit.Tables.Table_Type'Class;
                           Field : Kit.Fields.Field_Type'Class)
      is
      begin
         Create_Field_Store_Procedure
           (Table => Table,
            Base  => Base,
            Field => Field,
            Top   => Table_Package);
      end Add_Store;

      ------------------------------
      -- Create_Actual_Type_Fetch --
      ------------------------------

      procedure Create_Actual_Type_Fetch is
         Get              : Subprogram_Declaration :=
                              New_Function
                                ("Top_Record",
                                 "Record_Type",
                                 Object
                                   ("Item"
                                    & Table.Base_Component_Name
                                    & ".Db.Actual_Type"));
      begin

         Get.Add_Formal_Argument
           ("Item",
            Table.Ada_Name & "_Implementation");

         Get.Set_Overriding;
         Table_Package.Append_To_Body (Get);

      end Create_Actual_Type_Fetch;

      --------------------------------
      -- Create_Implementation_Type --
      --------------------------------

      procedure Create_Implementation_Type is

         Record_Defn : Aquarius.Drys.Types.Record_Type_Definition;
         Context_Defn : Aquarius.Drys.Types.Record_Type_Definition;

         procedure Add_Base_Component
           (It : Kit.Tables.Table_Type'Class);

         ------------------------
         -- Add_Base_Component --
         ------------------------

         procedure Add_Base_Component
           (It : Kit.Tables.Table_Type'Class)
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
         Record_Defn.Add_Component ("Key_Value", "Context_Key_Value");
         Record_Defn.Add_Component ("Index", "Marlowe.Database_Index");
         Record_Defn.Add_Component ("Local",
                                    "Local_Lock_Context");
         Record_Defn.Add_Component ("Link",
                                    "Lock_Context");

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              (Implementation_Type, Record_Defn));

         Create_Control_Procedures (Db, Table, Table_Package);
         Create_Search_Procedures (Db, Table, Table_Package);

      end Create_Implementation_Type;

      --------------------
      -- Create_Key_Get --
      --------------------

      procedure Create_Key_Get (Base  : Kit.Tables.Table_Type'Class;
                                Key   : Kit.Tables.Key_Cursor)
      is
         pragma Unreferenced (Base);
      begin
         for Use_Key_Value in Boolean loop
            Public_Get.Create_Get_Function
              (Db            => Db,
               Table         => Table,
               Table_Package => Table_Package,
               Scan          => True,
               First         => True,
               Key           => Key,
               Key_Value     => Use_Key_Value);
         end loop;
      end Create_Key_Get;

      --------------------------
      -- Create_Reference_Get --
      --------------------------

      procedure Create_Reference_Get (Base : Kit.Tables.Table_Type'Class) is
         use Aquarius.Drys.Expressions;
      begin

         if Base.Ada_Name = Table.Ada_Name then
            declare
               Abstract_Get : constant Subprogram_Declaration :=
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
            Get              : Subprogram_Declaration :=
                                 New_Function
                                   ("Reference",
                                    Base.Ada_Name & "_Reference",
                                    New_Function_Call_Expression
                                      (Base.Ada_Name & "_Reference",
                                       Index_Expression));
         begin

            Get.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");

            Get.Set_Overriding;
            Table_Package.Append_To_Body (Get);
         end;

      end Create_Reference_Get;

   begin

      Table_Package.With_Package ("Ada.Finalization",
                                  Body_With => True);

      Table_Package.With_Package ("System.Storage_Elements",
                                  Body_With => True);

      Table_Package.With_Package ("Marlowe.Btree_Handles",
                                Body_With => True);

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

      Create_Key_Marks (Db, Table, Table_Package);
      Create_Key_Context_Type (Db, Table, Table_Package);

      Create_Implementation_Type;

      Table_Package.Append
        (New_Separator);

      Create_Overrides (Db, Table, Table_Package);

      Table.Iterate (Create_Reference_Get'Access,
                     Inclusive   => True,
                     Table_First => False);

      Create_Actual_Type_Fetch;

      Table.Iterate_All (Add_Fetch'Access);
      Table.Iterate_All (Add_Store'Access);

      Create_Compound_Key_To_Storage_Functions
        (Db, Table, Table_Package);

      Add_Create_Function;

      Create_Generic_Get (Table, Table_Package);
      Create_Generic_Set (Table, Table_Package);

      Public_Get.Create_Get_Function
        (Db            => Db,
         Table         => Table,
         Table_Package => Table_Package,
         Scan          => False,
         First         => True,
         Key           => Kit.Tables.Null_Key_Cursor,
         Key_Value     => False);

      Public_Get.Create_Get_Function
        (Db            => Db,
         Table         => Table,
         Table_Package => Table_Package,
         Scan          => True,
         First         => True,
         Key           => Kit.Tables.Null_Key_Cursor,
         Key_Value     => False);

      Table.Scan_Keys (Create_Key_Get'Access);

      return Table_Package;
   end Generate_Public_Interface;

end Kit.Generate.Public_Interface;
