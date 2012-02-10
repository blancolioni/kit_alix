with Ada.Strings.Fixed;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Kit.Fields;

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

   procedure Create_Get_Function
     (Db            : in     Kit.Databases.Database_Type;
      Table         : in     Kit.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Scan          : in     Boolean;
      First         : in     Boolean;
      Key           : in     Kit.Tables.Key_Cursor;
      Key_Value     : in     Boolean);

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

         procedure Insert_Key (Key_Table : Kit.Tables.Table_Type'Class;
                               Key       : Kit.Tables.Key_Cursor);

         ----------------
         -- Insert_Key --
         ----------------

         procedure Insert_Key (Key_Table : Kit.Tables.Table_Type'Class;
                               Key       : Kit.Tables.Key_Cursor)
         is
            Insert : Procedure_Call_Statement :=
                       New_Procedure_Call_Statement
                         ("Marlowe.Btree_Handles.Insert");
         begin
            Insert.Add_Actual_Argument ("Marlowe_Keys.Handle");
            Insert.Add_Actual_Argument ("Marlowe_Keys."
                                        & Table.Ada_Name
                                        & "_"
                                        & Kit.Tables.Ada_Name (Key)
                                        & "_Ref");
            Insert.Add_Actual_Argument
              (Kit.Tables.To_Storage (Table       => Table,
                                      Key_Table   => Key_Table,
                                      Object_Name => "Item",
                                      Key         => Key));
            Insert_Keys.Append (Insert);
         end Insert_Key;

      begin

         Finalize_Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));
         Table.Scan_Keys (Insert_Key'Access);
         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));

         declare
            X_Locked : constant Expression'Class :=
                         Object ("Item.Link_Context.X_Locked");
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
                         Object ("Item.Link_Context.S_Locked");
            X_Locked : constant Expression'Class :=
                         Object ("Item.Link_Context.X_Locked");
            S_And_X  : constant Expression'Class :=
                         Operator ("or else", S_Locked, X_Locked);
            Same_Context  : constant Expression'Class :=
                              Operator ("not", Object ("Item.Subclassed"));
            Do_Unlock     : constant Expression'Class :=
                              Long_Operator ("and then",
                                             S_And_X, Same_Context);
            Unlock        : constant Statement'Class :=
                              New_Procedure_Call_Statement
                                ("Item.Local_Context.Unlock");
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
              ("Item.Local_Context",
               Aquarius.Drys.Expressions.New_Allocation_Expression
                 ("Local_Lock_Context_Record")));
         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Memory_Mutex.Unlock"));

         Initialize_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Assignment_Statement
              ("Item.Link_Context",
               Aquarius.Drys.Object
                 ("Lock_Context (Item.Local_Context)")));

         Set_Field ("Finished", "True");
         Set_Field ("Forward", "True");
         Set_Field ("Created", "False");
         Set_Field ("Deleted", "False");
         Set_Field ("Scanning", "False");
         Set_Field ("Has_Finish", "False");
         Set_Field ("Subclassed", "False");
         Set_Field ("Using_Key", "False");
         Set_Field ("Index", "0");

         Set_Field ("Local_Context.X_Locked", "False");
         Set_Field ("Local_Context.S_Locked", "False");

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

   -------------------------
   -- Create_Get_Function --
   -------------------------

   procedure Create_Get_Function
     (Db            : in     Kit.Databases.Database_Type;
      Table         : in     Kit.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Scan          : in     Boolean;
      First         : in     Boolean;
      Key           : in     Kit.Tables.Key_Cursor;
      Key_Value     : in     Boolean)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Get              : Function_Call_Expression :=
                           New_Function_Call_Expression
                             ("Ship_Cache.Get");
      Return_Sequence  : Sequence_Of_Statements;
      Lock_Sequence    : Sequence_Of_Statements;
      Invalid_Sequence : Sequence_Of_Statements;

      Using_Key : constant Boolean :=
                    Kit.Tables."/="
                      (Key, Kit.Tables.Null_Key_Cursor);

      procedure Declare_Index
        (Block : in out Aquarius.Drys.Blocks.Block_Type);

      function Function_Name return String;

      procedure Get_Base (Base   : Kit.Tables.Table_Type'Class);

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);

      -------------------
      -- Declare_Index --
      -------------------

      procedure Declare_Index
        (Block : in out Aquarius.Drys.Blocks.Block_Type)
      is
         use Aquarius.Drys.Declarations;
      begin
         if not Scan then
            if not Using_Key then
               Block.Add_Declaration
                 (New_Constant_Declaration
                    ("Index",
                     "Marlowe.Database_Index",
                     Object ("Marlowe.Database_Index (Reference)")));
            else
               null;
            end if;
         else
            Block.Add_Declaration
              (New_Object_Declaration
                 ("Index",
                  "Marlowe.Database_Index"));
            if not Using_Key then
               Block.Add_Statement ("Index := 1");
            end if;
         end if;
      end Declare_Index;

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is

         Base_Name : constant String :=
                       (if not Scan
                        then "Get"
                        elsif not First
                        then "Last"
                        else "First");
      begin
         if Using_Key then
            return Base_Name & "_By" & Kit.Tables.Ada_Name (Key);
         else
            return Base_Name;
         end if;
      end Function_Name;

      --------------
      -- Get_Base --
      --------------

      procedure Get_Base (Base   : Kit.Tables.Table_Type'Class) is
         Base_Target    : constant String :=
                            "Result.Local_Context."
                              & Base.Ada_Name
           & "_Data";
         Cache_Package  : constant String :=
                            Base.Ada_Name & "_Cache";
         Index_Variable : constant String :=
                            (if Base.Ada_Name = Table.Ada_Name
                             then "Index"
                             else "Result.Local_Context." & Table.Ada_Name
                             & "_Data.Db." & Base.Ada_Name & "_Index");
      begin
         Lock_Sequence.Append
           (New_Assignment_Statement
              (Base_Target,
               New_Function_Call_Expression
                 (Cache_Package & ".Get",
                  "Marlowe_Keys.Handle",
                  Index_Variable)));
      end Get_Base;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean)
      is
      begin
         Seq.Append
           (New_Assignment_Statement
              ("Result." & Field_Name,
               (if Value then Object ("True") else Object ("False"))));
      end Set_Field;

   begin
      Get.Add_Actual_Argument (Object ("Handle"));
      Get.Add_Actual_Argument
        (Object ("Index"));

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Lock"));

      Table.Iterate (Get_Base'Access,
                     Inclusive   => True,
                     Table_First => True);

      Set_Field (Lock_Sequence, "Finished", not Scan);
      Set_Field (Lock_Sequence, "Forward", First);
      Set_Field (Lock_Sequence, "Scanning", Scan);
      Lock_Sequence.Append ("Result.Index := Index");
      Set_Field (Lock_Sequence, "Link_Context.S_Locked", True);

      Set_Field (Invalid_Sequence, "Finished", True);
      Set_Field (Invalid_Sequence, "Forward", False);
      Set_Field (Invalid_Sequence, "Scanning", False);
      Lock_Sequence.Append ("Result.Index := 0");
      Set_Field (Invalid_Sequence, "Link_Context.S_Locked", False);

      Return_Sequence.Append
        (If_Statement
           (New_Function_Call_Expression
              ("Marlowe.Btree_Handles.Valid_Index",
               "Marlowe_Keys.Handle",
               Table.Ada_Name & "_Table_Index",
               "Index"),
            Lock_Sequence,
            Invalid_Sequence));

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Unlock"));

      declare
         use Aquarius.Drys.Declarations;
         Table_Class_Wide_Name  : constant String :=
                                    Table.Ada_Name & "_Type";
         Implementation_Type    : constant String :=
                                    Table.Ada_Name & "_Implementation";
         Block                  : Aquarius.Drys.Blocks.Block_Type;
         Fn                     : Subprogram_Declaration;
      begin
         Declare_Index (Block);
         Block.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              ("Result", Implementation_Type, Return_Sequence));

         Fn := New_Function
           (Function_Name, Table_Class_Wide_Name,
            Block);

         if not Scan and then not Using_Key then
            Fn.Add_Formal_Argument
              (New_Formal_Argument
                 ("Reference",
                  Named_Subtype (Table.Ada_Name & "_Reference")));
         end if;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Get_Function;

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
              ("Item." & T.Ada_Name
               & "_Data.S_Lock"));
      end S_Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (T : Kit.Tables.Table_Type'Class) is
      begin
         Unlock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item." & T.Ada_Name
               & "_Data.Unlock"));
      end Unlock;

      ------------
      -- X_Lock --
      ------------

      procedure X_Lock (T : Kit.Tables.Table_Type'Class) is
      begin
         X_Lock_Block.Add_Statement
           (Aquarius.Drys.Statements.New_Procedure_Call_Statement
              ("Item." & T.Ada_Name
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
        (New_Procedure_Call_Statement ("Item.Link_Context.Unlock"));
      Locking_Sequence.Append
        (New_Procedure_Call_Statement ("Item.Link_Context.X_Lock"));

      X_Lock_Block.Add_Statement
        (If_Statement
           (Operator
              ("not",
               Object ("Item.Link_Context.X_Locked")),
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
      begin
         Next_Block.Add_Declaration
           (Use_Type ("Marlowe.Database_Index"));
         Next_Block.Add_Statement
           (If_Statement
              (Operator
                 ("not",
                  Object ("Item.Scanning")),
               Raise_Statement
                 ("Constraint_Error",
                  "Not currently scanning this table")));

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
      Table_Interface_Name   : constant String :=
                                 Table.Ada_Name & "_Interface";
      Table_Class_Wide_Name  : constant String :=
                                 Table.Ada_Name & "_Type";
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
                 ("Result.Local_Context." & Base.Ada_Name & "_Data",
                  New_Allocation_Expression
                    (Base.Ada_Name & "_Cache.Cache_Record")));
         end Allocate_Context;

         ---------------------
         -- Database_Insert --
         ---------------------

         procedure Database_Insert (Base : Kit.Tables.Table_Type'Class) is
            use Aquarius.Drys.Expressions;

            Index_Field : constant String :=
                            (if Base.Ada_Name = Table.Ada_Name
                             then "Result.Index"
                             else "Result.Local_Context." & Table.Ada_Name
                             & "_Data.Db." & Base.Ada_Name & "_Index");

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
                    ("Result.Local_Context." & Base.Ada_Name & "_Data"
                     & ".Db." & Meta_Base.Ada_Name & "_Index",
                     Object
                       ("Result.Local_Context." & Table.Ada_Name & "_Data"
                        & ".Db." & Meta_Base.Ada_Name & "_Index")));
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
                 ("Result.Local_Context." & Base.Ada_Name & "_Data.Initialise",
                  Literal (Integer (Base.Reference_Index)),
                  Object (Index_Field)));

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Kit.Cache.Insert",
                  New_Function_Call_Expression
                    ("Kit.Cache.Cache_Entry",
                     "Result.Local_Context." & Base.Ada_Name & "_Data")));

            Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl.Write",
                  Object (Index_Field),
                  Object
                    ("Result.Local_Context." & Base.Ada_Name & "_Data"
                     & ".Db")));

            Sequence.Append
              ("Result.Local_Context." & Base.Ada_Name & "_Data.X_Lock");

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
         Set_Field ("Link_Context.X_Locked", "True");
         Set_Field ("Link_Context.S_Locked", "False");
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

         Sequence.Append ("Result.Local_Context.X_Locked := True");

         declare
            Block : Aquarius.Drys.Blocks.Block_Type;
         begin
            Block.Append
              (New_Return_Statement
                 ("Result", Implementation_Type, Sequence));
            Table_Package.Append
              (New_Function ("Create", Table_Class_Wide_Name,
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
            Fetch       : Subprogram_Declaration;
            Block  : Aquarius.Drys.Blocks.Block_Type;
         begin

            Block.Add_Declaration
              (Aquarius.Drys.Declarations.Renaming_Declaration
                 ("Result",
                  Field.Get_Field_Type.Unconstrained_Record_Subtype,
                  Object ("Item.Local_Context."
                    & Base.Ada_Name & "_Data.Db."
                    & Field.Ada_Name)));
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

      procedure Add_Store
        (Base  : Kit.Tables.Table_Type'Class;
         Field : Kit.Fields.Field_Type'Class)
      is

         use Aquarius.Drys.Statements;

         Update_Sequence  : Sequence_Of_Statements;

         procedure Update_Key (Key_Base : Kit.Tables.Table_Type'Class;
                               Key      : Kit.Tables.Key_Cursor);

         ----------------
         -- Update_Key --
         ----------------

         procedure Update_Key (Key_Base : Kit.Tables.Table_Type'Class;
                               Key      : Kit.Tables.Key_Cursor)
         is
            Update_Block : Aquarius.Drys.Blocks.Block_Type;
            Record_Index : constant String :=
                             (if Key_Base.Ada_Name = Table.Ada_Name
                              then "Item.Index"
                              else "Item.Local_Context." & Table.Ada_Name
                                & "_Data.Db." & Key_Base.Ada_Name & "_Index");

         begin
            Update_Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl."
                  & Kit.Tables.Ada_Name (Key) & "_Key_Mutex.Lock"));

            Update_Block.Add_Declaration
              (Aquarius.Drys.Declarations.Renaming_Declaration
                 ("Index",
                  "Marlowe.Database_Index",
                  Object (Record_Index)));

            declare
               use Aquarius.Drys.Expressions;
               Using_Key : constant Expression'Class :=
                             Object ("Item.Using_Key");
               This_Key  : constant Expression'Class :=
                             Operator
                               ("=",
                                Object ("Item.Key_Value.K"),
                                Object ("K_" & Table.Ada_Name & "_"
                                  & Kit.Tables.Ada_Name (Key)));
               Release   : constant Statement'Class :=
                             New_Procedure_Call_Statement
                               ("Marlowe.Btree_Handles.Release",
                                Object
                                  ("Item.Key_Value."
                                   & Kit.Tables.Ada_Name (Key)
                                   & "_Context"));
               Delete_Old_Key : Procedure_Call_Statement :=
                                  New_Procedure_Call_Statement
                                    ("Marlowe.Btree_Handles.Delete");
               Insert_New_Key : Procedure_Call_Statement :=
                                  New_Procedure_Call_Statement
                                    ("Marlowe.Btree_Handles.Insert");
            begin

               Update_Block.Add_Statement
                 (If_Statement
                    (Long_Operator ("and then", Using_Key, This_Key),
                     Release));

               Delete_Old_Key.Add_Actual_Argument
                 ("Marlowe_Keys.Handle");
               Delete_Old_Key.Add_Actual_Argument
                 ("Marlowe_Keys."
                  & Key_Base.Ada_Name
                  & "_"
                  & Kit.Tables.Ada_Name (Key)
                  & "_Ref");
               Delete_Old_Key.Add_Actual_Argument
                 (Kit.Tables.To_Storage
                    (Key_Value_Name   => "Old_Key_Value",
                     Index_Value_Name => "Index",
                     Key              => Key));
               Update_Block.Add_Statement (Delete_Old_Key);

               Insert_New_Key.Add_Actual_Argument
                 ("Marlowe_Keys.Handle");
               Insert_New_Key.Add_Actual_Argument
                 ("Marlowe_Keys."
                  & Key_Base.Ada_Name
                  & "_"
                  & Kit.Tables.Ada_Name (Key)
                  & "_Ref");
               Insert_New_Key.Add_Actual_Argument
                 (Kit.Tables.To_Storage
                    (Key_Value_Name   => "Item." & Kit.Tables.Ada_Name (Key),
                     Index_Value_Name => "Index",
                     Key              => Key));
               Update_Block.Add_Statement (Insert_New_Key);

            end;

            Update_Sequence.Append
              (Declare_Statement (Update_Block));
            Update_Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl."
                  & Kit.Tables.Ada_Name (Key) & "_Key_Mutex.Unlock"));
         end Update_Key;

      begin
         if Base.Name = Table.Name then
            declare
               Store : Subprogram_Declaration :=
                         New_Abstract_Procedure
                           ("Set_" & Field.Ada_Name,
                            New_Inout_Argument ("Item",
                              Named_Subtype
                                (Table.Ada_Name & "_Interface")));
            begin
               Store.Add_Formal_Argument
                 (New_Formal_Argument ("Value",
                  Named_Subtype
                    (Field.Get_Field_Type.Argument_Subtype)));

               Table_Package.Append (Store);
               Table_Package.Append (New_Separator);
            end;
         end if;

         declare
            Store_Block      : Aquarius.Drys.Blocks.Block_Type;
            Store            : Subprogram_Declaration;
         begin

            Store_Block.Add_Declaration
              (Aquarius.Drys.Declarations.Use_Type
                 ("System.Storage_Elements.Storage_Array"));

            Store_Block.Add_Declaration
              (Aquarius.Drys.Declarations.Renaming_Declaration
                 ("Target",
                  Field.Get_Field_Type.Unconstrained_Record_Subtype,
                  Object ("Item.Local_Context."
                    & Base.Ada_Name & "_Data.Db."
                    & Field.Ada_Name)));

            Store_Block.Add_Declaration
              (Aquarius.Drys.Declarations.New_Constant_Declaration
                 ("Old_Key_Value",
                  Field.Get_Field_Type.Return_Subtype,
                  Object ("Item." & Field.Ada_Name)));

            Store_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Database_Mutex.Shared_Lock"));
            Store_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Item.X_Lock"));

            Field.Get_Field_Type.Set_Value
              (Target_Name => "Target",
               Value_Name  => "Value",
               Sequence    => Store_Block);

            Table.Scan_Keys (Field, Update_Key'Access);

            declare
               S : constant Statement'Class :=
                     If_Statement (Object ("not Item.Created"),
                                   Update_Sequence);
            begin
               Store_Block.Append (S);
            end;

            Store_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Database_Mutex.Shared_Unlock"));

            Store := New_Procedure
              ("Set_" & Field.Ada_Name,
               New_Inout_Argument ("Item",
                 Named_Subtype
                   (Table.Ada_Name & "_Implementation")),
              Store_Block);
            Store.Add_Formal_Argument
              (New_Formal_Argument ("Value",
               Named_Subtype
                 (Field.Get_Field_Type.Argument_Subtype)));

            Store.Set_Overriding;
            Table_Package.Append_To_Body (Store);
         end;

      end Add_Store;

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
            Name : constant String := It.Ada_Name;
         begin
            Context_Defn.Add_Component
              (Name & "_Data",
               Name & "_Cache.Cache_Access");
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
         Record_Defn.Add_Component ("Local_Context",
                                    "Local_Lock_Context");
         Record_Defn.Add_Component ("Link_Context",
                                    "Lock_Context");

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              (Implementation_Type, Record_Defn));

         Create_Control_Procedures (Db, Table, Table_Package);
         Create_Search_Procedures (Db, Table, Table_Package);

      end Create_Implementation_Type;

   begin

      Table_Package.With_Package ("Ada.Finalization",
                                  Body_With => True);

      Table_Package.With_Package ("System.Storage_Elements",
                                  Body_With => True);

      Table_Package.With_Package ("Marlowe.Btree_Handles",
                                Body_With => True);

      Table_Package.With_Package ("Marlowe.Key_Storage",
                                Body_With => True);

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
           (Table.Ada_Name & "_Interface",
            Table_Interface));
      Table_Package.Append
        (New_Separator);

      Table_Package.Append
        (New_Subtype_Declaration
           (Table_Class_Wide_Name,
            Aquarius.Drys.Class_Wide_Subtype
              (Table_Interface_Name)));

      Create_Key_Marks (Db, Table, Table_Package);
      Create_Key_Context_Type (Db, Table, Table_Package);

      Create_Implementation_Type;

      Table_Package.Append
        (New_Separator);

      Create_Overrides (Db, Table, Table_Package);

      Table.Iterate_All (Add_Fetch'Access);
      Table.Iterate_All (Add_Store'Access);

      Add_Create_Function;

      Create_Get_Function
        (Db            => Db,
         Table         => Table,
         Table_Package => Table_Package,
         Scan          => False,
         First         => True,
         Key           => Kit.Tables.Null_Key_Cursor,
         Key_Value     => False);

      Create_Get_Function
        (Db            => Db,
         Table         => Table,
         Table_Package => Table_Package,
         Scan          => True,
         First         => True,
         Key           => Kit.Tables.Null_Key_Cursor,
         Key_Value     => False);

      return Table_Package;
   end Generate_Public_Interface;

end Kit.Generate.Public_Interface;
