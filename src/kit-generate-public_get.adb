with Ada.Strings.Fixed;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Kit.Generate.Fetch;
with Kit.Names;

with Kit.Schema.Fields;
with Kit.Schema.Types;

package body Kit.Generate.Public_Get is

   procedure Create_Iterator_Start_Function
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Container     : in     Boolean;
      First         : in     Boolean);

   procedure Create_Iterator_Next_Function
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      With_Iterator : in     Boolean;
      Inline        : in     Boolean;
      Next          : in     Boolean);

   ----------------------------------
   -- Create_Default_Key_Functions --
   ----------------------------------

   procedure Create_Default_Key_Functions
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key           : in     Kit.Schema.Keys.Key_Type'Class)
   is
      use Aquarius.Drys.Declarations;
      Ask   : Aquarius.Drys.Expressions.Function_Call_Expression :=
                Aquarius.Drys.Expressions.New_Function_Call_Expression
                  ("Get_By_" & Key.Ada_Name);
      Block : Aquarius.Drys.Blocks.Block_Type;
   begin

      for I in 1 .. Key.Field_Count loop
         declare
            Field : Kit.Schema.Fields.Field_Type'Class renames
                      Key.Field (I);
         begin
            Ask.Add_Actual_Argument
              (Aquarius.Drys.Object (Field.Ada_Name));
         end;
      end loop;

      Block.Add_Declaration
        (New_Constant_Declaration
           ("Item", Table.Type_Name, Ask));
      Block.Add_Statement
        (Aquarius.Drys.Statements.New_Return_Statement
           (Aquarius.Drys.Object ("Item.Has_Element")));

      declare
         Fn : Subprogram_Declaration'Class :=
                New_Function
                  ("Is_" & Key.Ada_Name,
                   "Boolean",
                   Block);
      begin
         for I in 1 .. Key.Field_Count loop
            declare
               Field : Kit.Schema.Fields.Field_Type'Class renames
                         Key.Field (I);
            begin
               Fn.Add_Formal_Argument
                 (New_Formal_Argument
                    (Field.Ada_Name,
                     Aquarius.Drys.Named_Subtype
                       (Field.Get_Field_Type.Argument_Subtype)));
            end;
         end loop;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);

   end Create_Default_Key_Functions;

   ---------------------------------
   -- Create_Generic_Get_Function --
   ---------------------------------

   procedure Create_Generic_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key_Value     : in     Boolean)
   is
      pragma Unreferenced (Db);

      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      Key_Case  : Aquarius.Drys.Statements.Case_Statement_Record'Class :=
                    Aquarius.Drys.Statements.Case_Statement ("Key");
      Block                  : Aquarius.Drys.Blocks.Block_Type;

      Function_Name          : constant String := "Select_By";

      procedure Process_Key (Base  : Kit.Schema.Tables.Table_Type'Class;
                             Key   : Kit.Schema.Keys.Key_Type'Class);

      -----------------
      -- Process_Key --
      -----------------

      procedure Process_Key (Base  : Kit.Schema.Tables.Table_Type'Class;
                             Key   : Kit.Schema.Keys.Key_Type'Class)
      is
         pragma Unreferenced (Base);
         use Aquarius.Drys.Expressions;
         Function_Name : constant String :=
                           "Select_By_"
                           & Key.Ada_Name;
         Call : Function_Call_Expression :=
                           New_Function_Call_Expression
                             (Function_Name);
         Seq  : Aquarius.Drys.Statements.Sequence_Of_Statements;
      begin

         if Key_Value and then Key.Field_Count = 1 then
            Call.Add_Actual_Argument
              (Key.Field (1).Get_Field_Type.Convert_From_String ("Value"));
         end if;

         Seq.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              (Call));
         Key_Case.Add_Case_Option ("K_" & Table.Ada_Name & "_"
                                   & Key.Ada_Name,
                                   Seq);
      end Process_Key;

   begin

      Table.Scan_Keys (Process_Key'Access);

      Block.Append (Key_Case);

      declare
         Fn : Subprogram_Declaration'Class :=
                New_Function
                  (Function_Name, "Selection",
                   Block);
      begin
         Fn.Add_Formal_Argument
           ("Key", Table.Ada_Name & "_Key");

         if Key_Value then
            Fn.Add_Formal_Argument ("Value", "String");
         end if;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Generic_Get_Function;

   ---------------------------
   -- Create_Get_From_Index --
   ---------------------------

   procedure Create_Get_From_Index
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Block : Aquarius.Drys.Blocks.Block_Type;

      procedure Set_Field
        (Seq        : in out Aquarius.Drys.Blocks.Block_Type;
         Field_Name : String;
         Value      : Boolean);

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Seq        : in out Aquarius.Drys.Blocks.Block_Type;
         Field_Name : String;
         Value      : Boolean)
      is
      begin
         Seq.Append
           (New_Assignment_Statement
              ("Element." & Field_Name,
               (if Value then Object ("True") else Object ("False"))));
      end Set_Field;

   begin

      Block.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

      Block.Append
        (New_Assignment_Statement
           ("Element.Index",
            Object ("Index")));

      Fetch.Fetch_From_Index (Table       => Table,
                              Object_Name => "Element",
                              Target      => Block);

      Set_Field (Block, "Finished", False);
      Set_Field (Block, "Using_Key_Value", False);
      Set_Field (Block, "Scanning", False);
      Set_Field (Block, "Link.S_Locked", True);

      Block.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      declare
         use Aquarius.Drys.Declarations;
         Proc : Subprogram_Declaration'Class :=
                  New_Procedure
                    ("Get", Block);
      begin
         Proc.Add_Formal_Argument
           (New_Formal_Argument
              ("Index",
               Named_Subtype ("Marlowe.Database_Index")));
         Proc.Add_Formal_Argument
           (New_Out_Argument
              ("Element",
               Named_Subtype
                 (Table.Ada_Name & "_Implementation'Class")));
         Table_Package.Append_To_Body (Proc);
      end;

      Table_Package.Append_To_Body (Aquarius.Drys.Declarations.New_Separator);
   end Create_Get_From_Index;

   ---------------------
   -- Create_Iterator --
   ---------------------

   procedure Create_Iterator
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      use Aquarius.Drys.Declarations, Aquarius.Drys.Types;
      Iterator_Definition : Record_Type_Definition;
   begin

      Table_Package.Append_To_Body
        (New_Full_Type_Declaration
           ("Selection_Access",
            Aquarius.Drys.New_Access_Type
              ("Selection",
               Access_All => False)));

      Iterator_Definition.Add_Parent
        ("Selection_Iterator_Interfaces.Reversible_Iterator");
      Iterator_Definition.Add_Component
        ("Container", "Selection_Access");
      Table_Package.Append_To_Body
        (New_Full_Type_Declaration
           ("Iterator", Iterator_Definition));

      for Container in Boolean loop
         for First in reverse Boolean loop
            Create_Iterator_Start_Function
              (Table, Table_Package,
               Container => Container,
               First     => First);
         end loop;
      end loop;

      for With_Iterator in Boolean loop
         for Next in reverse Boolean loop
            Create_Iterator_Next_Function (Table, Table_Package,
                                           With_Iterator => With_Iterator,
                                           Inline        => False,
                                           Next          => Next);
         end loop;
      end loop;

      for Next in reverse Boolean loop
         Create_Iterator_Next_Function (Table, Table_Package,
                                        With_Iterator => False,
                                        Inline        => True,
                                        Next          => Next);
      end loop;

   end Create_Iterator;

   -----------------------------------
   -- Create_Iterator_Next_Function --
   -----------------------------------

   procedure Create_Iterator_Next_Function
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      With_Iterator : in     Boolean;
      Inline        : in     Boolean;
      Next          : in     Boolean)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;
      Next_Block        : Aquarius.Drys.Blocks.Block_Type;
   begin
      Next_Block.Add_Declaration
        (New_Object_Declaration
           ("Item", "Implementation_Access"));
      Next_Block.Add_Declaration
        (New_Object_Declaration
           ("Mark", "Mark_Access"));
      Next_Block.Add_Declaration
        (New_Object_Declaration
           ("Got_Valid_Index", "Boolean"));

      Next_Block.Add_Statement
        (If_Statement
           (Operator
              ("not",
               New_Function_Call_Expression
                 ("Has_Element",
                  Object ("Position"))),
            (if not Inline
             then New_Return_Statement
               (Object ("Position"))
             else New_Return_Statement)));

      if With_Iterator then
         Next_Block.Add_Statement
           ("Object.Container.State.Mutex.Shared_Lock");
      end if;

      Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                & ".Shared_Lock");

      Next_Block.Add_Statement
        (New_Assignment_Statement
           ("Item",
            New_Function_Call_Expression
              ("Implementation_Access",
               New_Function_Call_Expression
                 ("List_Of_Elements.Element",
                  Object ("Position.Current_Element")))));
      Next_Block.Add_Statement
        (New_Assignment_Statement
           ("Mark",
            New_Function_Call_Expression
              ("List_Of_Marks.Element",
               Object ("Position.Current_Mark"))));

      Next_Block.Add_Statement ("Item.Local.Unlock");

      Next_Block.Add_Statement
        (New_Procedure_Call_Statement
           ("Marlowe.Btree_Handles.Next",
            Object ("Mark.all")));
      Next_Block.Add_Statement
        (New_Assignment_Statement
           ("Got_Valid_Index",
            New_Function_Call_Expression
              ("Marlowe.Btree_Handles.Valid", "Mark.all")));
      Next_Block.Add_Statement
        (If_Statement
           (Object ("Got_Valid_Index"),
            New_Assignment_Statement
              ("Item.Index",
               New_Function_Call_Expression
                 ("Marlowe.Key_Storage.To_Database_Index",
                  New_Function_Call_Expression
                    ("Marlowe.Btree_Handles.Get_Key",
                     "Mark.all")))));

      Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                & ".Shared_Unlock");

      declare
         Fetch_Found : Sequence_Of_Statements;
         Not_Found   : Sequence_Of_Statements;
      begin
         Fetch.Fetch_From_Index (Table, "Item", Fetch_Found);
         Fetch_Found.Append
           ("Item.Link.S_Locked := True");
         Not_Found.Append ("Item.Index := 0");
         Next_Block.Add_Statement
           (If_Statement
              (Object ("Got_Valid_Index"),
               Fetch_Found,
               Not_Found));
      end;

      if With_Iterator then
         Next_Block.Add_Statement
           ("Object.Container.State.Mutex.Shared_Unlock");
      end if;

      if not Inline then
         Next_Block.Add_Statement ("return Position");
      end if;

      declare
         Next_Declaration : Subprogram_Declaration'Class :=
                              (if Inline
                               then New_Procedure
                                 ((if Next then "Next" else "Previous"),
                                  Next_Block)
                               else New_Function
                                 ((if Next then "Next" else "Previous"),
                                  "Cursor",
                                  Next_Block));
      begin
         if With_Iterator then
            Next_Declaration.Add_Formal_Argument
              ("Object", "Iterator");
         end if;
         if Inline then
            Next_Declaration.Add_Formal_Argument
              ("Position", Inout_Argument, "Cursor");
         else
            Next_Declaration.Add_Formal_Argument
              ("Position", "Cursor");
         end if;
         if With_Iterator then
            Next_Declaration.Set_Overriding;
            Table_Package.Append_To_Body (Next_Declaration);
         else
            Table_Package.Append (Next_Declaration);
         end if;
      end;
   end Create_Iterator_Next_Function;

   ------------------------------------
   -- Create_Iterator_Start_Function --
   ------------------------------------

   procedure Create_Iterator_Start_Function
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Container     : in     Boolean;
      First         : in     Boolean)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Return_Sequence  : Sequence_Of_Statements;
      Valid_Block      : Aquarius.Drys.Blocks.Block_Type;
      Invalid_Sequence : Sequence_Of_Statements;

      function Function_Name return String;

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);
      pragma Unreferenced (Set_Field);

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is
      begin
         if First then
            return "First";
         else
            return "Last";
         end if;
      end Function_Name;

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

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

      if Container then
         Return_Sequence.Append
           (New_Procedure_Call_Statement
              ("Container.State.Mutex.Lock"));
      else
         Return_Sequence.Append
           (New_Procedure_Call_Statement
              ("Object.Container.State.Mutex.Lock"));
      end if;

      Valid_Block.Add_Declaration
        (New_Constant_Declaration
           (Name => "Element",
            Object_Type => "Implementation_Access",
            Value       =>
              New_Allocation_Expression
                (Table.Ada_Name & "_Implementation")));

      Valid_Block.Add_Declaration
        (New_Constant_Declaration
           (Name => "Mark",
            Object_Type => "Mark_Access",
            Value       =>
              New_Allocation_Expression
                ("Marlowe.Btree_Handles.Btree_Mark'(M)")));

      if Container then
         Valid_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Container.State.Elements.Append",
               New_Function_Call_Expression
                 ("Element_Access", "Element")));

         Valid_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Container.State.Marks.Append",
               Object ("Mark")));
      else
         Valid_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Object.Container.State.Elements.Append",
               New_Function_Call_Expression
                 ("Element_Access", "Element")));

         Valid_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Object.Container.State.Marks.Append",
               Object ("Mark")));
      end if;

      Valid_Block.Add_Statement
        (New_Procedure_Call_Statement
           ("Get",
            New_Function_Call_Expression
              ("Marlowe.Key_Storage.To_Database_Index",
               New_Function_Call_Expression
                 ("Marlowe.Btree_Handles.Get_Key", "M")),
            Object ("Element.all")));
      if Container then
         Valid_Block.Add_Statement
           (New_Assignment_Statement
              ("Result.Current_Element",
               Object ("Container.State.Elements.Last")));
         Valid_Block.Add_Statement
           (New_Assignment_Statement
              ("Result.Current_Mark",
               Object ("Container.State.Marks.Last")));
      else
         Valid_Block.Add_Statement
           (New_Assignment_Statement
              ("Result.Current_Element",
               Object ("Object.Container.State.Elements.Last")));
         Valid_Block.Add_Statement
           (New_Assignment_Statement
              ("Result.Current_Mark",
               Object ("Object.Container.State.Marks.Last")));
      end if;

      Invalid_Sequence.Append
        (New_Assignment_Statement
           ("Result.Current_Element",
            Object ("List_Of_Elements.No_Element")));
      Invalid_Sequence.Append
        (New_Assignment_Statement
           ("Result.Current_Mark",
            Object ("List_Of_Marks.No_Element")));

      declare
         Mark_Block : Aquarius.Drys.Blocks.Block_Type;
         Initialiser      : Function_Call_Expression :=
                              New_Function_Call_Expression
                                ("Marlowe.Btree_Handles.Search");
      begin
         Initialiser.Add_Actual_Argument
           (Object ("Marlowe_Keys.Handle"));
         if Container then
            Initialiser.Add_Actual_Argument
              (Object ("Container.Key_Ref"));
            Initialiser.Add_Actual_Argument
              (Object ("Container.First_Key"));
            Initialiser.Add_Actual_Argument
              (Object ("Container.Last_Key"));
         else
            Initialiser.Add_Actual_Argument
              (Object ("Object.Container.Key_Ref"));
            Initialiser.Add_Actual_Argument
              (Object ("Object.Container.First_Key"));
            Initialiser.Add_Actual_Argument
              (Object ("Object.Container.Last_Key"));
         end if;

         Initialiser.Add_Actual_Argument
           (Object ("Marlowe.Closed"));
         Initialiser.Add_Actual_Argument
           (Object ("Marlowe.Closed"));
         if First then
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Forward"));
         else
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Backward"));
         end if;
         Mark_Block.Add_Declaration
           (New_Constant_Declaration
              ("M", "Marlowe.Btree_Handles.Btree_Mark",
               Initialiser));
         declare
            Valid_Sequence : Sequence_Of_Statements;
         begin
            Valid_Sequence.Append
              (Declare_Statement (Valid_Block));

            Mark_Block.Append
              (If_Statement
                 (New_Function_Call_Expression
                    ("Marlowe.Btree_Handles.Valid", "M"),
                  Valid_Sequence,
                  Invalid_Sequence));
         end;

         Return_Sequence.Append
           (Declare_Statement
              (Mark_Block));
      end;

      if Container then
         Return_Sequence.Append
           (New_Procedure_Call_Statement
              ("Container.State.Mutex.Unlock"));
      else
         Return_Sequence.Append
           (New_Procedure_Call_Statement
              ("Object.Container.State.Mutex.Unlock"));
      end if;

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      declare
         Block                  : Aquarius.Drys.Blocks.Block_Type;
      begin
         Block.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              ("Result", "Cursor", Return_Sequence));

         declare
            Fn : Subprogram_Declaration'Class :=
                   New_Function
                     (Function_Name, "Cursor",
                      Block);
         begin
            if not Container then
               Fn.Set_Overriding;
            end if;

            if Container then
               Fn.Add_Formal_Argument
                 ("Container", "Selection");
            else
               Fn.Add_Formal_Argument
                 ("Object", "Iterator");
            end if;

            if Container then
               Table_Package.Append (Fn);
            else
               Table_Package.Append_To_Body (Fn);
               Table_Package.Append_To_Body
                 (Aquarius.Drys.Declarations.New_Separator);
            end if;
         end;

      end;

   end Create_Iterator_Start_Function;

   -----------------------------------
   -- Create_Reference_Get_Function --
   -----------------------------------

   procedure Create_Reference_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Return_Sequence  : Sequence_Of_Statements;

      function Function_Name return String;

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is
      begin
         return "Get";
      end Function_Name;

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

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

--        Return_Sequence.Append
--          (New_Assignment_Statement
--             ("Result.Mark",
--              Object ("null")));

      Return_Sequence.Append
        (New_Assignment_Statement
           ("Result.Index",
            New_Function_Call_Expression
              ("Marlowe.Database_Index", "Ref")));

      Fetch.Fetch_From_Index (Table       => Table,
                              Object_Name => "Result",
                              Target      => Return_Sequence);

      Set_Field (Return_Sequence, "Finished", False);
      Set_Field (Return_Sequence, "Using_Key_Value", False);
      Set_Field (Return_Sequence, "Scanning", False);
      Set_Field (Return_Sequence, "Link.S_Locked", True);

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      declare
         use Aquarius.Drys.Declarations;
         Block                  : Aquarius.Drys.Blocks.Block_Type;
      begin
         Block.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              ("Result", Table.Implementation_Name, Return_Sequence));

         declare
            Fn : Subprogram_Declaration'Class :=
                   New_Function
                     (Function_Name, Table.Type_Name,
                      Block);
         begin
            Fn.Add_Formal_Argument
              (New_Formal_Argument
                 ("Ref",
                  Named_Subtype
                    (Table.Ada_Name & "_Reference")));
            Table_Package.Append (Fn);
         end;

      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Reference_Get_Function;

   -------------------------------
   -- Create_Selection_Function --
   -------------------------------

   procedure Create_Selection_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key_Name      : in     String;
      Key_Value     : in     Boolean;
      Bounds        : in     Boolean)
   is

      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Return_Sequence  : Sequence_Of_Statements;
      Key              : constant Kit.Schema.Keys.Key_Type'Class :=
                           Table.Key (Key_Name);
      function Function_Name return String;

      function To_Storage (First : Boolean) return Expression'Class;

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is
      begin
         return "Select_By_"
           & Kit.Names.Ada_Name (Key_Name);
      end Function_Name;

      ----------------
      -- To_Storage --
      ----------------

      function To_Storage (First : Boolean) return Expression'Class is
      begin
         if not Bounds then
            return Table.To_Storage
              (Table, Key_Table, "", Key,
               With_Index => False);
         elsif First then
            return Table.To_Storage
              (Table, Key_Table,
               "Start_", Key,
               With_Index => False);
         else
            return Table.To_Storage
              (Table, Key_Table,
               "Finish_", Key,
               With_Index => False);
         end if;
      end To_Storage;

   begin

      if not Key_Value then
         Return_Sequence.Append
           (New_Assignment_Statement
              ("Result.First_Key",
               Object ("(others => 0)")));
         Return_Sequence.Append
           (New_Assignment_Statement
              ("Result.Last_Key",
               Object
                 ("(others => "
                  & "System.Storage_Elements.Storage_Element'Last)")));
      else
         declare
            Start_Storage    : constant Expression'Class :=
                                 New_Function_Call_Expression
                                   ("Marlowe.Key_Storage.To_Storage_Array",
                                    "Marlowe.Database_Index'First");
            Last_Storage     : constant Expression'Class :=
                                 New_Function_Call_Expression
                                   ("Marlowe.Key_Storage.To_Storage_Array",
                                    "Marlowe.Database_Index'Last");
         begin
            Return_Sequence.Append
              (New_Assignment_Statement
                 ("Result.First_Key",
                  Operator
                    (Name  => "&",
                     Left  => To_Storage (True),
                     Right => Start_Storage)));
            Return_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Last_Key",
                  Operator
                    (Name  => "&",
                     Left  => To_Storage (False),
                     Right => Last_Storage)));
         end;
      end if;

      Return_Sequence.Append
        (New_Assignment_Statement
           ("Result.Key_Ref",
            Object ("Marlowe_Keys."
              & Table.Key_Reference_Name (Key_Name))));

      Return_Sequence.Append
        (New_Assignment_Statement
           ("Result.State",
            New_Allocation_Expression
              (Allocated_Type => "Selection_State")));

      declare
         use Aquarius.Drys.Declarations;
         Block        : Aquarius.Drys.Blocks.Block_Type;
         Return_Type  : constant String :=
                          "Selection ("
                          & Ada.Strings.Fixed.Trim (Natural'Image (Key.Size),
                                                    Ada.Strings.Left)
                          & ")";
      begin
         Block.Add_Declaration
           (Use_Type ("System.Storage_Elements.Storage_Array"));
         Block.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              ("Result", Return_Type, Return_Sequence));

         declare
            Fn : Subprogram_Declaration'Class :=
                   New_Function
                     (Function_Name, "Selection",
                      Block);
         begin
            if Key_Value then
               declare
                  Key : constant Kit.Schema.Keys.Key_Type'Class :=
                          Table.Key (Key_Name);
               begin
                  if Bounds then
                     for Is_Finish in Boolean loop
                        for I in 1 .. Key.Field_Count loop
                           declare
                              Tag        : constant String :=
                                             (if Is_Finish
                                              then "Finish_"
                                              else "Start_");
                              Field      : Kit.Schema.Fields.Field_Type'Class
                              renames Key.Field (I);
                              Field_Type : Kit.Schema.Types.Kit_Type'Class
                              renames Field.Get_Field_Type;
                           begin
                              if I = 1
                                or else Field_Type.Is_Table_Reference
                              then
                                 Fn.Add_Formal_Argument
                                   (New_Formal_Argument
                                      (Tag & Field.Ada_Name,
                                       Named_Subtype
                                         (Field_Type.Argument_Subtype)));
                              else
                                 Fn.Add_Formal_Argument
                                   (New_Formal_Argument
                                      (Tag & Field.Ada_Name,
                                       Named_Subtype
                                         (Field_Type.Argument_Subtype),
                                       (if Is_Finish
                                        then Field_Type.Last_Value
                                        else Field_Type.First_Value)));
                              end if;
                           end;
                        end loop;
                     end loop;
                  else
                     for I in 1 .. Key.Field_Count loop
                        declare
                           Field : Kit.Schema.Fields.Field_Type'Class
                           renames Key.Field (I);
                        begin
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                (Field.Ada_Name,
                                 Named_Subtype
                                   (Field.Get_Field_Type.Argument_Subtype)));
                        end;
                     end loop;
                  end if;
               end;
            end if;
            Table_Package.Append (Fn);
         end;

      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Selection_Function;

   --------------------------------
   -- Create_Unique_Get_Function --
   --------------------------------

   procedure Create_Unique_Get_Function
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key_Name      : in     String)
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Key              : constant Kit.Schema.Keys.Key_Type'Class :=
                           Table.Key (Key_Name);

      procedure Create_Function
        (Reference : Boolean;
         Unique    : Boolean;
         First     : Boolean);

      procedure Create_Function
        (Reference : Boolean;
         Unique    : Boolean;
         First     : Boolean)
      is

         Block            : Aquarius.Drys.Blocks.Block_Type;

         function Function_Name return String;

         procedure Set_Field
           (Seq        : in out Sequence_Of_Statements;
            Field_Name : String;
            Value      : Boolean);

         -------------------
         -- Function_Name --
         -------------------

         function Function_Name return String is
            Reference_Part : constant String :=
                               (if Reference
                                then "Reference_"
                                else "");
            Order_Part     : constant String :=
                               (if Unique
                                then ""
                                elsif First
                                then "First_"
                                else "Last_");
         begin
            return "Get_"
              & Order_Part
              & Reference_Part
              & "By_"
              & Key.Ada_Name;
         end Function_Name;

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

         Block.Add_Declaration
           (Use_Type ("System.Storage_Elements.Storage_Array"));
         Block.Add_Declaration
           (Use_Type ("Marlowe.Database_Index"));
         Block.Add_Declaration
           (Aquarius.Drys.Declarations.New_Object_Declaration
              ("Index", "Marlowe.Database_Index", Literal (0)));

         Block.Add_Statement
           (New_Procedure_Call_Statement
              (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

         declare
            Mark_Block       : Aquarius.Drys.Blocks.Block_Type;
            Initialiser      : Function_Call_Expression :=
                                 New_Function_Call_Expression
                                   ("Marlowe.Btree_Handles.Search");
         begin
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe_Keys.Handle"));
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe_Keys." & Table.Key_Reference_Name (Key)));
            declare
               Key_To_Storage   : constant Expression'Class :=
                                    Table.To_Storage
                                      (Table, Key_Table, "", Key,
                                       With_Index => False);
               Start_Storage    : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'First");
               Last_Storage     : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'Last");
            begin
               Initialiser.Add_Actual_Argument
                 (Operator
                    (Name  => "&",
                     Left  => Key_To_Storage,
                     Right => Start_Storage));
               Initialiser.Add_Actual_Argument
                 (Operator
                    (Name  => "&",
                     Left  => Key_To_Storage,
                     Right => Last_Storage));
            end;

            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));
            if First then
               Initialiser.Add_Actual_Argument
                 (Object ("Marlowe.Forward"));
            else
               Initialiser.Add_Actual_Argument
                 (Object ("Marlowe.Backward"));
            end if;

            Mark_Block.Add_Declaration
              (New_Constant_Declaration
                 ("M", "Marlowe.Btree_Handles.Btree_Mark",
                  Initialiser));
            Mark_Block.Add_Statement
              (If_Statement
                 (New_Function_Call_Expression
                    ("Marlowe.Btree_Handles.Valid",
                     Object ("M")),
                  New_Assignment_Statement
                    ("Index",
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Database_Index",
                        New_Function_Call_Expression
                          ("Marlowe.Btree_Handles.Get_Key",
                           "M")))));
            Block.Add_Statement
              (Declare_Statement (Mark_Block));
         end;

         if Reference then
            Block.Add_Statement
              (New_Return_Statement
                 (New_Function_Call_Expression
                    (Table.Ada_Name & "_Reference",
                     Object ("Index"))));
         else

            declare
               Return_Sequence  : Sequence_Of_Statements;
               Valid_Sequence   : Sequence_Of_Statements;
               Invalid_Sequence : Sequence_Of_Statements;
            begin

               Return_Sequence.Append
                 (New_Assignment_Statement
                    (Target => "Result.Index",
                     Value  => Object ("Index")));

               Fetch.Fetch_From_Index (Table       => Table,
                                       Object_Name => "Result",
                                       Target      => Valid_Sequence);

               Set_Field (Valid_Sequence, "Finished", False);
               Set_Field (Valid_Sequence, "Using_Key_Value", False);
               Set_Field (Valid_Sequence, "Scanning", False);
               Set_Field (Valid_Sequence, "Link.S_Locked", True);

               Set_Field (Invalid_Sequence, "Finished", True);
               Set_Field (Invalid_Sequence, "Using_Key_Value", False);
               Set_Field (Invalid_Sequence, "Scanning", False);
               Set_Field (Invalid_Sequence, "Link.S_Locked", False);

               Return_Sequence.Append
                 (If_Statement
                    (Operator ("/=", Object ("Index"), Literal (0)),
                     Valid_Sequence,
                     Invalid_Sequence));
               Return_Sequence.Append
                 (New_Procedure_Call_Statement
                    (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

               Block.Add_Statement
                 (New_Return_Statement
                    ("Result", Table.Implementation_Name,
                     Return_Sequence));
            end;
         end if;

         declare
            Result_Type : constant String :=
                            (if Reference
                             then Table.Ada_Name & "_Reference"
                             else Table.Type_Name);
            Fn : Subprogram_Declaration'Class :=
                   New_Function
                     (Function_Name, Result_Type,
                      Block);
         begin
            for I in 1 .. Key.Field_Count loop
               declare
                  Field : Kit.Schema.Fields.Field_Type'Class
                  renames Key.Field (I);
               begin
                  Fn.Add_Formal_Argument
                    (New_Formal_Argument
                       (Field.Ada_Name,
                        Named_Subtype
                          (Field.Get_Field_Type.Argument_Subtype)));
               end;
            end loop;
            Table_Package.Append (Fn);
         end;
      end Create_Function;

   begin
      for Reference in Boolean loop
         if Key.Unique then
            Create_Function (Reference, Unique => True, First => True);
         else
            Create_Function (Reference, Unique => True, First => True);
            Create_Function (Reference, Unique => False, First => True);
            Create_Function (Reference, Unique => False, First => False);
         end if;
      end loop;
      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Unique_Get_Function;

end Kit.Generate.Public_Get;
