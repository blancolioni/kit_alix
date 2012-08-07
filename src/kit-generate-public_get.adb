with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

with Kit.Generate.Fetch;
with Kit.Names;

with Kit.Schema.Fields;
with Kit.Schema.Types;

package body Kit.Generate.Public_Get is

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
                  ("First_By_" & Key.Ada_Name);
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
      First         : in     Boolean;
      Key_Value     : in     Boolean)
   is
      pragma Unreferenced (Db);

      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      Key_Case  : Aquarius.Drys.Statements.Case_Statement_Record'Class :=
                    Aquarius.Drys.Statements.Case_Statement ("Key");
      Block                  : Aquarius.Drys.Blocks.Block_Type;

      Function_Name          : constant String :=
                                 (if First
                                  then "First_By"
                                  else "Last_By");

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
         Call : Function_Call_Expression :=
                  New_Function_Call_Expression
                    ("First_By_" &
                     Key.Ada_Name);
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
                  (Function_Name, Table.Type_Name,
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

      Return_Sequence.Append
        (New_Assignment_Statement
           ("Result.Mark",
            Object ("null")));

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

   --------------------------
   -- Create_Scan_Function --
   --------------------------

   procedure Create_Scan_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      First         : in     Boolean;
      Key_Name      : in     String;
      Key_Value     : in     Boolean;
      Bounds        : in     Boolean)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys;
      use Aquarius.Drys.Expressions, Aquarius.Drys.Statements;

      Return_Sequence  : Sequence_Of_Statements;
      Lock_Sequence    : Sequence_Of_Statements;
      Invalid_Sequence : Sequence_Of_Statements;

      function Function_Name return String;

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is

         Base_Name : constant String :=
                       (if First
                        then "First"
                        else "Last");
      begin
         return Base_Name & "_By_" & Kit.Names.Ada_Name (Key_Name);
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

      Return_Sequence.Append
        (Table.Ada_Name & "_Impl."
         & Kit.Names.Ada_Name (Key_Name)
         & "_Key_Mutex.Shared_Lock");

      Lock_Sequence.Append
        (New_Assignment_Statement
           ("Result.Key_Ref",
            Object
              ("Marlowe_Keys."
               & Table.Key_Reference_Name (Key_Name))));

      Lock_Sequence.Append
        (New_Assignment_Statement
           ("Result.Mark",
            Object
              ("new Marlowe.Btree_Handles.Btree_Mark'(M)")));
      Lock_Sequence.Append
        (New_Assignment_Statement
           ("Result.Index",
            New_Function_Call_Expression
              ("Marlowe.Key_Storage.To_Database_Index",
               New_Function_Call_Expression
                 ("Marlowe.Btree_Handles.Get_Key",
                  "M"))));

      Fetch.Fetch_From_Index (Table       => Table,
                              Object_Name => "Result",
                              Target      => Lock_Sequence);

      Set_Field (Lock_Sequence, "Finished", False);
      Set_Field (Lock_Sequence, "Forward", First);
      Set_Field (Lock_Sequence, "Using_Key_Value", Key_Value);
      Set_Field (Lock_Sequence, "Scanning", True);
      Set_Field (Lock_Sequence, "Link.S_Locked", True);

      Set_Field (Invalid_Sequence, "Finished", True);
      Set_Field (Invalid_Sequence, "Forward", False);
      Set_Field (Invalid_Sequence, "Scanning", False);
      Invalid_Sequence.Append ("Result.Index := 0");
      Set_Field (Invalid_Sequence, "Link.S_Locked", False);

      declare
         use Aquarius.Drys.Declarations;
         Mark_Block : Aquarius.Drys.Blocks.Block_Type;
      begin
         if Key_Value then
            declare
               Key              : constant Kit.Schema.Keys.Key_Type'Class :=
                                    Table.Key (Key_Name);
               Key_To_Storage   : constant Expression'Class :=
                                    Table.To_Storage
                                      (Table, Key_Table, "", Key,
                                       With_Index => False);
               First_To_Storage : constant Expression'Class :=
                                    Table.To_Storage
                                      (Table, Key_Table,
                                       "Start_", Key,
                                       With_Index => False);
               Last_To_Storage  : constant Expression'Class :=
                                    Table.To_Storage
                                      (Table, Key_Table,
                                       "Finish_", Key,
                                       With_Index => False);
               Start_Storage    : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'First");
               Last_Storage     : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'Last");
               Initialiser      : Function_Call_Expression :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Btree_Handles.Search");
            begin
               Initialiser.Add_Actual_Argument
                 (Object ("Marlowe_Keys.Handle"));
               Initialiser.Add_Actual_Argument
                 (Object
                    ("Marlowe_Keys."
                     & Table.Key_Reference_Name (Key)));

               if Bounds then
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", First_To_Storage, Start_Storage));
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", Last_To_Storage, Last_Storage));
               else
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", Key_To_Storage, Start_Storage));
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", Key_To_Storage, Last_Storage));
               end if;

               Initialiser.Add_Actual_Argument
                 (Object ("Marlowe.Closed"));
               Initialiser.Add_Actual_Argument
                 (Object ("Marlowe.Closed"));
               Initialiser.Add_Actual_Argument
                 (Object
                    ((if First
                     then "Marlowe.Forward"
                     else "Marlowe.Backward")));
               Mark_Block.Add_Declaration
                 (Use_Type ("System.Storage_Elements.Storage_Array"));
               Mark_Block.Add_Declaration
                 (New_Constant_Declaration
                    ("M", "Marlowe.Btree_Handles.Btree_Mark",
                     Initialiser));
            end;

         else
            Mark_Block.Add_Declaration
              (New_Constant_Declaration
                 ("M", "Marlowe.Btree_Handles.Btree_Mark",
                  New_Function_Call_Expression
                    ("Marlowe.Btree_Handles.Search",
                     "Marlowe_Keys.Handle",
                     "Marlowe_Keys."
                     & Table.Key_Reference_Name (Key_Name),
                     (if First
                      then "Marlowe.Forward"
                      else "Marlowe.Backward"))));
         end if;

         Mark_Block.Append
           (If_Statement
              (New_Function_Call_Expression
                 ("Marlowe.Btree_Handles.Valid", "M"),
               Lock_Sequence,
               Invalid_Sequence));
         Return_Sequence.Append
           (Declare_Statement
              (Mark_Block));
      end;

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      Return_Sequence.Append
        (Table.Ada_Name & "_Impl." &
           Kit.Names.Ada_Name (Key_Name)
         & "_Key_Mutex.Shared_Unlock");

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
                                       Field_Type.First_Value));
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
   end Create_Scan_Function;

end Kit.Generate.Public_Get;
