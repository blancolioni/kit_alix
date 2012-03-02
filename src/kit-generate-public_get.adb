with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

with Kit.Generate.Fetch;

with Kit.Fields;

package body Kit.Generate.Public_Get is

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

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);

      ------------------
      -- Declare_Index --
      ------------------

      procedure Declare_Index
        (Block : in out Aquarius.Drys.Blocks.Block_Type)
      is
         use Aquarius.Drys.Declarations;
      begin
         if not Scan then
            if not Using_Key then
               null;
--                 Block.Add_Declaration
--                   (New_Constant_Declaration
--                      ("Index",
--                       "Marlowe.Database_Index",
--                       Object ("Marlowe.Database_Index (Reference)")));
            else
               null;
            end if;
         else
            if not Using_Key then
               pragma Assert (First);
               Block.Add_Declaration (Use_Type ("Marlowe.Database_Index"));
               Block.Add_Declaration
                 (New_Object_Declaration
                    ("Index",
                     "Marlowe.Database_Index"));

               Block.Add_Statement ("Index := 1");
               declare
                  Valid_Index : constant Expression'Class :=
                                  New_Function_Call_Expression
                                    ("Marlowe.Btree_Handles.Valid_Index",
                                     "Marlowe_Keys.Handle",
                                     Table.Ada_Name & "_Table_Index",
                                     "Index");
                  Is_Deleted  : constant Expression'Class :=
                                  New_Function_Call_Expression
                                    ("Marlowe.Btree_Handles.Deleted_Record",
                                     "Marlowe_Keys.Handle",
                                     Table.Ada_Name & "_Table_Index",
                                     "Index");
                  Condition   : constant Expression'Class :=
                                  Operator ("and then",
                                            Valid_Index, Is_Deleted);
               begin
                  Block.Add_Statement
                    (While_Statement
                       (Condition,
                        New_Assignment_Statement
                          ("Index",
                          Operator ("+", Object ("Index"), Literal (1)))));
               end;

            else

               null;

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
            return Base_Name & "_By_" & Kit.Tables.Ada_Name (Key);
         else
            return Base_Name;
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
      Get.Add_Actual_Argument (Object ("Handle"));
      Get.Add_Actual_Argument
        (Object ("Index"));

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

      if Using_Key then
         Return_Sequence.Append
           (Table.Ada_Name & "_Impl." & Kit.Tables.Ada_Name (Key)
            & "_Key_Mutex.Shared_Lock");
      end if;

      if not Scan then
         if not Using_Key then
            Return_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Index",
                  Object ("Marlowe.Database_Index (Reference)")));
         else
            null;
         end if;
      else
         if not Using_Key then
            Return_Sequence.Append ("Result.Index := Index");
         else
            Lock_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Key_Value",
                  Object
                    ("(K_" & Table.Ada_Name & "_"
                     & Tables.Ada_Name (Key)
                     & ", M)")));
            Lock_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Index",
                  New_Function_Call_Expression
                    ("Marlowe.Key_Storage.To_Database_Index",
                     New_Function_Call_Expression
                       ("Marlowe.Btree_Handles.Get_Key",
                        "M"))));
         end if;
      end if;

      Fetch.Fetch_From_Index (Table       => Table,
                              Object_Name => "Result",
                              Target      => Lock_Sequence);

      Set_Field (Lock_Sequence, "Finished", not Scan);
      Set_Field (Lock_Sequence, "Forward", First);
      Set_Field (Lock_Sequence, "Using_Key", Using_Key);
      Set_Field (Lock_Sequence, "Using_Key_Value", Key_Value);
      Set_Field (Lock_Sequence, "Scanning", Scan);
      Set_Field (Lock_Sequence, "Link.S_Locked", True);

      Set_Field (Invalid_Sequence, "Finished", True);
      Set_Field (Invalid_Sequence, "Forward", False);
      Set_Field (Invalid_Sequence, "Scanning", False);
      Invalid_Sequence.Append ("Result.Index := 0");
      Set_Field (Invalid_Sequence, "Link.S_Locked", False);

      if not Using_Key then
         Return_Sequence.Append
           (If_Statement
              (New_Function_Call_Expression
                 ("Marlowe.Btree_Handles.Valid_Index",
                  "Marlowe_Keys.Handle",
                  Table.Ada_Name & "_Table_Index",
                  "Result.Index"),
               Lock_Sequence,
               Invalid_Sequence));
      else
         declare
            use Aquarius.Drys.Declarations;
            Mark_Block : Aquarius.Drys.Blocks.Block_Type;
         begin
            if Key_Value then
               declare
                  Key_To_Storage : constant Expression'Class :=
                                     Table.Key_To_Storage (Key, "");
--                                       Kit.Types.To_Storage_Array
--                                         (Kit.Tables.Key_Type (Key),
--                                          Kit.Tables.Ada_Name (Key));
                  Start_Storage  : constant Expression'Class :=
                                     New_Function_Call_Expression
                                       ("Marlowe.Key_Storage.To_Storage_Array",
                                        "Marlowe.Database_Index'First");
                  Last_Storage   : constant Expression'Class :=
                                     New_Function_Call_Expression
                                       ("Marlowe.Key_Storage.To_Storage_Array",
                                        "Marlowe.Database_Index'Last");
                  Initialiser    : Function_Call_Expression :=
                                     New_Function_Call_Expression
                                       ("Marlowe.Btree_Handles.Search");
               begin
                  Initialiser.Add_Actual_Argument
                    (Object ("Marlowe_Keys.Handle"));
                  Initialiser.Add_Actual_Argument
                    (Object
                       ("Marlowe_Keys." & Table.Ada_Name
                        & "_" & Tables.Ada_Name (Key) & "_Ref"));
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", Key_To_Storage, Start_Storage));
                  Initialiser.Add_Actual_Argument
                    (Operator ("&", Key_To_Storage, Last_Storage));
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
                        "Marlowe_Keys." & Table.Ada_Name
                        & "_" & Tables.Ada_Name (Key) & "_Ref",
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
      end if;

      Return_Sequence.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      if Using_Key then
         Return_Sequence.Append
           (Table.Ada_Name & "_Impl." & Kit.Tables.Ada_Name (Key)
            & "_Key_Mutex.Shared_Unlock");
      end if;

      declare
         use Aquarius.Drys.Declarations;
         Block                  : Aquarius.Drys.Blocks.Block_Type;
         Fn                     : Subprogram_Declaration;
      begin
         Declare_Index (Block);
         Block.Append
           (Aquarius.Drys.Statements.New_Return_Statement
              ("Result", Table.Implementation_Name, Return_Sequence));

         Fn := New_Function
           (Function_Name, Table.Type_Name,
            Block);

         if not Scan and then not Using_Key then
            Fn.Add_Formal_Argument
              (New_Formal_Argument
                 ("Reference",
                  Named_Subtype (Table.Ada_Name & "_Reference")));
         end if;

         if Using_Key and then Key_Value then
            if Kit.Tables.Is_Compound_Key (Key) then
               for I in 1 .. Kit.Tables.Compound_Field_Count (Key) loop
                  declare
                     Field : Kit.Fields.Field_Type'Class
                     renames Kit.Tables.Compound_Field (Key, I);
                  begin
                     Fn.Add_Formal_Argument
                       (New_Formal_Argument
                          (Field.Ada_Name,
                           Named_Subtype
                             (Field.Get_Field_Type.Argument_Subtype)));
                  end;
               end loop;
            else
               Fn.Add_Formal_Argument
                 (New_Formal_Argument
                    (Kit.Tables.Ada_Name (Key),
                     Named_Subtype
                       (Kit.Tables.Key_Type
                          (Key).Argument_Subtype)));
            end if;
         end if;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Aquarius.Drys.Declarations.New_Separator);
   end Create_Get_Function;

end Kit.Generate.Public_Get;
