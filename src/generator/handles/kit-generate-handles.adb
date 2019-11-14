with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Types;

with Kit.String_Maps;

package body Kit.Generate.Handles is

   procedure Create_Handle_Type
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   ------------------------
   -- Create_Handle_Type --
   ------------------------

   procedure Create_Handle_Type
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      Interface_Name : constant String :=
        Table.Ada_Name & "_Interface";
      Class_Name     : constant String :=
        Table.Ada_Name & "_Class";
      Handle_Name    : constant String :=
        Table.Ada_Name & "_Handle";

      Interface_Definition : Syn.Interface_Type_Definition;
      Handle_Definition    : Syn.Types.Record_Type_Definition;

      Interface_Argument : constant Syn.Declarations.Formal_Argument'Class :=
        Syn.Declarations.New_Formal_Argument
          (Name          => "Handle",
           Argument_Type => Syn.Named_Subtype (Interface_Name));

      Handle_Argument : constant Syn.Declarations.Formal_Argument'Class :=
        Syn.Declarations.New_Formal_Argument
          (Name          => "Handle",
           Argument_Type => Syn.Named_Subtype (Handle_Name));

      Withed_Tables    : Kit.String_Maps.String_Map;
      Withed_Db_Tables : Kit.String_Maps.String_Map;

      procedure Add_Base
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Add_Base_Db
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Abstract_Property
        (Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Abstract_Reference_Functions;

      procedure Create_Property
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Get_Functions;

      procedure Create_Reference_Functions;
      procedure Create_Update_Functions;

      procedure Create_Base_Conversion
        (Base : Kit.Schema.Tables.Table_Type);

      function Return_Type_Name
        (Field : Kit.Schema.Fields.Field_Type)
         return String;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base
        (Base : Kit.Schema.Tables.Table_Type)
      is
      begin
         if not Withed_Tables.Contains (Base.Ada_Name) then
            Target.With_Package
              (Db.Handle_Package_Name & "." & Base.Package_Name);
            Withed_Tables.Insert (Base.Ada_Name);
         end if;

         Interface_Definition.Add_Parent
           (Name => Base.Package_Name & "." & Base.Ada_Name & "_Interface");
      end Add_Base;

      -----------------
      -- Add_Base_Db --
      -----------------

      procedure Add_Base_Db
        (Base : Kit.Schema.Tables.Table_Type)
      is
      begin
         if not Withed_Db_Tables.Contains (Base.Ada_Name)
           and then Base.Has_Writable_Field
         then
            Target.With_Package
              (Db.Database_Package_Name
               & "." & Base.Package_Name);
            Withed_Db_Tables.Insert (Base.Ada_Name);
         end if;
      end Add_Base_Db;

      -------------------------
      -- Add_Field_Type_With --
      -------------------------

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
      begin
         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Table_Name : constant String :=
                 Field.Get_Field_Type.Referenced_Table_Name;
            begin
               if not Withed_Tables.Contains (Table_Name)
                 and then Table_Name /= Table.Ada_Name
               then
                  Target.With_Package
                    (Db.Handle_Package_Name & "." & Table_Name);
                  Withed_Tables.Insert (Table_Name);
               end if;
            end;
         elsif Field.Get_Field_Type.Is_External_Type then
            declare
               Type_Package : constant String :=
                 Field.Get_Field_Type
                   .External_Type_Package_Name;
            begin
               if not Withed_Tables.Contains (Type_Package) then
                  Target.With_Package
                    (Type_Package);
                  Withed_Tables.Insert (Type_Package);
               end if;
            end;
         end if;
      end Add_Field_Type_With;

      ------------------------------
      -- Create_Abstract_Property --
      ------------------------------

      procedure Create_Abstract_Property
        (Field : Kit.Schema.Fields.Field_Type)
      is
      begin
         if not Field.Base_Reference then
            Target.Append
              (Syn.Declarations.New_Abstract_Function
                 (Name        => Field.Ada_Name,
                  Argument    => Interface_Argument,
                  Result_Type =>
                    Syn.Named_Subtype (Return_Type_Name (Field))));
         end if;
      end Create_Abstract_Property;

      -----------------------------------------
      -- Create_Abstract_Reference_Functions --
      -----------------------------------------

      procedure Create_Abstract_Reference_Functions is
      begin
         Target.Append
           (Syn.Declarations.New_Abstract_Function
              (Name        => "Reference_" & Table.Ada_Name,
               Argument    => Interface_Argument,
               Result_Type =>
                 Syn.Named_Subtype
                   (Db.Database_Package_Name
                    & "." & Table.Ada_Name & "_Reference")));

         if Table.Has_Writable_Field then
            Target.Append
              (Syn.Declarations.New_Abstract_Function
                 (Name        => "Update_" & Table.Ada_Name,
                  Argument    => Interface_Argument,
                  Result_Type =>
                    Syn.Named_Subtype
                      (Db.Database_Package_Name
                       & "." & Table.Ada_Name
                       & "." & Table.Ada_Name & "_Update_Handle")));
         end if;
      end Create_Abstract_Reference_Functions;

      ----------------------------
      -- Create_Base_Conversion --
      ----------------------------

      procedure Create_Base_Conversion
        (Base : Kit.Schema.Tables.Table_Type)
      is
         Block       : Syn.Blocks.Block_Type;
         Base_Handle : constant String :=
           Db.Handle_Package_Name
           & "." & Base.Ada_Name
           & "." & Base.Ada_Name & "_Handle";
      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Rec",
               Db.Database_Package_Name & "."
               & Table.Ada_Name & "."
               & Table.Ada_Name & "_Type",
               Syn.Expressions.New_Function_Call_Expression
                 (Db.Database_Package_Name
                  & "."
                  & Table.Ada_Name
                  & "."
                  & "Get",
                  "Handle.Reference")));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   (Db.Handle_Package_Name
                    & "." & Base.Ada_Name
                    & ".Get",
                    Syn.Object ("Rec.Get_" & Base.Ada_Name & "_Reference"))));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Base.Ada_Name & "_Handle",
                 Argument    => Handle_Argument,
                 Result_Type => Base_Handle,
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;
      end Create_Base_Conversion;

      --------------------------
      -- Create_Get_Functions --
      --------------------------

      procedure Create_Get_Functions is
         Block : Syn.Blocks.Block_Type;
         Ret   : Syn.Statements.Sequence_Of_Statements;
      begin
         Ret.Append
           (Syn.Statements.New_Assignment_Statement
              (Target => "Handle.Reference",
               Value  => Syn.Object ("Reference")));
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Return_Variable   => "Handle",
               Variable_Type     => Table.Ada_Name & "_Handle",
               Return_Statements => Ret));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Get",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Reference",
                      Syn.Named_Subtype
                        (Db.Database_Package_Name
                         & "." & Table.Ada_Name & "_Reference")),
                 Result_Type => Table.Ada_Name & "_Handle",
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;

      end Create_Get_Functions;

      ---------------------
      -- Create_Property --
      ---------------------

      procedure Create_Property
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Block : Syn.Blocks.Block_Type;
         Function_Name : constant String :=
           (if Field.Base_Reference
            then "Reference_" & Field.Ada_Name
            else Field.Ada_Name);

         Return_Type : constant Kit.Schema.Types.Kit_Type :=
           Field.Get_Field_Type;
         Return_Name : constant String :=
           (if Field.Base_Reference
            then Db.Database_Package_Name & "."
            & Field.Ada_Name & "_Reference"
            else Return_Type_Name (Field));
      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Rec",
               Db.Database_Package_Name & "."
               & Table.Ada_Name & "."
               & Table.Ada_Name & "_Type",
               Syn.Expressions.New_Function_Call_Expression
                 (Db.Database_Package_Name
                  & "."
                  & Table.Ada_Name
                  & "."
                  & "Get",
                  "Handle.Reference")));

         if Field.Base_Reference then
            Block.Append
              (Syn.Statements.New_Return_Statement
                 (Syn.Object
                      ("Rec.Get_" & Field.Ada_Name & "_Reference")));
         else
            if Return_Type.Is_Table_Reference then
               Block.Append
                 (Syn.Statements.New_Return_Statement
                    (Syn.Expressions.New_Function_Call_Expression
                         (Db.Handle_Package_Name
                          & "." & Return_Type.Ada_Name
                          & ".Get",
                          Syn.Object ("Rec." & Field.Ada_Name))));
            else
               Block.Append
                 (Syn.Statements.New_Return_Statement
                    (Syn.Object ("Rec." & Field.Ada_Name)));
            end if;
         end if;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Function_Name,
                 Argument    => Handle_Argument,
                 Result_Type => Return_Name,
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;
      end Create_Property;

      --------------------------------
      -- Create_Reference_Functions --
      --------------------------------

      procedure Create_Reference_Functions is
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Object ("Handle.Reference")));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Reference",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name & "_Reference",
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Reference_" & Table.Ada_Name,
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name & "_Reference",
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;

      end Create_Reference_Functions;

      -----------------------------
      -- Create_Update_Functions --
      -----------------------------

      procedure Create_Update_Functions is
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   (Db.Database_Package_Name
                    & "." & Table.Ada_Name
                    & ".Update_" & Table.Ada_Name,
                    Syn.Object ("Handle.Reference"))));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Update",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name
                 & "." & Table.Ada_Name & "_Update_Handle",
            Block       => Block);
         begin
            Target.Append (Fn);
         end;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Update_" & Table.Ada_Name,
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name
                 & "." & Table.Ada_Name
                 & "_Update_Handle",
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;

         declare
            procedure Create_Base_Update_Function
              (Base : Kit.Schema.Tables.Table_Type);

            procedure Create_Base_Update_Function
              (Base : Kit.Schema.Tables.Table_Type)
            is
               Block : Syn.Blocks.Block_Type;
            begin

               if Base.Has_Writable_Field then
                  Block.Append
                    (Syn.Statements.New_Return_Statement
                       (Syn.Expressions.New_Function_Call_Expression
                            (Db.Database_Package_Name
                             & "." & Base.Ada_Name
                             & ".Update_" & Base.Ada_Name,
                             Syn.Object
                               ("Handle.Reference_"
                                & Base.Ada_Name))));

                  declare
                     use Syn.Declarations;
                     Fn : Subprogram_Declaration'Class :=
                       Syn.Declarations.New_Function
                         (Name        => "Update_" & Base.Ada_Name,
                          Argument    =>
                            Syn.Declarations.New_Formal_Argument
                              ("Handle",
                               Syn.Named_Subtype
                                 (Table.Ada_Name & "_Handle")),
                          Result_Type =>
                            Db.Database_Package_Name
                          & "." & Base.Ada_Name
                          & "." & Base.Ada_Name
                          & "_Update_Handle",
                          Block       => Block);
                  begin
                     Fn.Set_Overriding;
                     Target.Append (Fn);
                  end;
               end if;
            end Create_Base_Update_Function;

         begin
            Table.Iterate (Create_Base_Update_Function'Access,
                           Inclusive => False);
         end;
      end Create_Update_Functions;

      ----------------------
      -- Return_Type_Name --
      ----------------------

      function Return_Type_Name
        (Field : Kit.Schema.Fields.Field_Type)
         return String
      is
         Return_Type : constant Kit.Schema.Types.Kit_Type :=
           Field.Get_Field_Type;
         Return_Name : constant String :=
           (if Return_Type.Is_Table_Reference
            then Db.Handle_Package_Name
            & "." & Return_Type.Ada_Name
            & "." & Return_Type.Ada_Name & "_Class"
            elsif Return_Type.Has_Custom_Type
            then Db.Database_Package_Name
            & "." & Return_Type.Ada_Name
            else Return_Type.Return_Handle_Subtype);
      begin
         return Return_Name;
      end Return_Type_Name;

   begin
      Table.Iterate
        (Process     => Add_Base'Access,
         Inclusive   => False);

      Table.Iterate_All (Add_Field_Type_With'Access,
                         Table_First => True);

      Table.Iterate
        (Process     => Add_Base_Db'Access,
         Inclusive   => False);

      Target.With_Package
        (Db.Database_Package_Name & "." & Table.Ada_Name,
         Body_With => not Table.Has_Writable_Field);

      Target.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Identifier => Interface_Name,
            Definition => Interface_Definition));

      Target.Append (Syn.Declarations.New_Separator);

      Target.Append
        (Syn.Declarations.New_Subtype_Declaration
           (Identifier => Class_Name,
            Definition => Syn.Named_Subtype (Interface_Name & "'Class")));

      Target.Append (Syn.Declarations.New_Separator);

      Create_Abstract_Reference_Functions;

      Table.Iterate (Create_Abstract_Property'Access);

      Handle_Definition.Set_Visible_Derivation;
      Handle_Definition.Add_Parent (Interface_Name);

      Handle_Definition.Add_Component
        (Component_Name => "Reference",
         Component_Type =>
           Db.Database_Package_Name & "."
         & Table.Ada_Name & "_Reference");

      declare
         Handle_Type : constant Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Private_Type_Declaration
             (Identifier => Handle_Name,
              Definition => Handle_Definition);
      begin
         Target.Append (Handle_Type);
      end;

      Create_Get_Functions;
      Create_Reference_Functions;

      if Table.Has_Writable_Field then
         Create_Update_Functions;
      end if;

      Table.Iterate (Process     => Create_Base_Conversion'Access,
                     Inclusive   => False);

      Table.Iterate_All (Create_Property'Access);

   end Create_Handle_Type;

   -----------------------------
   -- Generate_Handle_Package --
   -----------------------------

   function Generate_Handle_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      Handle_Package : Syn.Declarations.Package_Type'Class :=
        Top.New_Child_Package
          (Table.Ada_Name);

   begin

      Handle_Package.With_Package
        (Db.Database_Package_Name);

      Create_Handle_Type (Db, Table, Handle_Package);

      return Handle_Package;
   end Generate_Handle_Package;

end Kit.Generate.Handles;
