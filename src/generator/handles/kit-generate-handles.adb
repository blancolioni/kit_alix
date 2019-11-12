with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Types;

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

      procedure Add_Base
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Create_Abstract_Property
        (Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Property
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Get_Functions;

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
         Interface_Definition.Add_Parent
           (Name => Base.Package_Name & "." & Base.Ada_Name & "_Interface");
      end Add_Base;

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
         Return_Type : constant Kit.Schema.Types.Kit_Type :=
           Field.Get_Field_Type;
      begin

         if not Field.Base_Reference then
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

            declare
               Fn : Syn.Declarations.Subprogram_Declaration'Class :=
                 Syn.Declarations.New_Function
                   (Name        => Field.Ada_Name,
                    Argument    => Handle_Argument,
                    Result_Type => Return_Type_Name (Field),
                    Block       => Block);
            begin
               Fn.Set_Overriding;
               Target.Append (Fn);
            end;
         end if;
      end Create_Property;

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
            else Return_Type.Return_Handle_Subtype);
      begin
         return Return_Name;
      end Return_Type_Name;

   begin
      Table.Iterate
        (Process     => Add_Base'Access,
         Inclusive   => False);

      Target.With_Package
        (Db.Database_Package_Name & "." & Table.Ada_Name,
         Body_With => True);

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

      procedure With_Base (Base : Kit.Schema.Tables.Table_Type);

      ---------------
      -- With_Base --
      ---------------

      procedure With_Base (Base : Kit.Schema.Tables.Table_Type) is
      begin
         Handle_Package.With_Package
           (Db.Handle_Package_Name & "." & Base.Package_Name);
      end With_Base;

   begin

      Handle_Package.With_Package
        (Db.Database_Package_Name);

      Table.Iterate (With_Base'Access, False);

      Create_Handle_Type (Db, Table, Handle_Package);

      return Handle_Package;
   end Generate_Handle_Package;

end Kit.Generate.Handles;
