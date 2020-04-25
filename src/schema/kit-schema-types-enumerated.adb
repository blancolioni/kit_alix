with Syn.Blocks;
with Syn.Declarations;
with Syn.Expressions;
with Syn.Statements;

package body Kit.Schema.Types.Enumerated is

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (To      : in out Enumerated_Type;
      Literal : String)
   is
   begin
      To.Literals.Append (Literal);
      if To.Literals.Last_Index = 2 then
         To.Size := 1;
      elsif To.Literals.Last_Index mod 256 = 0 then
         To.Size := To.Size + 1;
      end if;
   end Add_Literal;

   -----------------------------
   -- Argument_Handle_Subtype --
   -----------------------------

   overriding function Argument_Handle_Subtype
     (Item : Enumerated_Type)
      return String
   is
      use Ada.Strings.Unbounded;
   begin
      if Item.Pkg_Name /= Null_Unbounded_String then
         return To_String (Item.Pkg_Name) & "."
           & Root_Kit_Type (Item).Argument_Handle_Subtype;
      else
         return Root_Kit_Type (Item).Argument_Handle_Subtype;
      end if;
   end Argument_Handle_Subtype;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding function Create_Database_Record
     (For_Type : Enumerated_Type)
      return Syn.Statement'Class
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;
      T : Enumerated_Type'Class renames
            Enumerated_Type'Class (For_Type);
      Create : constant Expression'Class :=
                 New_Function_Call_Expression
                   ("Kit_Enumeration.Create",
                    Literal (Size (T)),
                    Literal (T.Standard_Name));
      Block        : Syn.Blocks.Block_Type;
   begin
      Block.Add_Declaration
        (New_Constant_Declaration
           ("Enum", "Kit_Enumeration_Reference",
            Create));
      for I in 1 .. T.Literals.Last_Index loop
         declare
            Create_Literal : Procedure_Call_Statement'Class :=
                               New_Procedure_Call_Statement
                                 ("Kit_Literal.Create");
         begin
            Create_Literal.Add_Actual_Argument
              (Literal
                 (Kit.Names.Standard_Name (For_Type.Literals.Element (I))));
            Create_Literal.Add_Actual_Argument
              (Object ("Enum"));
            Create_Literal.Add_Actual_Argument (Literal (I - 1));

            Block.Append (Create_Literal);
         end;
      end loop;

      return Declare_Statement (Block);

   end Create_Database_Record;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : Enumerated_Type)
      return Syn.Expression'Class
   is
   begin
      return Syn.Object
        (Kit.Names.Ada_Name (Item.Literals.Element (1)));
   end Default_Value;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   overriding function Haskell_Type_Name
     (Item : Enumerated_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "Int";
   end Haskell_Type_Name;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding function Return_Subtype
     (Item : Enumerated_Type)
      return String
   is
   begin
      return Item.Ada_Name;
   end Return_Subtype;

   --------------------------
   -- Set_Defining_Package --
   --------------------------

   procedure Set_Defining_Package
     (Of_Type : in out Enumerated_Type'Class;
      Name    : String)
   is
   begin
      Of_Type.Pkg_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Defining_Package;

   ----------
   -- Size --
   ----------

   overriding function Size (Item : Record_Type_Enumeration) return Natural is
      pragma Unreferenced (Item);
   begin
      return 4;
   end Size;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   overriding
   function Storage_Array_Transfer
     (Item          : Enumerated_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
      Block : Syn.Blocks.Block_Type;
   begin
      if To_Storage then
         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("T", "Marlowe.Key_Storage.Unsigned_Integer",
               Syn.Object
                 (Item.Ada_Name & "'Pos (" & Object_Name & ")")));
      else
         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              ("T", "Marlowe.Key_Storage.Unsigned_Integer"));
      end if;

      declare
         Proc_Name : constant String :=
                       (if To_Storage then "To" else "From")
                       & "_Storage";
      begin
         Block.Add_Statement
           (Storage_Array_Transfer
              (Item, "T",
               Storage_Name, Start, Finish,
               Proc_Name));
      end;

      if not To_Storage then
         Block.Add_Statement
           (Syn.Statements.New_Assignment_Statement
              (Object_Name,
               Syn.Object (Item.Ada_Name & "'Val (T)")));
      end if;

      return Syn.Statements.Declare_Statement (Block);

   end Storage_Array_Transfer;

   --------------------
   -- To_Declaration --
   --------------------

   overriding
   function To_Declaration
     (Item : Enumerated_Type)
      return Syn.Declaration'Class
   is
      Definition : Syn.Enumeration_Type_Definition;
   begin
      for Literal of Item.Literals loop
         Definition.New_Literal (Literal);
      end loop;
      return Syn.Declarations.New_Full_Type_Declaration
        (Item.Ada_Name, Definition);
   end To_Declaration;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   overriding function To_Storage_Array
     (Item        : Enumerated_Type;
      Object      : Syn.Expression'Class)
      return Syn.Expression'Class
   is
      use Syn, Syn.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         New_Function_Call_Expression
           (Item.Ada_Name & "'Pos",
            Object),
         Literal (Size (Root_Kit_Type'Class (Item))));
   end To_Storage_Array;

end Kit.Schema.Types.Enumerated;
