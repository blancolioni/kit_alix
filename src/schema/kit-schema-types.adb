with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;

with Syn.Blocks;
with Syn.Declarations;
with Syn.Expressions;
with Syn.Statements;

with Kit.Schema.Types.Enumerated;

package body Kit.Schema.Types is

   Text_Type_Record_Size : constant := 32;

   Standard_Boolean_Type      : Kit_Type;
   Standard_Float_Type        : Kit_Type;
   Standard_Long_Float_Type   : Kit_Type;
   Standard_Integer_Type      : Kit_Type;
   Standard_Long_Integer_Type : Kit_Type;
   Standard_Natural_Type      : Kit_Type;
   Standard_Positive_Type     : Kit_Type;
   Standard_Rec_Type          : Kit_Type;

   package Type_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Kit_Type,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   Type_Table : Type_Maps.Map;

   type Integer_Type is new Root_Kit_Type with
      record
         Low, High : Integer;
      end record;

   overriding
   function Return_Subtype (Item : Integer_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Integer_Type)
      return Syn.Statement'Class;

   overriding
   function Default_Value (Item : Integer_Type)
                           return Syn.Expression'Class;

   overriding
   function Haskell_Type_Name (Item : Integer_Type) return String;

   type Long_Integer_Type is new Root_Kit_Type with
      record
         Low, High : Integer_64;
      end record;

   overriding function Return_Subtype
     (Item : Long_Integer_Type)
      return String
   is ("Integer_64");

   overriding function Create_Database_Record
     (For_Type : Long_Integer_Type)
      return Syn.Statement'Class;

   overriding function Default_Value
     (Item : Long_Integer_Type)
      return Syn.Expression'Class
   is (Syn.Literal (0));

   overriding function Haskell_Type_Name
     (Item : Long_Integer_Type)
      return String
   is ("Integer");

   overriding function To_Storage_Array
     (Item        : Long_Integer_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   overriding function Storage_Array_Transfer
     (Item          : Long_Integer_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   type Float_Type is new Root_Kit_Type with
      record
         Long : Boolean;
      end record;

   overriding
   function Return_Subtype (Item : Float_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Float_Type)
      return Syn.Statement'Class;

   overriding
   function Default_Value (Item : Float_Type)
                           return Syn.Expression'Class;

   overriding
   function Haskell_Type_Name (Item : Float_Type) return String;

   overriding
   function To_Storage_Array
     (Item        : Float_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   type Boolean_Type is new Root_Kit_Type with null record;

   overriding
   function Return_Subtype (Item : Boolean_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Boolean_Type)
      return Syn.Statement'Class;

   overriding
   function Default_Value (Item : Boolean_Type)
                           return Syn.Expression'Class;

   overriding
   function Haskell_Type_Name (Item : Boolean_Type) return String;

   type Table_Reference_Type_Record is
     new Root_Kit_Type with null record;

   overriding
   function Return_Subtype
     (Item : Table_Reference_Type_Record)
      return String;

   overriding
   function To_Storage_Array
     (Item        : Table_Reference_Type_Record;
      Object_Name : String)
      return Syn.Expression'Class;

   overriding
   function Storage_Array_Transfer
     (Item          : Table_Reference_Type_Record;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   overriding
   function First_Value (Item : Table_Reference_Type_Record)
                         return Syn.Expression'Class;

   overriding
   function Last_Value (Item : Table_Reference_Type_Record)
                        return Syn.Expression'Class;

   overriding
   function Convert_To_String (Item   : Table_Reference_Type_Record;
                               Object_Name : String)
                               return Syn.Expression'Class;

   overriding
   function Convert_From_String (Item   : Table_Reference_Type_Record;
                               Object_Name : String)
                                 return Syn.Expression'Class;

   overriding
   function Create_Database_Record
     (For_Type : Table_Reference_Type_Record)
      return Syn.Statement'Class;

   overriding
   function Default_Value (Item : Table_Reference_Type_Record)
                           return Syn.Expression'Class;

   overriding
   function Is_Reference_To
     (Item       : Table_Reference_Type_Record;
      Table_Name : String)
      return Boolean;

   type String_Type is new Root_Kit_Type with
      record
         Length : Positive;
      end record;

--     overriding
--     function To_Storage_Array
--       (Item        : String_Type;
--        Object_Name : String)
--        return Syn.Expression'Class;

   overriding
   function Convert_To_String (Item   : String_Type;
                               Object_Name : String)
                               return Syn.Expression'Class;

   overriding
   function Convert_From_String (Item   : String_Type;
                               Object_Name : String)
                                 return Syn.Expression'Class;

   overriding
   function Return_Value
     (Value_Type  : String_Type;
      Target_Name : String)
      return Syn.Expression'Class;

   overriding
   procedure Set_Value
     (Value_Type  : String_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class);

   overriding
   function Return_Subtype (Item : String_Type) return String;

   overriding
   function Record_Subtype (Item : String_Type) return String;

   overriding
   function Unconstrained_Record_Subtype
     (Item : String_Type)
      return String;

   overriding
   function Is_String (Item : String_Type) return Boolean;

   overriding
   function Is_Table_Reference
     (Item : Table_Reference_Type_Record)
      return Boolean;

   overriding
   function First_Value (Item : String_Type)
                         return Syn.Expression'Class;

   overriding
   function Last_Value (Item : String_Type)
                        return Syn.Expression'Class;

   overriding
   function Create_Database_Record
     (For_Type : String_Type)
      return Syn.Statement'Class;

   overriding
   function Has_Default_Value (Item : String_Type)
                               return Boolean;

   overriding
   function Default_Value (Item : String_Type)
                           return Syn.Expression'Class;

   function Standard_String_Name (Length : Natural) return String;
   --  String types of the given length have this name in the
   --  internal database representation

   overriding
   function Storage_Array_Transfer
     (Item          : String_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   overriding
   function Haskell_Type_Name (Item : String_Type) return String;

   type Text_Type is new Root_Kit_Type with null record;

   overriding function Key_OK (Item : Text_Type) return Boolean
   is (False);

   overriding function Is_Text (Item : Text_Type) return Boolean is (True);

   overriding function Record_Subtype (Item : Text_Type) return String;

   overriding function Return_Subtype (Item : Text_Type) return String;

   overriding function Return_Value
     (Value_Type  : Text_Type;
      Target_Name : String)
      return Syn.Expression'Class;

   overriding function Has_Default_Value
     (Item : Text_Type)
      return Boolean
   is (False);

   overriding
   function Default_Value (Item : Text_Type)
                           return Syn.Expression'Class;

   overriding
   function Convert_To_String
     (Item   : Text_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   overriding
   function Convert_From_String (Item   : Text_Type;
                                 Object_Name : String)
                                 return Syn.Expression'Class;

   overriding function Create_Database_Record
     (For_Type : Text_Type)
      return Syn.Statement'Class;

   overriding procedure Set_Value
     (Value_Type  : Text_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class);

   overriding function Storage_Array_Transfer
     (Item          : Text_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   type External_Type is new Root_Kit_Type with
      record
         Local_Type       : Kit_Type;
         External_Package : Ada.Strings.Unbounded.Unbounded_String;
         External_Name    : Ada.Strings.Unbounded.Unbounded_String;
         To_Database      : Ada.Strings.Unbounded.Unbounded_String;
         From_Database    : Ada.Strings.Unbounded.Unbounded_String;
         To_String        : Ada.Strings.Unbounded.Unbounded_String;
         From_String      : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Key_OK (Item : External_Type) return Boolean
   is (False);

   overriding function Return_Subtype
     (Item : External_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Item.External_Package)
       & "."
       & Ada.Strings.Unbounded.To_String (Item.External_Name));

   overriding
   function Create_Database_Record
     (For_Type : External_Type)
      return Syn.Statement'Class
   is (For_Type.Local_Type.Create_Database_Record);

   overriding
   function Default_Value (Item : External_Type)
                           return Syn.Expression'Class
   is (Item.Local_Type.Default_Value);

   overriding function Record_Subtype
     (Item : External_Type) return String
   is (Item.Local_Type.Record_Subtype);

   overriding function Return_Value
     (Value_Type  : External_Type;
      Target_Name : String)
      return Syn.Expression'Class;

   overriding procedure Set_Value
     (Value_Type  : External_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class);

   overriding function To_Storage_Array
     (Item        : External_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is (Item.Local_Type.To_Storage_Array (Object_Name));

   overriding function Storage_Array_Transfer
     (Item          : External_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is (Item.Local_Type.Storage_Array_Transfer
       (To_Storage, Object_Name, Storage_Name, Start, Finish));

   overriding function Convert_To_String
     (Item        : External_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   overriding function Convert_From_String
     (Item        : External_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   overriding function Internal_Database_Name
     (Item : External_Type) return String
   is (Item.Local_Type.Internal_Database_Name);

   ----------------------
   -- Argument_Subtype --
   ----------------------

   function Argument_Subtype (Item : Root_Kit_Type) return String is
   begin
      return Root_Kit_Type'Class (Item).Return_Subtype;
   end Argument_Subtype;

   -------------------------
   -- Convert_From_String --
   -------------------------

   function Convert_From_String (Item   : Root_Kit_Type;
                                 Object_Name : String)
                                 return Syn.Expression'Class
   is
      use Syn.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "'Value",
         Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   overriding function Convert_From_String
     (Item   : Table_Reference_Type_Record;
      Object_Name : String)
      return Syn.Expression'Class
   is
      use Syn.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Return_Subtype & "'Value",
         Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   overriding function Convert_From_String (Item   : String_Type;
                                 Object_Name : String)
                                 return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object (Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   overriding function Convert_From_String
     (Item   : Text_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object (Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   overriding function Convert_From_String
     (Item        : External_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      use Syn.Expressions;
      use Ada.Strings.Unbounded;
   begin
      return New_Function_Call_Expression
        (To_String (Item.External_Package)
         & "."
         & To_String (Item.From_String),
         Object_Name);
   end Convert_From_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   function Convert_To_String (Item   : Root_Kit_Type;
                               Object_Name : String)
                               return Syn.Expression'Class
   is
      use Syn.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "'Image",
         Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   overriding function Convert_To_String
     (Item   : Table_Reference_Type_Record;
      Object_Name : String)
      return Syn.Expression'Class
   is
      use Syn.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Return_Subtype & "'Image",
         Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   overriding function Convert_To_String
     (Item   : String_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object (Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   overriding
   function Convert_To_String
     (Item   : Text_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object (Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   overriding function Convert_To_String
     (Item        : External_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      use Syn.Expressions;
      use Ada.Strings.Unbounded;
   begin
      return New_Function_Call_Expression
        (To_String (Item.External_Package)
         & "."
         & To_String (Item.To_String),
         Object_Name);
   end Convert_To_String;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Integer_Type)
      return Syn.Statement'Class
   is
      use Syn;
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_Integer.Create");
   begin
      Result.Add_Actual_Argument (Literal (For_Type.Size));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument (Literal (For_Type.Low));
      Result.Add_Actual_Argument (Literal (For_Type.High));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Long_Integer_Type)
      return Syn.Statement'Class
   is
      use Syn;
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_Long_Integer.Create");
   begin
      Result.Add_Actual_Argument (Literal (For_Type.Size));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument (Value (Image (For_Type.Low)));
      Result.Add_Actual_Argument (Value (Image (For_Type.High)));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Float_Type)
      return Syn.Statement'Class
   is
      use Syn;
      Record_Name : constant String :=
                      (if For_Type.Long then "Long_Float" else "Float");
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_" & Record_Name & ".Create");
   begin
      Result.Add_Actual_Argument (Literal (For_Type.Size));
      Result.Add_Actual_Argument
        (Literal (Ada.Characters.Handling.To_Lower (Record_Name)));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding function Create_Database_Record
     (For_Type : Boolean_Type)
      return Syn.Statement'Class
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;
      Create : constant Expression'Class :=
                 New_Function_Call_Expression
                   ("Kit_Enumeration.Create",
                    Literal (For_Type.Size),
                    Literal (For_Type.Standard_Name));
      Create_False : Procedure_Call_Statement'Class :=
                       New_Procedure_Call_Statement
                         ("Kit_Literal.Create");
      Create_True  : Procedure_Call_Statement'Class :=
                       New_Procedure_Call_Statement
                         ("Kit_Literal.Create");
      Block        : Syn.Blocks.Block_Type;
   begin
      Create_False.Add_Actual_Argument (Literal ("false"));
      Create_False.Add_Actual_Argument (Object ("Enum"));
      Create_False.Add_Actual_Argument (Literal (0));

      Create_True.Add_Actual_Argument (Literal ("true"));
      Create_True.Add_Actual_Argument (Object ("Enum"));
      Create_True.Add_Actual_Argument (Literal (1));

      Block.Add_Declaration
        (New_Constant_Declaration
           ("Enum", "Kit_Enumeration_Reference",
            Create));
      Block.Append (Create_False);
      Block.Append (Create_True);

      return Declare_Statement (Block);

   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Table_Reference_Type_Record)
      return Syn.Statement'Class
   is
      use Syn;
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_Reference.Create");
   begin
      Result.Add_Actual_Argument (Literal (8));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument (Object (For_Type.Ada_Name & "_Ref"));
--
--          (Object
--             ("Kit_Record.Get_By_Name ("""
--              & For_Type.Standard_Name
--              & """).Reference"));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding function Create_Database_Record
     (For_Type : String_Type)
      return Syn.Statement'Class
   is
      use Syn;
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_String.Create");
   begin
      Result.Add_Actual_Argument (Literal (For_Type.Size));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument (Literal (For_Type.Length));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Text_Type)
      return Syn.Statement'Class
   is
      use Syn;
      Result : Syn.Statements.Procedure_Call_Statement'Class :=
                 Syn.Statements.New_Procedure_Call_Statement
                   ("Kit_Type.Create");
   begin
      Result.Add_Actual_Argument (Literal (Text_Type_Record_Size));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      return Result;
   end Create_Database_Record;

   ---------------------------
   -- Create_Standard_Types --
   ---------------------------

   procedure Create_Standard_Types is
   begin
      declare
         Result : Enumerated.Enumerated_Type;
      begin
         Result.Create ("boolean");
         Root_Kit_Type (Result).User_Defined := False;
         Result.Add_Literal ("False");
         Result.Add_Literal ("True");
         Standard_Boolean_Type := new Enumerated.Enumerated_Type'(Result);
      end;

      declare
         Result : Float_Type;
      begin
         Result.Create ("float");
         Result.User_Defined := False;
         Result.Size := Float'Size / 8;
         Result.Long := False;
         Standard_Float_Type :=
           new Float_Type'(Result);
      end;

      declare
         Result : Integer_Type;
      begin
         Result.Create ("integer");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := Integer'First;
         Result.High := Integer'Last;
         Standard_Integer_Type := new Integer_Type'(Result);
      end;

      declare
         Result : Long_Integer_Type;
      begin
         Result.Create ("integer_64");
         Result.User_Defined := False;
         Result.Size := Integer_64'Size / 8;
         Result.Low := Integer_64'First;
         Result.High := Integer_64'Last;
         Standard_Long_Integer_Type := new Long_Integer_Type'(Result);
      end;

      declare
         Result : Float_Type;
      begin
         Result.Create ("long_float");
         Result.User_Defined := False;
         Result.Size := Long_Float'Size / 8;
         Result.Long := True;
         Standard_Long_Float_Type :=
           new Float_Type'(Result);
      end;

      declare
         Result : Integer_Type;
      begin
         Result.Create ("natural");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := 0;
         Result.High := Integer'Last;
         Standard_Natural_Type := new Integer_Type'(Result);
      end;

      declare
         Result : Integer_Type;
      begin
         Result.Create ("positive");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := 1;
         Result.High := Integer'Last;
         Standard_Positive_Type := new Integer_Type'(Result);
      end;

      declare
         Result : Enumerated.Record_Type_Enumeration;
      begin
         Result.Create ("record_type");
         Result.Add_Literal ("R_None");
         Root_Kit_Type'Class (Result).User_Defined := False;
         Standard_Rec_Type := new Enumerated.Record_Type_Enumeration'(Result);
      end;

      New_Type (Standard_Integer);
      New_Type (Standard_Long_Integer);
      New_Type (Standard_Positive);
      New_Type (Standard_Natural);
      New_Type (Standard_Float);
      New_Type (Standard_Long_Float);
      New_Type (Standard_Boolean);
      New_Type (Standard_Record_Type);
      New_Type (Standard_Text);

   end Create_Standard_Types;

   -------------------
   -- Default_Value --
   -------------------

   overriding
   function Default_Value (Item : Integer_Type)
                           return Syn.Expression'Class
   is
   begin
      return Syn.Literal (Integer'Max (0, Item.Low));
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : Float_Type)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("0.0");
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : Boolean_Type)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("False");
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : Table_Reference_Type_Record)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Literal (0);
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : String_Type)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("((others => Character'Val (0)), 0)");
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   overriding function Default_Value
     (Item : Text_Type)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("(0, 0, (others => Character'Val (0)))");
   end Default_Value;

   function External_Type_Package_Name
     (Item : Root_Kit_Type'Class)
      return String
   is (Ada.Strings.Unbounded.To_String
       (External_Type (Item).External_Package));

   -----------------
   -- First_Value --
   -----------------

   function First_Value (Of_Type : Root_Kit_Type)
                         return Syn.Expression'Class
   is
   begin
      return Syn.Object
        (Root_Kit_Type'Class (Of_Type).Ada_Name
         & "'First");
   end First_Value;

   -----------------
   -- First_Value --
   -----------------

   overriding
   function First_Value (Item : String_Type)
                         return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("(1 => Character'First)");
   end First_Value;

   -----------------
   -- First_Value --
   -----------------

   overriding
   function First_Value (Item : Table_Reference_Type_Record)
                         return Syn.Expression'Class
   is
   begin
      return Syn.Object
        (Root_Kit_Type'Class (Item).Ada_Name & "_Reference'First");
   end First_Value;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Name : String) return Kit_Type is
   begin
      return Type_Table.Element
        (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Get_Type;

   -----------------------
   -- Has_Default_Value --
   -----------------------

   function Has_Default_Value (Item : Root_Kit_Type)
                               return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return True;
   end Has_Default_Value;

   -----------------------
   -- Has_Default_Value --
   -----------------------

   overriding function Has_Default_Value
     (Item : String_Type)
      return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Default_Value;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   function Haskell_Type_Name (Item : Root_Kit_Type) return String is
   begin
      return Item.Haskell_Name;
   end Haskell_Type_Name;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   overriding
   function Haskell_Type_Name (Item : Integer_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Int";
   end Haskell_Type_Name;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   overriding
   function Haskell_Type_Name (Item : Float_Type) return String is
   begin
      if Item.Long then
         return "Double";
      else
         return "Float";
      end if;
   end Haskell_Type_Name;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   overriding
   function Haskell_Type_Name (Item : Boolean_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Bool";
   end Haskell_Type_Name;

   -----------------------
   -- Haskell_Type_Name --
   -----------------------

   overriding
   function Haskell_Type_Name (Item : String_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "String";
   end Haskell_Type_Name;

   ----------------------
   -- Is_External_Type --
   ----------------------

   function Is_External_Type
     (Item : Root_Kit_Type'Class)
      return Boolean
   is (Item in External_Type);

   ---------------------
   -- Is_Reference_To --
   ---------------------

   function Is_Reference_To
     (Item       : Root_Kit_Type;
      Table_Name : String)
      return Boolean
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Table_Name);
   begin
      return False;
   end Is_Reference_To;

   ---------------------
   -- Is_Reference_To --
   ---------------------

   overriding function Is_Reference_To
     (Item       : Table_Reference_Type_Record;
      Table_Name : String)
      return Boolean
   is
   begin
      return Item.Ada_Name = Kit.Names.Ada_Name (Table_Name);
   end Is_Reference_To;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Item : Root_Kit_Type) return Boolean is
      pragma Unreferenced (Item);
   begin
      return False;
   end Is_String;

   ---------------
   -- Is_String --
   ---------------

   overriding
   function Is_String (Item : String_Type) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   ------------------------
   -- Is_Table_Reference --
   ------------------------

   function Is_Table_Reference (Item : Root_Kit_Type) return Boolean is
      pragma Unreferenced (Item);
   begin
      return False;
   end Is_Table_Reference;

   ------------------------
   -- Is_Table_Reference --
   ------------------------

   overriding
   function Is_Table_Reference
     (Item : Table_Reference_Type_Record)
      return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_Table_Reference;

   ------------------
   -- Is_Type_Name --
   ------------------

   function Is_Type_Name (Name : String) return Boolean is
   begin
      return Type_Table.Contains
        (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Is_Type_Name;

   -----------------------
   -- Iterate_All_Types --
   -----------------------

   procedure Iterate_All_Types
     (Process : not null access procedure (User_Type : Kit_Type))
   is
      use Type_Maps;
      It : Cursor := Type_Table.First;
   begin
      while Has_Element (It) loop
         Process (Element (It));
         Next (It);
      end loop;
   end Iterate_All_Types;

   --------------------------------
   -- Iterate_User_Defined_Types --
   --------------------------------

   procedure Iterate_User_Defined_Types
     (Process : not null access procedure (User_Type : Kit_Type))
   is
      use Type_Maps;
      It : Cursor := Type_Table.First;
   begin
      while Has_Element (It) loop
         if Element (It).User_Defined then
            Process (Element (It));
         end if;
         Next (It);
      end loop;
   end Iterate_User_Defined_Types;

   ----------------
   -- Last_Value --
   ----------------

   function Last_Value (Of_Type : Root_Kit_Type)
                         return Syn.Expression'Class
   is
   begin
      return Syn.Object
        (Root_Kit_Type'Class (Of_Type).Ada_Name
         & "'Last");
   end Last_Value;

   ----------------
   -- Last_Value --
   ----------------

   overriding
   function Last_Value (Item : String_Type)
                        return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Syn.Object ("(1 => Character'Last)");
   end Last_Value;

   ----------------
   -- Last_Value --
   ----------------

   overriding
   function Last_Value (Item : Table_Reference_Type_Record)
                        return Syn.Expression'Class
   is
   begin
      return Syn.Object
        (Root_Kit_Type'Class (Item).Ada_Name & "_Reference'Last");
   end Last_Value;

   ----------------
   -- Local_Type --
   ----------------

   function Local_Type
     (From_External_Type : Root_Kit_Type'Class)
      return Kit_Type
   is (External_Type (From_External_Type).Local_Type);

   -----------------------
   -- New_External_Type --
   -----------------------

   procedure New_External_Type
     (Base_Type              : Kit_Type;
      External_Package_Name  : String;
      External_Type_Name     : String;
      To_Database_Function   : String;
      From_Database_Function : String;
      To_String_Function     : String;
      From_String_Function   : String)
   is
      use Ada.Strings.Unbounded;
      Ext_Type  : constant Kit_Type :=
                    new External_Type'
                      (Kit.Names.Root_Named_Object with
                       Size             => Base_Type.Size,
                       User_Defined     => True,
                       Local_Type       => Base_Type,
                       External_Package =>
                         To_Unbounded_String (External_Package_Name),
                       External_Name    =>
                         To_Unbounded_String (External_Type_Name),
                       To_Database      =>
                         To_Unbounded_String (To_Database_Function),
                       From_Database    =>
                         To_Unbounded_String (From_Database_Function),
                       To_String        =>
                         To_Unbounded_String (To_String_Function),
                       From_String      =>
                         To_Unbounded_String (From_String_Function));
   begin
      Ext_Type.Create (External_Type_Name);
      Type_Table.Insert
        (To_Unbounded_String (External_Type_Name), Ext_Type);
   end New_External_Type;

   --------------
   -- New_Type --
   --------------

   procedure New_Type (New_Type  : Kit_Type) is
   begin
      Type_Table.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String (New_Type.Name),
         New_Type);
   end New_Type;

   --------------------
   -- Record_Subtype --
   --------------------

   function Record_Subtype (Item : Root_Kit_Type) return String is
   begin
      return Root_Kit_Type'Class (Item).Return_Subtype;
   end Record_Subtype;

   --------------------
   -- Record_Subtype --
   --------------------

   overriding
   function Record_Subtype (Item : String_Type) return String is
      L : constant String :=
            Ada.Strings.Fixed.Trim (Natural'Image (Item.Length),
                                    Ada.Strings.Left);
   begin
      return "Kit.Strings.String_Type (" & L & ")";
   end Record_Subtype;

   --------------------
   -- Record_Subtype --
   --------------------

   overriding
   function Record_Subtype (Item : Text_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Kit.Text.Text_Type";
   end Record_Subtype;

   -----------------------------
   -- Reference_Database_Type --
   -----------------------------

   function Reference_Database_Type
     (Of_Kit_Type : Root_Kit_Type)
      return Syn.Expression'Class
   is
   begin
      return Syn.Object
        ("Kit_Type.Get_By_Name ("""
         & Root_Kit_Type'Class (Of_Kit_Type).Internal_Database_Name
         & """).Reference");
   end Reference_Database_Type;

   ---------------------------
   -- Referenced_Table_Name --
   ---------------------------

   function Referenced_Table_Name
     (Item : Root_Kit_Type'Class)
      return String
   is
   begin
      return Table_Reference_Type_Record (Item).Ada_Name;
   end Referenced_Table_Name;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype (Item : Boolean_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Boolean";
   end Return_Subtype;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype (Item : Float_Type) return String is
   begin
      if Item.Long then
         return "Long_Float";
      else
         return "Float";
      end if;
   end Return_Subtype;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype (Item : Integer_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "Integer";
   end Return_Subtype;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype (Item : String_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "String";
   end Return_Subtype;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype
     (Item : Table_Reference_Type_Record)
      return String
   is
   begin
      if Item.Name = "" then
         return "Marlowe.Database_Index";
      else
         return Item.Ada_Name & "_Reference";
      end if;
   end Return_Subtype;

   --------------------
   -- Return_Subtype --
   --------------------

   overriding
   function Return_Subtype (Item : Text_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "String";
   end Return_Subtype;

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value
     (Value_Type  : Root_Kit_Type;
      Target_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Value_Type);
   begin
      return Syn.Object (Target_Name);
   end Return_Value;

   ------------------
   -- Return_Value --
   ------------------

   overriding function Return_Value
     (Value_Type  : External_Type;
      Target_Name : String)
      return Syn.Expression'Class
   is
      use Ada.Strings.Unbounded;
   begin
      return Syn.Expressions.New_Function_Call_Expression
        (To_String (Value_Type.External_Package)
         & "."
         & To_String (Value_Type.From_Database),
         Target_Name);
   end Return_Value;

   ------------------
   -- Return_Value --
   ------------------

   overriding
   function Return_Value
     (Value_Type  : String_Type;
      Target_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Value_Type);
   begin
      return Syn.Object
        (Target_Name & ".Text (1 .. " & Target_Name & ".Length)");
   end Return_Value;

   ------------------
   -- Return_Value --
   ------------------

   overriding function Return_Value
     (Value_Type  : Text_Type;
      Target_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Value_Type);
   begin
      return Syn.Object
        ("Kit.Text.To_String (Marlowe_Keys.Handle, " & Target_Name & ")");
   end Return_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Value_Type  : Root_Kit_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class)
   is
      pragma Unreferenced (Value_Type);
   begin
      Sequence.Append
        (Syn.Statements.New_Assignment_Statement
           (Target_Name,
            Syn.Object (Value_Name)));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Value_Type  : String_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class)
   is
      pragma Unreferenced (Value_Type);
   begin
      Sequence.Append
        (Syn.Statements.New_Assignment_Statement
           (Target_Name & ".Length",
            Syn.Object (Value_Name & "'Length")));
      Sequence.Append
        (Syn.Statements.New_Assignment_Statement
           (Target_Name & ".Text (1 .. " & Value_Name & "'Length)",
            Syn.Object (Value_Name)));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Value_Type  : Text_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class)
   is
      pragma Unreferenced (Value_Type);
      use Syn;
   begin
      Sequence.Append
        (Syn.Statements.New_Procedure_Call_Statement
           ("Kit.Text.Set_Text",
            Object ("Marlowe_Keys.Handle"),
            Object (Value_Name),
            Object (Target_Name)));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Value_Type  : External_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class)
   is
      use Syn;
      use Ada.Strings.Unbounded;
   begin
      Sequence.Append
        (Syn.Statements.New_Assignment_Statement
           (Target_Name,
            Syn.Expressions.New_Function_Call_Expression
              (To_String (Value_Type.External_Package
               & "." & To_String (Value_Type.To_Database)),
               Object (Value_Name))));
   end Set_Value;

   ----------
   -- Size --
   ----------

   function Size (Item : Root_Kit_Type) return Natural is
   begin
      return Item.Size;
   end Size;

   ----------------------
   -- Standard_Boolean --
   ----------------------

   function Standard_Boolean return Kit_Type is
   begin
      return Standard_Boolean_Type;
   end Standard_Boolean;

   --------------------
   -- Standard_Float --
   --------------------

   function Standard_Float return Kit_Type is
   begin
      return Standard_Float_Type;
   end Standard_Float;

   ----------------------
   -- Standard_Integer --
   ----------------------

   function Standard_Integer return Kit_Type is
   begin
      return Standard_Integer_Type;
   end Standard_Integer;

   -------------------------
   -- Standard_Long_Float --
   -------------------------

   function Standard_Long_Float return Kit_Type is
   begin
      return Standard_Long_Float_Type;
   end Standard_Long_Float;

   ---------------------------
   -- Standard_Long_Integer --
   ---------------------------

   function Standard_Long_Integer   return Kit_Type is
   begin
      return Standard_Long_Integer_Type;
   end Standard_Long_Integer;

   ----------------------
   -- Standard_Natural --
   ----------------------

   function Standard_Natural return Kit_Type is
   begin
      return Standard_Natural_Type;
   end Standard_Natural;

   -----------------------
   -- Standard_Positive --
   -----------------------

   function Standard_Positive return Kit_Type is
   begin
      return Standard_Positive_Type;
   end Standard_Positive;

   --------------------------
   -- Standard_Record_Type --
   --------------------------

   function Standard_Record_Type return Kit_Type is
   begin
      return Standard_Rec_Type;
   end Standard_Record_Type;

   ---------------------
   -- Standard_String --
   ---------------------

   function Standard_String (Length : Positive) return Kit_Type is
      use Ada.Strings.Unbounded;
      Name : constant String :=
               Standard_String_Name (Length);
      U_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Type_Table.Contains (U_Name) then
         return Type_Table.Element (U_Name);
      else
         declare
            String_Record : String_Type;
            Result : Kit_Type;
         begin
            String_Record.Create (Name);
            String_Record.User_Defined := False;
            String_Record.Size := Length;
            String_Record.Length := Length;
            Result := new String_Type'(String_Record);
            Type_Table.Insert (U_Name, Result);
            return Result;
         end;
      end if;
   end Standard_String;

   --------------------------
   -- Standard_String_Name --
   --------------------------

   function Standard_String_Name (Length : Natural) return String is
      Length_Image : String := Natural'Image (Length);
   begin
      Length_Image (1) := '_';
      return "String" & Length_Image;
   end Standard_String_Name;

   -------------------
   -- Standard_Text --
   -------------------

   function Standard_Text return Kit_Type is
      use Ada.Strings.Unbounded;
      Name : constant String := "text";
      U_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Type_Table.Contains (U_Name) then
         return Type_Table.Element (U_Name);
      else
         declare
            Text_Record : Text_Type;
            Result      : Kit_Type;
         begin
            Text_Record.Create (Name);
            Text_Record.User_Defined := False;
            Text_Record.Size := Text_Type_Record_Size;
            Result := new Text_Type'(Text_Record);
            return Result;
         end;
      end if;
   end Standard_Text;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   function Storage_Array_Transfer
     (Item          : Root_Kit_Type'Class;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset;
      Proc_Name     : String)
      return Syn.Statement'Class
   is
      pragma Unreferenced (Item);
      use System.Storage_Elements;
      use Ada.Strings, Ada.Strings.Fixed;
      use Syn, Syn.Statements;
      S : constant String :=
            Trim (Storage_Offset'Image (Start), Left);
      F : constant String :=
            Trim (Storage_Offset'Image (Finish), Left);
      Store  : constant String :=
                 Storage_Name & " (" & S & " .. " & F & ")";
   begin
      return New_Procedure_Call_Statement
        ("Marlowe.Key_Storage." & Proc_Name,
         Object (Object_Name), Object (Store));
   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   function Storage_Array_Transfer
     (Item          : Root_Kit_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
      Proc_Name : constant String :=
                    (if To_Storage
                     then "To_Storage"
                     else "From_Storage");
   begin
      return Storage_Array_Transfer
        (Item, Object_Name, Storage_Name, Start, Finish,
         Proc_Name);
   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   overriding function Storage_Array_Transfer
     (Item          : Table_Reference_Type_Record;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
   begin
      if To_Storage then
         return Storage_Array_Transfer
           (Item,
            "Marlowe.Database_Index (" & Object_Name & ")",
            Storage_Name, Start, Finish,
            "To_Storage");
      else
         declare
            Block : Syn.Blocks.Block_Type;
         begin
            Block.Add_Declaration
              (Syn.Declarations.New_Object_Declaration
                 ("T", "Marlowe.Database_Index"));
            Block.Add_Statement
              (Storage_Array_Transfer
                 (Item, "T",
                  Storage_Name, Start, Finish,
                  "From_Storage"));
            Block.Add_Statement
              (Syn.Statements.New_Assignment_Statement
                 (Object_Name,
                  Syn.Object (Item.Ada_Name & "_Reference (T)")));
            return Syn.Statements.Declare_Statement (Block);
         end;
      end if;
   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   overriding function Storage_Array_Transfer
     (Item          : Long_Integer_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
      pragma Unreferenced (Item);
      use System.Storage_Elements;
      use Ada.Strings, Ada.Strings.Fixed;
      use Syn, Syn.Statements;
      S      : constant String :=
                 Trim (Storage_Offset'Image (Start), Left);
      F      : constant String :=
                 Trim (Storage_Offset'Image (Finish), Left);
      Store  : constant String :=
                 Storage_Name & " (" & S & " .. " & F & ")";
   begin
      if To_Storage then
         return New_Procedure_Call_Statement
           ("Integer_64_Storage.To_Storage",
            Object (Object_Name), Object (Store));
      else
         return New_Procedure_Call_Statement
           ("Integer_64_Storage.From_Storage",
            Object (Object_Name), Object (Store));
      end if;
   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   overriding function Storage_Array_Transfer
     (Item          : String_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
   begin
      if To_Storage then
         return Item.Storage_Array_Transfer
           (Object_Name & ".Text (1 .. " & Object_Name & ".Length)",
            Storage_Name,
            Start, Finish,
            "To_Storage");
      else
         declare
            use System.Storage_Elements;
            use Ada.Strings, Ada.Strings.Fixed;
            use Syn, Syn.Statements;
            S         : constant String :=
                          Trim (Storage_Offset'Image (Start), Left);
            F         : constant String :=
                          Trim (Storage_Offset'Image (Finish), Left);
            Store     : constant String :=
                          Storage_Name & " (" & S & " .. " & F & ")";
         begin
            return New_Procedure_Call_Statement
              ("Marlowe.Key_Storage.From_Storage",
               Object (Object_Name & ".Text"),
               Object (Object_Name & ".Length"),
               Object (Store));
         end;
      end if;

   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   overriding function Storage_Array_Transfer
     (Item          : Text_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class
   is
      pragma Unreferenced (Item);
      use System.Storage_Elements;
      use Ada.Strings, Ada.Strings.Fixed;
      use Syn, Syn.Statements;
      S         : constant String :=
                    Trim (Storage_Offset'Image (Start), Left);
      F         : constant String :=
                    Trim (Storage_Offset'Image (Finish), Left);
      Store     : constant String :=
                    Storage_Name & " (" & S & " .. " & F & ")";
   begin
      if To_Storage then
         return New_Procedure_Call_Statement
           ("Kit.Text.To_Storage",
            Object (Object_Name), Object (Store));
      else
         return New_Procedure_Call_Statement
           ("Kit.Text.From_Storage",
            Object (Object_Name), Object (Store));
      end if;
   end Storage_Array_Transfer;

   --------------------------
   -- Table_Reference_Type --
   --------------------------

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type
   is
      Reference_Record : Table_Reference_Type_Record;
      Result           : Kit_Type;
   begin
      Reference_Record.Create (Table_Name);
      Reference_Record.Size := 8;
      Result := new Table_Reference_Type_Record'(Reference_Record);
      return Result;
   end Table_Reference_Type;

   --------------------
   -- To_Declaration --
   --------------------

   function To_Declaration
     (From_Type : Root_Kit_Type)
      return Syn.Declaration'Class
   is
   begin
      return Syn.Declarations.New_Full_Type_Declaration
        (From_Type.Ada_Name,
         Syn.New_Derived_Type
           (Root_Kit_Type'Class (From_Type).Return_Subtype));
   end To_Declaration;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Item        : Root_Kit_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      use Syn, Syn.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         Object (Object_Name),
         Literal (Item.Size));
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   overriding function To_Storage_Array
     (Item        : Long_Integer_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
      use Syn, Syn.Expressions;
   begin
      return New_Function_Call_Expression
        ("Integer_64_Storage.To_Storage_Array",
         Object (Object_Name));
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   overriding function To_Storage_Array
     (Item        : Table_Reference_Type_Record;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
      use Syn, Syn.Expressions;
      Convert_To_Index : constant Expression'Class :=
                           New_Function_Call_Expression
                             ("Marlowe.Database_Index",
                              Object_Name);
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         Argument => Convert_To_Index);
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   overriding function To_Storage_Array
     (Item        : Float_Type;
      Object_Name : String)
      return Syn.Expression'Class
   is
      pragma Unreferenced (Item);
      use Syn, Syn.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         Object_Name);
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

--     overriding
--     function To_Storage_Array
--       (Item        : String_Type;
--        Object_Name : String)
--        return Syn.Expression'Class
--     is
--        use Syn.Expressions;
--     begin
--        return New_Function_Call_Expression
--          ("Kit.Runtime.To_Storage",
--           Object_Name,
--           Ada.Strings.Fixed.Trim
--             (Natural'Image (Item.Length),
--              Ada.Strings.Left));
--     end To_Storage_Array;

   ----------------------------------
   -- Unconstrained_Record_Subtype --
   ----------------------------------

   function Unconstrained_Record_Subtype
     (Item : Root_Kit_Type)
      return String
   is
   begin
      return Root_Kit_Type'Class (Item).Record_Subtype;
   end Unconstrained_Record_Subtype;

   ----------------------------------
   -- Unconstrained_Record_Subtype --
   ----------------------------------

   overriding
   function Unconstrained_Record_Subtype
     (Item : String_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "Kit.Strings.String_Type";
   end Unconstrained_Record_Subtype;

   ------------------------
   -- Update_Record_Type --
   ------------------------

   procedure Update_Record_Type
     (Record_Count : Natural;
      Record_Name  : access
        function (Index : Positive) return String)
   is
   begin
      for I in 1 .. Record_Count loop
         Enumerated.Enumerated_Type'Class (Standard_Rec_Type.all).Add_Literal
           (Record_Name (I));
      end loop;
   end Update_Record_Type;

end Kit.Schema.Types;
