with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Declarations;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

with Kit.Schema.Types.Enumerated;

package body Kit.Schema.Types is

   package Type_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Kit_Type'Class,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   Type_Table : Type_Maps.Map;

   type Integer_Type is new Kit_Type with
      record
         Low, High : Integer;
      end record;

   overriding
   function Return_Subtype (Item : Integer_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Integer_Type)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Default_Value (Item : Integer_Type)
                           return Aquarius.Drys.Expression'Class;

   type Float_Type is new Kit_Type with
      record
         Long : Boolean;
      end record;

   overriding
   function Return_Subtype (Item : Float_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Float_Type)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Default_Value (Item : Float_Type)
                           return Aquarius.Drys.Expression'Class;

   overriding
   function To_Storage_Array
     (Item        : Float_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

   type Boolean_Type is new Kit_Type with null record;

   overriding
   function Return_Subtype (Item : Boolean_Type) return String;

   overriding
   function Create_Database_Record
     (For_Type : Boolean_Type)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Default_Value (Item : Boolean_Type)
                           return Aquarius.Drys.Expression'Class;

   type Table_Reference_Type_Record is
     new Kit_Type with null record;

   overriding
   function Return_Subtype
     (Item : Table_Reference_Type_Record)
      return String;

   overriding
   function To_Storage_Array
     (Item        : Table_Reference_Type_Record;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

   overriding
   function Storage_Array_Transfer
     (Item          : Table_Reference_Type_Record;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class;

   overriding
   function First_Value (Item : Table_Reference_Type_Record)
                         return Aquarius.Drys.Expression'Class;

   overriding
   function Last_Value (Item : Table_Reference_Type_Record)
                        return Aquarius.Drys.Expression'Class;

   overriding
   function Convert_To_String (Item   : Table_Reference_Type_Record;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class;

   overriding
   function Convert_From_String (Item   : Table_Reference_Type_Record;
                               Object_Name : String)
                                 return Aquarius.Drys.Expression'Class;

   overriding
   function Create_Database_Record
     (For_Type : Table_Reference_Type_Record)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Default_Value (Item : Table_Reference_Type_Record)
                           return Aquarius.Drys.Expression'Class;

   type String_Type is new Kit_Type with
      record
         Length : Positive;
      end record;

--     overriding
--     function To_Storage_Array
--       (Item        : String_Type;
--        Object_Name : String)
--        return Aquarius.Drys.Expression'Class;

   overriding
   function Convert_To_String (Item   : String_Type;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class;

   overriding
   function Convert_From_String (Item   : String_Type;
                               Object_Name : String)
                                 return Aquarius.Drys.Expression'Class;

   overriding
   function Return_Value
     (Value_Type  : String_Type;
      Target_Name : String)
      return Aquarius.Drys.Expression'Class;

   overriding
   procedure Set_Value
     (Value_Type  : String_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Aquarius.Drys.Statement_Sequencer'Class);

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
                         return Aquarius.Drys.Expression'Class;

   overriding
   function Last_Value (Item : String_Type)
                        return Aquarius.Drys.Expression'Class;

   overriding
   function Create_Database_Record
     (For_Type : String_Type)
      return Aquarius.Drys.Statement'Class;

   overriding
   function Has_Default_Value (Item : String_Type)
                               return Boolean;

   overriding
   function Default_Value (Item : String_Type)
                           return Aquarius.Drys.Expression'Class;

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
      return Aquarius.Drys.Statement'Class;

   ----------------------
   -- Argument_Subtype --
   ----------------------

   function Argument_Subtype (Item : Kit_Type) return String is
   begin
      return Kit_Type'Class (Item).Return_Subtype;
   end Argument_Subtype;

   -------------------------
   -- Convert_From_String --
   -------------------------

   function Convert_From_String (Item   : Kit_Type;
                                 Object_Name : String)
                                 return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "'Value",
         Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   function Convert_From_String (Item   : Table_Reference_Type_Record;
                                 Object_Name : String)
                                 return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "_Reference" & "'Value",
         Object_Name);
   end Convert_From_String;

   -------------------------
   -- Convert_From_String --
   -------------------------

   function Convert_From_String (Item   : String_Type;
                                 Object_Name : String)
                                 return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object (Object_Name);
   end Convert_From_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   function Convert_To_String (Item   : Kit_Type;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "'Image",
         Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   function Convert_To_String (Item   : Table_Reference_Type_Record;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        (Item.Ada_Name & "_Reference" & "'Image",
         Object_Name);
   end Convert_To_String;

   -----------------------
   -- Convert_To_String --
   -----------------------

   function Convert_To_String (Item   : String_Type;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object (Object_Name);
   end Convert_To_String;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   overriding
   function Create_Database_Record
     (For_Type : Integer_Type)
      return Aquarius.Drys.Statement'Class
   is
      use Aquarius.Drys;
      Result : Aquarius.Drys.Statements.Procedure_Call_Statement'Class :=
                 Aquarius.Drys.Statements.New_Procedure_Call_Statement
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
     (For_Type : Float_Type)
      return Aquarius.Drys.Statement'Class
   is
      use Aquarius.Drys;
      Record_Name : constant String :=
                      (if For_Type.Long then "Long_Float" else "Float");
      Result : Aquarius.Drys.Statements.Procedure_Call_Statement'Class :=
                 Aquarius.Drys.Statements.New_Procedure_Call_Statement
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

   function Create_Database_Record
     (For_Type : Boolean_Type)
      return Aquarius.Drys.Statement'Class
   is
      use Aquarius.Drys;
      use Aquarius.Drys.Declarations;
      use Aquarius.Drys.Expressions;
      use Aquarius.Drys.Statements;
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
      Block        : Aquarius.Drys.Blocks.Block_Type;
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
      return Aquarius.Drys.Statement'Class
   is
      use Aquarius.Drys;
      Result : Aquarius.Drys.Statements.Procedure_Call_Statement'Class :=
                 Aquarius.Drys.Statements.New_Procedure_Call_Statement
                   ("Kit_Reference.Create");
   begin
      Result.Add_Actual_Argument (Literal (8));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument
        (Object
           ("Kit_Record.First_By_Name ("""
            & For_Type.Standard_Name
            & """).Reference"));
      return Result;
   end Create_Database_Record;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   function Create_Database_Record
     (For_Type : String_Type)
      return Aquarius.Drys.Statement'Class
   is
      use Aquarius.Drys;
      Result : Aquarius.Drys.Statements.Procedure_Call_Statement'Class :=
                 Aquarius.Drys.Statements.New_Procedure_Call_Statement
                   ("Kit_String.Create");
   begin
      Result.Add_Actual_Argument (Literal (For_Type.Size));
      Result.Add_Actual_Argument (Literal (For_Type.Standard_Name));
      Result.Add_Actual_Argument (Literal (For_Type.Length));
      return Result;
   end Create_Database_Record;

   ---------------------------
   -- Create_Standard_Types --
   ---------------------------

   procedure Create_Standard_Types is
   begin
      New_Type (Standard_Integer);
      New_Type (Standard_Positive);
      New_Type (Standard_Natural);
      New_Type (Standard_Float);
      New_Type (Standard_Long_Float);
      New_Type (Standard_Boolean);
      New_Type (Standard_Record_Type);
   end Create_Standard_Types;

   -------------------
   -- Default_Value --
   -------------------

   overriding
   function Default_Value (Item : Integer_Type)
                           return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Literal (Integer'Max (0, Item.Low));
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Item : Float_Type)
                           return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object ("0.0");
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Item : Boolean_Type)
                           return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object ("False");
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Item : Table_Reference_Type_Record)
                           return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Literal (0);
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Item : String_Type)
                           return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object ("((others => Character'Val (0)), 0)");
   end Default_Value;

   -----------------
   -- First_Value --
   -----------------

   function First_Value (Of_Type : Kit_Type)
                         return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Object
        (Kit_Type'Class (Of_Type).Ada_Name
         & "'First");
   end First_Value;

   -----------------
   -- First_Value --
   -----------------

   overriding
   function First_Value (Item : String_Type)
                         return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object ("(1 => Character'First)");
   end First_Value;

   -----------------
   -- First_Value --
   -----------------

   overriding
   function First_Value (Item : Table_Reference_Type_Record)
                         return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Object
        (Kit_Type'Class (Item).Ada_Name & "_Reference'First");
   end First_Value;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Name : String) return Kit_Type'Class is
   begin
      return Type_Table.Element
        (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Get_Type;

   -----------------------
   -- Has_Default_Value --
   -----------------------

   function Has_Default_Value (Item : Kit_Type)
                               return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return True;
   end Has_Default_Value;

   -----------------------
   -- Has_Default_Value --
   -----------------------

   function Has_Default_Value (Item : String_Type)
                               return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Default_Value;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Item : Kit_Type) return Boolean is
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

   function Is_Table_Reference (Item : Kit_Type) return Boolean is
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
     (Process : not null access procedure (User_Type : Kit_Type'Class))
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
     (Process : not null access procedure (User_Type : Kit_Type'Class))
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

   function Last_Value (Of_Type : Kit_Type)
                         return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Object
        (Kit_Type'Class (Of_Type).Ada_Name
         & "'Last");
   end Last_Value;

   ----------------
   -- Last_Value --
   ----------------

   overriding
   function Last_Value (Item : String_Type)
                        return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius.Drys.Object ("(1 => Character'Last)");
   end Last_Value;

   ----------------
   -- Last_Value --
   ----------------

   overriding
   function Last_Value (Item : Table_Reference_Type_Record)
                        return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Object
        (Kit_Type'Class (Item).Ada_Name & "_Reference'Last");
   end Last_Value;

   --------------
   -- New_Type --
   --------------

   procedure New_Type (New_Type  : Kit_Type'Class) is
   begin
      Type_Table.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String (New_Type.Name),
         New_Type);
   end New_Type;

   --------------------
   -- Record_Subtype --
   --------------------

   function Record_Subtype (Item : Kit_Type) return String is
   begin
      return Kit_Type'Class (Item).Return_Subtype;
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

   -----------------------------
   -- Reference_Database_Type --
   -----------------------------

   function Reference_Database_Type
     (Of_Kit_Type : Kit_Type)
      return Aquarius.Drys.Expression'Class
   is
   begin
      return Aquarius.Drys.Object
        ("Kit_Type.First_By_Name (""" & Of_Kit_Type.Standard_Name
         & """).Reference");
   end Reference_Database_Type;

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
      return Item.Ada_Name & "_Reference";
   end Return_Subtype;

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value
     (Value_Type  : Kit_Type;
      Target_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Value_Type);
   begin
      return Aquarius.Drys.Object (Target_Name);
   end Return_Value;

   ------------------
   -- Return_Value --
   ------------------

   overriding
   function Return_Value
     (Value_Type  : String_Type;
      Target_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Value_Type);
   begin
      return Aquarius.Drys.Object
        (Target_Name & ".Text (1 .. " & Target_Name & ".Length)");
   end Return_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Value_Type  : Kit_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Aquarius.Drys.Statement_Sequencer'Class)
   is
      pragma Unreferenced (Value_Type);
   begin
      Sequence.Append
        (Aquarius.Drys.Statements.New_Assignment_Statement
           (Target_Name,
            Aquarius.Drys.Object (Value_Name)));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding
   procedure Set_Value
     (Value_Type  : String_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Aquarius.Drys.Statement_Sequencer'Class)
   is
      pragma Unreferenced (Value_Type);
   begin
      Sequence.Append
        (Aquarius.Drys.Statements.New_Assignment_Statement
           (Target_Name & ".Length",
            Aquarius.Drys.Object (Value_Name & "'Length")));
      Sequence.Append
        (Aquarius.Drys.Statements.New_Assignment_Statement
           (Target_Name & ".Text (1 .. " & Value_Name & "'Length)",
            Aquarius.Drys.Object (Value_Name)));
   end Set_Value;

   ----------
   -- Size --
   ----------

   function Size (Item : Kit_Type) return Natural is
   begin
      return Item.Size;
   end Size;

   ----------------------
   -- Standard_Boolean --
   ----------------------

   function Standard_Boolean return Kit_Type'Class is
   begin
      return Result : Enumerated.Enumerated_Type do
         Result.Create ("boolean");
         Kit_Type (Result).User_Defined := False;
         Result.Add_Literal ("False");
         Result.Add_Literal ("True");
      end return;
   end Standard_Boolean;

   --------------------
   -- Standard_Float --
   --------------------

   function Standard_Float return Kit_Type'Class is
   begin
      return Result : Float_Type do
         Result.Create ("float");
         Result.User_Defined := False;
         Result.Size := Float'Size / 8;
         Result.Long := False;
      end return;
   end Standard_Float;

   ----------------------
   -- Standard_Integer --
   ----------------------

   function Standard_Integer return Kit_Type'Class is
   begin
      return Result : Integer_Type do
         Result.Create ("integer");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := Integer'First;
         Result.High := Integer'Last;
      end return;
   end Standard_Integer;

   -------------------------
   -- Standard_Long_Float --
   -------------------------

   function Standard_Long_Float return Kit_Type'Class is
   begin
      return Result : Float_Type do
         Result.Create ("long_float");
         Result.User_Defined := False;
         Result.Size := Long_Float'Size / 8;
         Result.Long := True;
      end return;
   end Standard_Long_Float;

   ----------------------
   -- Standard_Natural --
   ----------------------

   function Standard_Natural return Kit_Type'Class is
   begin
      return Result : Integer_Type do
         Result.Create ("natural");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := 0;
         Result.High := Integer'Last;
      end return;
   end Standard_Natural;

   -----------------------
   -- Standard_Positive --
   -----------------------

   function Standard_Positive return Kit_Type'Class is
   begin
      return Result : Integer_Type do
         Result.Create ("positive");
         Result.User_Defined := False;
         Result.Size := Integer'Size / 8;
         Result.Low := 1;
         Result.High := Integer'Last;
      end return;
   end Standard_Positive;

   --------------------------
   -- Standard_Record_Type --
   --------------------------

   function Standard_Record_Type return Kit_Type'Class is
   begin
      return Result : Kit.Schema.Types.Enumerated.Record_Type_Enumeration do
         Result.Create ("record_type");
         Result.Add_Literal ("R_None");
         Kit_Type (Result).User_Defined := False;
      end return;
   end Standard_Record_Type;

   ---------------------
   -- Standard_String --
   ---------------------

   function Standard_String (Length : Positive) return Kit_Type'Class is
      use Ada.Strings.Unbounded;
      Name : constant String :=
               Standard_String_Name (Length);
      U_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Type_Table.Contains (U_Name) then
         return Type_Table.Element (U_Name);
      else
         return Result : String_Type do
            Result.Create (Name);
            Result.User_Defined := False;
            Result.Size := Length;
            Result.Length := Length;
            Type_Table.Insert (U_Name, Result);
         end return;
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

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   function Storage_Array_Transfer
     (Item          : Kit_Type'Class;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset;
      Proc_Name     : String)
      return Aquarius.Drys.Statement'Class
   is
      pragma Unreferenced (Item);
      use System.Storage_Elements;
      use Ada.Strings, Ada.Strings.Fixed;
      use Aquarius.Drys, Aquarius.Drys.Statements;
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
     (Item          : Kit_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class
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

   overriding
   function Storage_Array_Transfer
     (Item          : Table_Reference_Type_Record;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class
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
            Block : Aquarius.Drys.Blocks.Block_Type;
         begin
            Block.Add_Declaration
              (Aquarius.Drys.Declarations.New_Object_Declaration
                 ("T", "Marlowe.Database_Index"));
            Block.Add_Statement
              (Storage_Array_Transfer
                 (Item, "T",
                  Storage_Name, Start, Finish,
                  "From_Storage"));
            Block.Add_Statement
              (Aquarius.Drys.Statements.New_Assignment_Statement
                 (Object_Name,
                  Aquarius.Drys.Object (Item.Ada_Name & "_Reference (T)")));
            return Aquarius.Drys.Statements.Declare_Statement (Block);
         end;
      end if;
   end Storage_Array_Transfer;

   ----------------------------
   -- Storage_Array_Transfer --
   ----------------------------

   function Storage_Array_Transfer
     (Item          : String_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class
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
            use Aquarius.Drys, Aquarius.Drys.Statements;
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

   --------------------------
   -- Table_Reference_Type --
   --------------------------

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type'Class
   is
   begin
      return Result : Table_Reference_Type_Record do
         Result.Create (Table_Name);
         Result.Size := 8;
      end return;
   end Table_Reference_Type;

   --------------------
   -- To_Declaration --
   --------------------

   function To_Declaration
     (From_Type : Kit_Type)
      return Aquarius.Drys.Declaration'Class
   is
   begin
      return Aquarius.Drys.Declarations.New_Full_Type_Declaration
        (From_Type.Ada_Name,
         Aquarius.Drys.New_Derived_Type
           (Kit_Type'Class (From_Type).Return_Subtype));
   end To_Declaration;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Item        : Kit_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys, Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         Object (Object_Name),
         Literal (Item.Size));
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Item        : Table_Reference_Type_Record;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
      use Aquarius.Drys, Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         New_Function_Call_Expression
           ("Marlowe.Database_Index",
            Object_Name));
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Item        : Float_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      pragma Unreferenced (Item);
      use Aquarius.Drys, Aquarius.Drys.Expressions;
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
--        return Aquarius.Drys.Expression'Class
--     is
--        use Aquarius.Drys.Expressions;
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

   function Unconstrained_Record_Subtype (Item : Kit_Type) return String is
   begin
      return Kit_Type'Class (Item).Record_Subtype;
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
      Name    : constant Ada.Strings.Unbounded.Unbounded_String :=
                  Ada.Strings.Unbounded.To_Unbounded_String
                    ("record_type");
      Current : Kit.Schema.Types.Enumerated.Enumerated_Type'Class :=
                  Kit.Schema.Types.Enumerated.Enumerated_Type'Class
                    (Type_Table.Element (Name));
   begin
      for I in 1 .. Record_Count loop
         Current.Add_Literal (Record_Name (I));
      end loop;
      Type_Table.Replace (Name, Current);
   end Update_Record_Type;

end Kit.Schema.Types;
