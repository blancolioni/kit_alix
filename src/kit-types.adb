with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;

with Aquarius.Drys.Declarations;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

package body Kit.Types is

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

   type Float_Type is new Kit_Type with
      record
         Long : Boolean;
      end record;

   overriding
   function Return_Subtype (Item : Float_Type) return String;

   type Boolean_Type is new Kit_Type with null record;

   overriding
   function Return_Subtype (Item : Boolean_Type) return String;

   type Table_Reference_Type_Record is
     new Kit_Type with
      record
         Table_Name : access String;
      end record;

   overriding
   function Return_Subtype
     (Item : Table_Reference_Type_Record)
      return String;

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

   ----------------------
   -- Argument_Subtype --
   ----------------------

   function Argument_Subtype (Item : Kit_Type) return String is
   begin
      return Kit_Type'Class (Item).Return_Subtype;
   end Argument_Subtype;

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
   end Create_Standard_Types;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Name : String) return Kit_Type'Class is
   begin
      return Type_Table.Element
        (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Get_Type;

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

   ------------------
   -- Is_Type_Name --
   ------------------

   function Is_Type_Name (Name : String) return Boolean is
   begin
      return Type_Table.Contains
        (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Is_Type_Name;

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
      return Kit.Names.Ada_Name (Item.Table_Name.all) & "_Reference";
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
      return Result : Boolean_Type do
         Result.Create ("boolean");
         Result.User_Defined := False;
         Result.Size := 1;
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

   ---------------------
   -- Standard_String --
   ---------------------

   function Standard_String (Length : Positive) return Kit_Type'Class is
   begin
      return Result : String_Type do
         Result.Create ("string");
         Result.User_Defined := False;
         Result.Size := Length;
         Result.Length := Length;
      end return;
   end Standard_String;

   --------------------------
   -- Table_Reference_Type --
   --------------------------

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type'Class
   is
   begin
      return Result : Table_Reference_Type_Record do
         Result.Size := 8;
         Result.Table_Name := new String'(Table_Name);
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

end Kit.Types;
