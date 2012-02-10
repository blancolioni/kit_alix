with Ada.Strings.Fixed;

with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

package body Kit.Types is

   type Integer_Type is new Kit_Type with
      record
         Low, High : Integer;
      end record;

   overriding
   function Return_Subtype (Item : Integer_Type) return String;

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
   -- Standard_Integer --
   ----------------------

   function Standard_Integer return Kit_Type'Class is
   begin
      return Result : Integer_Type do
         Result.Create ("integer");
         Result.Size := Integer'Size / 8;
         Result.Low := Integer'First;
         Result.High := Integer'Last;
      end return;
   end Standard_Integer;

   ----------------------
   -- Standard_Natural --
   ----------------------

   function Standard_Natural return Kit_Type'Class is
   begin
      return Result : Integer_Type do
         Result.Create ("natural");
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
