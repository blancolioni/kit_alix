with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Approximate_IO;

package body Abydos.Values is

   package Approximate_Float_IO is
     new Approximate_IO (Float);

   type Numeric_Value_Record is
     abstract new Root_Value_Record with null record;

   type Null_Value_Record is
     new Numeric_Value_Record with null record;

   overriding
   function To_Boolean (X : Null_Value_Record) return Boolean;

   overriding
   function To_Integer (X : Null_Value_Record) return Integer;
   function To_Float (X : Null_Value_Record) return Float;
   function To_String (X : Null_Value_Record) return String;
   function Type_Name (X : Null_Value_Record) return String;

   type Integer_Value_Record is
     new Numeric_Value_Record with
      record
         Value : Integer;
      end record;

   function To_Boolean (X : Integer_Value_Record) return Boolean;
   function To_Integer (X : Integer_Value_Record) return Integer;
   function To_Float (X : Integer_Value_Record) return Float;
   function To_String (X : Integer_Value_Record) return String;
   function Type_Name (X : Integer_Value_Record) return String;

   type Float_Value_Record is
     new Numeric_Value_Record with
      record
         Value : Float;
      end record;

   function To_Boolean (X : Float_Value_Record) return Boolean;
   function To_Integer (X : Float_Value_Record) return Integer;
   function To_Float (X : Float_Value_Record) return Float;
   function To_String (X : Float_Value_Record) return String;
   function Type_Name (X : Float_Value_Record) return String;

   type String_Access is access String;

   type String_Value_Record is
     new Root_Value_Record with
      record
         Value : String_Access;
      end record;

   function To_Boolean (X : String_Value_Record) return Boolean;
   function To_Integer (X : String_Value_Record) return Integer;
   function To_Float (X : String_Value_Record) return Float;
   function To_String (X : String_Value_Record) return String;
   function Type_Name (X : String_Value_Record) return String;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (V : in out Value) is
   begin
      V.V.Refs := V.V.Refs + 1;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (V        : in out Value;
      New_Item : Value)
   is
      pragma Unreferenced (V);
      pragma Unreferenced (New_Item);
   begin
      null;
   end Append;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (V : in out Value) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Root_Value_Record'Class,
                                        Value_Record_Access);
      T : Value_Record_Access := V.V;
   begin
      V.V := null;
      if T /= null then
         T.Refs := T.Refs - 1;
         if T.Refs = 0 then
            Free (T);
         end if;
      end if;
   end Finalize;

   -----------
   -- Index --
   -----------

   function Index
     (V     : Value;
      Index : Value)
      return Value
   is
      pragma Unreferenced (V);
      pragma Unreferenced (Index);
   begin
      return Null_Value;
   end Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (V : in out Value) is
      pragma Unreferenced (V);
   begin
      null;
   end Initialize;

   ---------------
   -- Null_Fill --
   ---------------

   function Null_Fill (V : Value;
                       Fill : Value)
                       return Value
   is
   begin
      if V.V.all in Null_Value_Record'Class then
         return Fill;
      else
         return V;
      end if;
   end Null_Fill;

   ----------------
   -- Null_Value --
   ----------------

   function Null_Value return Value is
   begin
      return Result : Value do
         Result.V := new Null_Value_Record;
      end return;
   end Null_Value;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (X : Value) return Boolean is
   begin
      return X.V.To_Boolean;
   end To_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (X : Null_Value_Record) return Boolean is
      pragma Unreferenced (X);
   begin
      return False;
   end To_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (X : Integer_Value_Record) return Boolean is
   begin
      return X.Value /= 0;
   end To_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (X : Float_Value_Record) return Boolean is
   begin
      return X.Value /= 0.0;
   end To_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (X : String_Value_Record) return Boolean is
      V : constant String := Ada.Characters.Handling.To_Lower (X.Value.all);
   begin
      return V /= "" and then V /= "no" and then V /= "false"
        and then V /= "0" and then V /= "0.0";
   end To_Boolean;

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Value) return Float is
   begin
      return X.V.To_Float;
   end To_Float;

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Null_Value_Record) return Float is
      pragma Unreferenced (X);
   begin
      return 0.0;
   end To_Float;

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Integer_Value_Record) return Float is
   begin
      return Float (X.Value);
   end To_Float;

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Float_Value_Record) return Float is
   begin
      return X.Value;
   end To_Float;

   --------------
   -- To_Float --
   --------------

   function To_Float (X : String_Value_Record) return Float is
   begin
      return Float'Value (X.Value.all);
   exception
      when others =>
         return 0.0;
   end To_Float;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Value) return Integer is
   begin
      return X.V.To_Integer;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Null_Value_Record) return Integer is
      pragma Unreferenced (X);
   begin
      return 0;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Integer_Value_Record) return Integer is
   begin
      return X.Value;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Float_Value_Record) return Integer is
   begin
      return Integer (X.Value);
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : String_Value_Record) return Integer is
   begin
      return Integer'Value (X.Value.all);
   exception
      when others =>
         return 0;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Value) return String is
   begin
      return X.V.To_String;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String  (X : Value;
                        Default : String)
                        return String
   is
   begin
      return To_String (Null_Fill (X, To_Value (Default)));
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Null_Value_Record) return String is
      pragma Unreferenced (X);
   begin
      return "";
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Integer_Value_Record) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (X.Value),
                                     Ada.Strings.Left);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Float_Value_Record) return String is
   begin
      return Approximate_Float_IO.Image (X.Value);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : String_Value_Record) return String is
   begin
      return X.Value.all;
   end To_String;

   --------------
   -- To_Value --
   --------------

   function To_Value (X : Boolean) return Value is
   begin
      return Result : Value do
         Result.V := new Integer_Value_Record'(1, Boolean'Pos (X));
      end return;
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (X : Integer) return Value is
   begin
      return Result : Value do
         Result.V := new Integer_Value_Record'(1, X);
      end return;
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (X : Float) return Value is
   begin
      return Result : Value do
         Result.V := new Float_Value_Record'(1, X);
      end return;
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (X : String) return Value is
   begin
      return Result : Value do
         Result.V := new String_Value_Record'(1, new String'(X));
      end return;
   end To_Value;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (X : Null_Value_Record) return String is
      pragma Unreferenced (X);
   begin
      return "()";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (X : Integer_Value_Record) return String is
      pragma Unreferenced (X);
   begin
      return "Integer";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (X : Float_Value_Record) return String is
      pragma Unreferenced (X);
   begin
      return "Float";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (X : String_Value_Record) return String is
      pragma Unreferenced (X);
   begin
      return "String";
   end Type_Name;

end Abydos.Values;
