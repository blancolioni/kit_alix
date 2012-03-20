private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Finalization;
private with Ada.Strings.Unbounded.Hash;

package Abydos.Values is

   Evaluation_Error : exception;

   type Value is private;

   type Array_Of_Values is array (Positive range <>) of Value;

   function To_Boolean (X : Value) return Boolean;
   function To_Integer (X : Value) return Integer;
   function To_Float   (X : Value) return Float;
   function To_String  (X : Value) return String;
   function To_String  (X : Value;
                        Default : String)
                        return String;

   function To_Value (X : Boolean) return Value;
   function To_Value (X : Integer) return Value;
   function To_Value (X : Float) return Value;
   function To_Value (X : String) return Value;

   function Index (V     : Value;
                   Index : Value)
                  return Value;
   procedure Append (V        : in out Value;
                     New_Item : Value);

   function Null_Value return Value;

   function Null_Fill (V : Value;
                       Fill : Value)
                       return Value;

private

   type Root_Value_Record is abstract tagged
      record
         Refs : Natural := 1;
      end record;

   function To_Boolean (X : Root_Value_Record) return Boolean
                        is abstract;
   function To_Integer (X : Root_Value_Record) return Integer
                        is abstract;
   function To_Float   (X : Root_Value_Record) return Float
                        is abstract;
   function To_String  (X : Root_Value_Record) return String
                        is abstract;
   function Type_Name (X : Root_Value_Record) return String
                       is abstract;

   type Value_Record_Access is access all Root_Value_Record'Class;

   type Value is new Ada.Finalization.Controlled with
      record
         V : Value_Record_Access;
      end record;

   procedure Initialize (V : in out Value);
   procedure Finalize (V : in out Value);
   procedure Adjust (V : in out Value);

   package Value_Vectors is
      new Ada.Containers.Vectors (Positive, Value);

   package Value_Associations is
      new Ada.Containers.Hashed_Maps
     (Ada.Strings.Unbounded.Unbounded_String,
      Value,
      Ada.Strings.Unbounded.Hash,
      Ada.Strings.Unbounded."=");

--     type Get_Record_Field is access
--       function (Table : Marlowe.Table_Index;
--                 Index : Marlowe.Database_Index;
--                 Field : String)
--                return String;
--
--     type Set_Record_Field is access
--       procedure (Table : Marlowe.Table_Index;
--                  Index : Marlowe.Database_Index;
--                  Field : String;
--                  Value : String);
--
--     type Value_Record (Class : Value_Class := Null_Value) is
--        record
--           Count : Natural;
--           case Class is
--              when Null_Value =>
--                 null;
--              when Integer_Value =>
--                 Int_Val : Integer;
--              when Float_Value =>
--                 Float_Val : Float;
--              when String_Value =>
--                 String_Val : Ada.Strings.Unbounded.Unbounded_String;
--              when Database_Record_Value =>
--                 Table    : Marlowe.Table_Index;
--                 Index    : Marlowe.Database_Index;
--                 Get      : Get_Record_Field;
--                 Set      : Set_Record_Field;
--              when Array_Value =>
--                 Array_Elements : Value_Vectors.Vector;
--              when Association_Value =>
--                 Associations   : Value_Associations.Map;
--           end case;
--        end record;

end Abydos.Values;
