private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

with System.Storage_Elements;

package Kit.SQL.Constraints is

   type Constraint_Class is
     (Always, Never, Maximum, Minimum, Equal, Not_Equal);

   type Field_Value_Type is private;

   No_Value : constant Field_Value_Type;

   function Has_Value (Value : Field_Value_Type) return Boolean;

   function To_Field_Value
     (Integer_Value : Integer;
      Size          : Natural)
      return Field_Value_Type;

   function To_Field_Value
     (String_Value : String;
      Size         : Natural)
      return Field_Value_Type;

   function To_Storage
     (Value   : Field_Value_Type;
      Default : System.Storage_Elements.Storage_Array)
      return System.Storage_Elements.Storage_Array;

   type Constraint_Type is tagged private;

   function Always return Constraint_Type;
   function Never return Constraint_Type;

   function Minimum_Value
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type;

   function Maximum_Value
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type;

   function Equal_To
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type;

   function Table_Name
     (Constraint : Constraint_Type)
      return String;

   function Field_Name
     (Constraint : Constraint_Type)
      return String;

   function Class
     (Constraint : Constraint_Type)
      return Constraint_Class;

   function Value
     (Constraint : Constraint_Type)
      return Field_Value_Type;

   type Constraint_List is tagged private;

   procedure Add
     (List       : in out Constraint_List'Class;
      Constraint : Constraint_Type'Class);

   procedure Iterate
     (List : Constraint_List'Class;
      Process : not null access
        procedure (Constraint : Constraint_Type'Class));

private

   package Storage_Array_Holders is
     new Ada.Containers.Indefinite_Holders
       (System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Field_Value_Type is
      record
         Holder : Storage_Array_Holders.Holder;
      end record;

   No_Value : constant Field_Value_Type :=
                (Holder => Storage_Array_Holders.Empty_Holder);

   function Has_Value (Value : Field_Value_Type) return Boolean
   is (not Value.Holder.Is_Empty);

   type Constraint_Type is tagged
      record
         Table_Name : Ada.Strings.Unbounded.Unbounded_String;
         Field_Name : Ada.Strings.Unbounded.Unbounded_String;
         Class      : Constraint_Class;
         Value      : Field_Value_Type;
      end record;

   function Table_Name
     (Constraint : Constraint_Type)
      return String
   is (-Constraint.Table_Name);

   function Field_Name
     (Constraint : Constraint_Type)
      return String
   is (-Constraint.Field_Name);

   function Class
     (Constraint : Constraint_Type)
      return Constraint_Class
   is (Constraint.Class);

   function Value
     (Constraint : Constraint_Type)
      return Field_Value_Type
   is (Constraint.Value);

   function Always return Constraint_Type
   is (Constraint_Type'
         (Class      => Always, others => <>));

   function Never return Constraint_Type
   is (Constraint_Type'
         (Class      => Never, others => <>));

   package Constraint_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Constraint_Type);

   type Constraint_List is
     new Constraint_Lists.List with null record;

end Kit.SQL.Constraints;
