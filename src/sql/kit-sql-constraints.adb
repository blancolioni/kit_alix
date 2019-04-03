with Marlowe.Key_Storage;

package body Kit.SQL.Constraints is

   function To_Field_Value
     (Storage : System.Storage_Elements.Storage_Array)
      return Field_Value_Type;

   function To_Constraint
     (Table_Name : String;
      Field_Name : String;
      Class      : Constraint_Class;
      Value      : Field_Value_Type)
      return Constraint_Type;

   ---------
   -- Add --
   ---------

   procedure Add
     (List       : in out Constraint_List'Class;
      Constraint : Constraint_Type'Class)
   is
   begin
      List.Append (Constraint_Type (Constraint));
   end Add;

   --------------
   -- Equal_To --
   --------------

   function Equal_To
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type
   is
   begin
      return To_Constraint (Table_Name, Field_Name, Equal, Value);
   end Equal_To;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (List    : Constraint_List'Class;
      Process : not null access
        procedure (Constraint : Constraint_Type'Class))
   is
   begin
      for Constraint of List loop
         Process (Constraint);
      end loop;
   end Iterate;

   -------------------
   -- Maximum_Value --
   -------------------

   function Maximum_Value
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type
   is
   begin
      return To_Constraint (Table_Name, Field_Name, Maximum, Value);
   end Maximum_Value;

   -------------------
   -- Minimum_Value --
   -------------------

   function Minimum_Value
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type
   is
   begin
      return To_Constraint (Table_Name, Field_Name, Minimum, Value);
   end Minimum_Value;

   -------------------
   -- To_Constraint --
   -------------------

   function To_Constraint
     (Table_Name : String;
      Field_Name : String;
      Class      : Constraint_Class;
      Value      : Field_Value_Type)
      return Constraint_Type
   is
   begin
      return Constraint_Type'
        (Table_Name => +Table_Name,
         Field_Name => +Field_Name,
         Class      => Class,
         Value      => Value);
   end To_Constraint;

   --------------------
   -- To_Field_Value --
   --------------------

   function To_Field_Value
     (Integer_Value : Integer;
      Size          : Natural)
      return Field_Value_Type
   is
   begin
      return To_Field_Value
        (Marlowe.Key_Storage.To_Storage_Array
           (Integer_Value,
            System.Storage_Elements.Storage_Count (Size)));
   end To_Field_Value;

   --------------------
   -- To_Field_Value --
   --------------------

   function To_Field_Value
     (String_Value : String;
      Size         : Natural)
      return Field_Value_Type
   is
   begin
      return To_Field_Value
        (Marlowe.Key_Storage.To_Storage_Array
           (String_Value,
            System.Storage_Elements.Storage_Count (Size)));
   end To_Field_Value;

   --------------------
   -- To_Field_Value --
   --------------------

   function To_Field_Value
     (Storage : System.Storage_Elements.Storage_Array)
      return Field_Value_Type
   is
   begin
      return Field_Value_Type'
        (Holder => Storage_Array_Holders.To_Holder (Storage));
   end To_Field_Value;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage
     (Value   : Field_Value_Type;
      Default : System.Storage_Elements.Storage_Array)
      return System.Storage_Elements.Storage_Array
   is
   begin
      if Has_Value (Value) then
         return Value.Holder.Element;
      else
         return Default;
      end if;
   end To_Storage;

end Kit.SQL.Constraints;
