with Marlowe.Key_Storage;

package body Kit.SQL.Constraints is

   function To_Constraint
     (Table_Name : String;
      Field_Name : String;
      Class      : Constraint_Class;
      Value      : Field_Value_Type;
      Inclusive  : Boolean := True)
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
      Value      : Field_Value_Type;
      Inclusive  : Boolean)
      return Constraint_Type
   is
   begin
      return To_Constraint
        (Table_Name, Field_Name, Maximum, Value, Inclusive);
   end Maximum_Value;

   -------------------
   -- Minimum_Value --
   -------------------

   function Minimum_Value
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type;
      Inclusive  : Boolean)
      return Constraint_Type
   is
   begin
      return To_Constraint
        (Table_Name, Field_Name, Minimum, Value, Inclusive);
   end Minimum_Value;

   ------------------
   -- Not_Equal_To --
   ------------------

   function Not_Equal_To
     (Table_Name : String;
      Field_Name : String;
      Value      : Field_Value_Type)
      return Constraint_Type
   is
   begin
      return To_Constraint (Table_Name, Field_Name, Not_Equal, Value);
   end Not_Equal_To;

   ------------------
   -- Satisfied_By --
   ------------------

   function Satisfied_By
     (Constraint : Constraint_Type;
      Field_Type : Kit.SQL.Database.Data_Type'Class;
      Data       : System.Storage_Elements.Storage_Array)
      return Boolean
   is
   begin
      case Constraint.Class is
         when Always =>
            return True;
         when Never =>
            return False;
         when Maximum | Minimum | Equal | Not_Equal =>
            declare
               use System.Storage_Elements;
               use all type Marlowe.Key_Storage.Compare_Result;
               Constraint_Data : constant Storage_Array :=
                                   To_Storage
                                     (Constraint.Value,
                                      Field_Type,
                                      Data);
               Compare : constant Marlowe.Key_Storage.Compare_Result :=
                           Marlowe.Key_Storage.Compare
                             (Left  => Data,
                              Right => Constraint_Data);
            begin
               return (case Compare is
                          when Less =>
                            Constraint.Class in Maximum | Not_Equal,
                          when Equal =>
                            Constraint.Inclusive or else
                       Constraint.Class = Equal,
                          when Greater =>
                            Constraint.Class in Minimum | Not_Equal);
            end;
      end case;
   end Satisfied_By;

   ------------------
   -- Satisfied_By --
   ------------------

   function Satisfied_By
     (List      : Constraint_List'Class;
      Reference : Kit.SQL.Database.Record_Reference)
      return Boolean
   is
   begin
      for Constraint of List loop
         declare
            Field : constant Kit.SQL.Database.Field_Reference :=
                      Kit.SQL.Database.Get_Field
                        (Kit.SQL.Database.Get_Table_Reference (Reference),
                         -Constraint.Field_Name);
         begin
            if not Constraint.Satisfied_By
              (Kit.SQL.Database.Get_Field_Type (Field),
               Kit.SQL.Database.Get_Field_Value
                 (Reference, Field))
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Satisfied_By;

   -------------------
   -- To_Constraint --
   -------------------

   function To_Constraint
     (Table_Name : String;
      Field_Name : String;
      Class      : Constraint_Class;
      Value      : Field_Value_Type;
      Inclusive  : Boolean := True)
      return Constraint_Type
   is
   begin
      return Constraint_Type'
        (Table_Name => +Table_Name,
         Field_Name => +Field_Name,
         Class      => Class,
         Value      => Value,
         Inclusive  => Inclusive);
   end To_Constraint;

   --------------------
   -- To_Field_Value --
   --------------------

   function To_Field_Value
     (Value : Integer)
      return Field_Value_Type
   is
   begin
      return (Integer_Value, Value);
   end To_Field_Value;

   --------------------
   -- To_Field_Value --
   --------------------

   function To_Field_Value
     (Value : String)
      return Field_Value_Type
   is
   begin
      return (String_Value, +Value);
   end To_Field_Value;

   ----------------
   -- To_Storage --
   ----------------

   function To_Storage
     (Value      : Field_Value_Type;
      Value_Type : Kit.SQL.Database.Data_Type'Class;
      Default    : System.Storage_Elements.Storage_Array)
      return System.Storage_Elements.Storage_Array
   is
   begin
      case Value.Class is
         when Empty_Value =>
            return Default;
         when Integer_Value =>
            return Marlowe.Key_Storage.To_Storage_Array
              (Value.Val_Integer, Value_Type.Size);
         when Float_Value =>
            return Marlowe.Key_Storage.To_Storage_Array
              (Value.Val_Float);
         when String_Value =>
            declare
               use System.Storage_Elements;
               Data : Storage_Array (1 .. Value_Type.Size);
            begin
               Marlowe.Key_Storage.Bounded_String_To_Storage
                 (-Value.Val_String, Data);
               return Data;
            end;
      end case;
   end To_Storage;

end Kit.SQL.Constraints;
