with Ada.Strings.Fixed;

with Approximate_IO;

with Marlowe.Key_Storage;

with Kit.Db.Kit_Enumeration;
with Kit.Db.Kit_Field;
with Kit.Db.Kit_Literal;
with Kit.Db.Kit_Record;

with Kit.Server.Tables;

package body Kit.Server.Storage is

   package Approximate_Float_IO is
     new Approximate_IO (Float);

   -----------------------
   -- Storage_To_String --
   -----------------------

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type)
      return String
   is
   begin
      case Value_Type.Top_Record is
         when Kit.Db.R_Kit_Integer =>
            declare
               X : Integer;
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               return Integer'Image (X);
            end;
         when Kit.Db.R_Kit_Float =>
            declare
               X : Float;
               Result : String (1 .. 10);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               Approximate_Float_IO.Put (Result, X);
               return Ada.Strings.Fixed.Trim (Result, Ada.Strings.Left);
            end;
         when Kit.Db.R_Kit_Long_Float =>
            declare
               X : Long_Float;
               Result : String (1 .. 10);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);
               Approximate_Float_IO.Put (Result, Float (X));
               return Ada.Strings.Fixed.Trim (Result, Ada.Strings.Left);
            end;
         when Kit.Db.R_Kit_String =>
            declare
               X : String (1 .. Value'Length);
               Last : Natural;
            begin
               Marlowe.Key_Storage.From_Storage (X, Last, Value);
               return X (1 .. Last);
            end;
         when Kit.Db.R_Kit_Enumeration =>
            declare
               use type System.Storage_Elements.Storage_Element;
               X : Marlowe.Key_Storage.Unsigned_Integer := 0;
               Enum : Kit.Db.Kit_Enumeration.Kit_Enumeration_Type :=
                        Kit.Db.Kit_Enumeration.Get_By_Name
                          (Value_Type.Name);
            begin
               Marlowe.Key_Storage.From_Storage (X, Value);

               declare
                  Lit : constant Kit.Db.Kit_Literal.Kit_Literal_Type :=
                          Kit.Db.Kit_Literal.Get_By_Enum_Value
                            (Enum.Reference, Natural (X));
               begin
                  return Lit.Name;
               end;
            end;
         when Kit.Db.R_Kit_Reference =>
            declare
               Rec : Kit.Db.Kit_Record.Kit_Record_Type :=
                       Kit.Db.Kit_Record.Get_By_Name (Value_Type.Name);
               Name_Field : Kit.Db.Kit_Field.Kit_Field_Type :=
                              Kit.Db.Kit_Field.Get_By_Record_Field
                                (Rec.Reference, "Name");
               Index      : Marlowe.Database_Index;
            begin
               Marlowe.Key_Storage.From_Storage (Index, Value);
               if Rec.Has_Element and then Name_Field.Has_Element then
                  declare
                     Item : constant Database_Record :=
                              Kit.Server.Tables.Active_Database.Get
                                (Marlowe.Table_Index (Rec.Table_Index),
                                 Index);
                     Name : constant String := Item.Get ("Name");
                  begin
                     return Name;
                  end;
               else
                  return Marlowe.Key_Storage.Image (Value);
               end if;
            end;

         when others =>
            return Marlowe.Key_Storage.Image (Value);
      end case;
   end Storage_To_String;

   -----------------------
   -- String_To_Storage --
   -----------------------

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type;
      Storage    : out System.Storage_Elements.Storage_Array)
   is
   begin
      case Value_Type.Top_Record is
         when Kit.Db.R_Kit_Integer =>
            declare
               X : constant Integer := Integer'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when Kit.Db.R_Kit_Float =>
            declare
               X : constant Float := Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when Kit.Db.R_Kit_Long_Float =>
            declare
               X : constant Long_Float := Long_Float'Value (Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;
         when Kit.Db.R_Kit_String =>
            Marlowe.Key_Storage.To_Storage (Value, Storage);
         when Kit.Db.R_Kit_Enumeration =>
            declare
               use type System.Storage_Elements.Storage_Element;
               Enum : Kit.Db.Kit_Enumeration.Kit_Enumeration_Type :=
                        Kit.Db.Kit_Enumeration.Get_By_Name
                          (Value_Type.Name);
               Lit : constant Kit.Db.Kit_Literal.Kit_Literal_Type :=
                       Kit.Db.Kit_Literal.Get_By_Enum_Name
                         (Enum.Reference, Value);
               X   : constant Marlowe.Key_Storage.Unsigned_Integer :=
                       Marlowe.Key_Storage.Unsigned_Integer (Lit.Value);
            begin
               Marlowe.Key_Storage.To_Storage (X, Storage);
            end;

         when others =>
            Storage := (others => 0);
      end case;
   end String_To_Storage;

end Kit.Server.Storage;
