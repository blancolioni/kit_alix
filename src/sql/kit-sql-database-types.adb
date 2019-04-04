with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;

with WL.Generic_Real_Images;
with WL.String_Maps;

with Marlowe.Key_Storage;

with Kit.Db.Kit_Bounded_String;
with Kit.Db.Kit_Enumeration;
with Kit.Db.Kit_Fixed_String;
with Kit.Db.Kit_Integer;
with Kit.Db.Kit_Literal;
with Kit.Db.Kit_Type;

package body Kit.SQL.Database.Types is

   package Long_Float_Images is
     new WL.Generic_Real_Images (Long_Float);

   package Type_Maps is
     new WL.String_Maps (Data_Type'Class);

   Type_Map : Type_Maps.Map;

   type Integer_Data_Type is new Data_Type with
      record
         Low, High : Integer;
      end record;

   overriding function To_String
     (With_Type : Integer_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String;

   package Enumerated_Literal_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   type Enumerated_Data_Type is new Data_Type with
      record
         Literals : Enumerated_Literal_Vectors.Vector;
      end record;

   overriding function To_String
     (With_Type : Enumerated_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String;

   type Float_Data_Type is new Data_Type with
      record
         Long : Boolean;
      end record;

   overriding function To_String
     (With_Type : Float_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String;

   type String_Data_Type is new Data_Type with
      record
         Fixed  : Boolean;
         Length : Natural;
      end record;

   overriding function To_String
     (With_Type : String_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String;

   ------------------
   -- To_Data_Type --
   ------------------

   function To_Data_Type
     (From : Kit.Db.Kit_Type_Reference)
      return Data_Type'Class
   is
      Base : constant Kit.Db.Kit_Type.Kit_Type_Type :=
               Kit.Db.Kit_Type.Get (From);

      function Create_Type return Data_Type'Class;

      -----------------
      -- Create_Type --
      -----------------

      function Create_Type return Data_Type'Class is
         use Kit.Db;
         Name : constant Ada.Strings.Unbounded.Unbounded_String :=
                  +Base.Name;
         Size : constant System.Storage_Elements.Storage_Count :=
                  System.Storage_Elements.Storage_Count (Base.Size);
      begin
         case Base.Top_Record is
            when R_Kit_Integer =>
               declare
                  use Kit.Db.Kit_Integer;
                  Kit_Integer : constant Kit_Integer_Type :=
                                  Get_Kit_Integer
                                    (Kit_Type => Base.Get_Kit_Type_Reference);
               begin
                  return Integer_Data_Type'
                    (Name => Name,
                     Size => Size,
                     Low  => Kit_Integer.Low,
                     High => Kit_Integer.High);
               end;
            when R_Kit_Float =>
               return Float_Data_Type'
                 (Name => Name,
                  Size => Size,
                  Long => False);

            when R_Kit_Long_Float =>
               return Float_Data_Type'
                 (Name => Name,
                  Size => Size,
                  Long => True);

            when R_Kit_Fixed_String =>
               declare
                  use Kit.Db.Kit_Fixed_String;
                  Kit_String : constant Kit_Fixed_String_Type :=
                                 Get_Kit_Fixed_String
                                   (Kit_Type => Base.Get_Kit_Type_Reference);
               begin
                  return String_Data_Type'
                    (Name   => Name,
                     Size   => Size,
                     Fixed  => True,
                     Length => Kit_String.Length);
               end;

            when R_Kit_Bounded_String =>
               declare
                  use Kit.Db.Kit_Bounded_String;
                  Kit_String : constant Kit_Bounded_String_Type :=
                                 Get_Kit_Bounded_String
                                   (Kit_Type => Base.Get_Kit_Type_Reference);
               begin
                  return String_Data_Type'
                    (Name   => Name,
                     Size   => Size,
                     Fixed  => False,
                     Length => Kit_String.Length);
               end;

            when R_Kit_Enumeration =>
               declare
                  use Kit.Db.Kit_Enumeration;
                  use Kit.Db.Kit_Literal;
                  Enum : constant Kit_Enumeration_Type :=
                           Get_Kit_Enumeration (Base.Get_Kit_Type_Reference);
                  Result : Enumerated_Data_Type :=
                             Enumerated_Data_Type'
                               (Name     => Name,
                                Size     => Size,
                                Literals => <>);
               begin
                  for Literal of
                    Select_By_Kit_Enumeration
                      (Enum.Get_Kit_Enumeration_Reference)
                  loop
                     while Result.Literals.Last_Index < Literal.Value loop
                        Result.Literals.Append ("");
                     end loop;
                     Result.Literals.Replace_Element
                       (Literal.Value, Literal.Name);
                  end loop;
                  return Result;
               end;

            when others =>
               return Data_Type'
                 (Name => Name,
                  Size => Size);
         end case;
      end Create_Type;

   begin
      if not Type_Map.Contains (Base.Name) then
         Type_Map.Insert
           (Base.Name, Create_Type);
      end if;

      return Type_Map.Element (Base.Name);
   end To_Data_Type;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (With_Type : Integer_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String
   is
      pragma Unreferenced (With_Type);
      Value : Integer;
   begin
      Marlowe.Key_Storage.From_Storage (Value, Data);
      return Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (With_Type : Enumerated_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String
   is
      use Marlowe.Key_Storage;
      Value : Unsigned_Integer;
   begin
      Marlowe.Key_Storage.From_Storage (Value, Data);
      if Value > Unsigned_Integer (With_Type.Literals.Last_Index) then
         return "#invalid["
           & Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left)
           & "]";
      else
         return With_Type.Literals.Element (Natural (Value));
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (With_Type : Float_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String
   is
   begin
      if With_Type.Long then
         declare
            Value : Long_Float;
         begin
            Marlowe.Key_Storage.From_Storage (Value, Data);
            return Long_Float_Images.Approximate_Image (Value);
         end;
      else
         declare
            Value : Float;
         begin
            Marlowe.Key_Storage.From_Storage (Value, Data);
            return Long_Float_Images.Approximate_Image (Long_Float (Value));
         end;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (With_Type : String_Data_Type;
      Data      : System.Storage_Elements.Storage_Array)
      return String
   is
      S : String (1 .. With_Type.Length);
      Last : Natural;
   begin
      if With_Type.Fixed then
         Marlowe.Key_Storage.Fixed_String_From_Storage (S, Data);
         Last := S'Last;
      else
         Marlowe.Key_Storage.Bounded_String_From_Storage (S, Last, Data);
      end if;
      return S (1 .. Last);
   end To_String;

end Kit.SQL.Database.Types;
