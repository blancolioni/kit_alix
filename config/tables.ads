private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
private with System.Storage_Elements;

package {Database}.Tables is

   type Database_Table is tagged private;

   function Get_Table (Table_Name : String)
                       return Database_Table;

   function Has_Element (Table : Database_Table'Class) return Boolean;

   function Name (Table : Database_Table'Class) return String
   with Pre => Table.Has_Element;

   type Record_Reference is private;
   Null_Record_Reference : constant Record_Reference;
   function To_String (Reference : Record_Reference) return String;

   type Database_Record is tagged limited private;

   function Has_Element (Rec : Database_Record'Class) return Boolean;
   function Reference (Rec : Database_Record'Class) return Record_Reference;

   function Get (Table        : Database_Table'Class;
                 Reference    : Record_Reference)
                 return Database_Record;

   function Get (Table        : Database_Table'Class;
                 Key_Name     : String;
                 Key_Value    : String)
                 return Database_Record;
   function Get
     (From_Record : Database_Record'Class;
      Field_Name  : String)
      return String;

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Process      : not null access procedure
        (Item : Database_Record'Class));

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      Key_Value    : String;
      Process      : not null access procedure
        (Item : Database_Record'Class));

   procedure Iterate
     (Table        : Database_Table'Class;
      Key_Name     : String;
      First        : String;
      Last         : String;
      Process      : not null access procedure
        (Item : Database_Record'Class));

private

   type Database_Table is tagged
      record
         Index : Marlowe.Table_Index;
      end record;

   type Record_Reference is new Marlowe.Database_Index;
   Null_Record_Reference : constant Record_Reference := 0;

   package Storage_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive,
        System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Database_Record is
     limited new Ada.Finalization.Limited_Controlled with
      record
         Table   : Marlowe.Table_Index;
         Index   : Marlowe.Database_Index;
         Value   : Storage_Vectors.Vector;
      end record;
end {Database}.Tables;
