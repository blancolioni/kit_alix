private with Ada.Containers.Vectors;

with Aquarius.Drys;

with Marlowe;

with Kit.Fields;
with Kit.Names;

package Kit.Tables is

   type Table_Type is
     new Kit.Names.Root_Named_Object with private;

   overriding
   procedure Create (Item : in out Table_Type;
                     Name : in     String);

   function Magic_Number
     (Item : Table_Type)
      return Natural;

   function Reference_Index
     (Item : Table_Type)
      return Marlowe.Table_Index;

   function Reference_Type (Item : Table_Type) return String;
   function Implementation_Record_Type (Item : Table_Type) return String;

   function Has_String_Type (Item : Table_Type) return Boolean;

   procedure Scan_Fields
     (Table    : Table_Type;
      Process : not null access procedure
        (Field : Kit.Fields.Field_Type'Class));

   type Field_Cursor is private;
   type Base_Cursor is private;
   type Key_Cursor is private;

   Null_Key_Cursor : constant Key_Cursor;

   function First_Field (Table : Table_Type) return Field_Cursor;
   function Contains_Field (Table : Table_Type;
                            Name     : String)
                           return Boolean;

   function First_Base (Table : Table_Type) return Base_Cursor;
   function Contains_Base (Table : Table_Type;
                           Name     : String)
                          return Boolean;

   procedure Next (Position : in out Field_Cursor);
   procedure Next (Position : in out Base_Cursor);

   function Element (Position : Field_Cursor)
                    return Kit.Fields.Field_Type'Class;
   function Has_Element (Position : Field_Cursor)
                        return Boolean;

   procedure Scan_Keys (Table : Table_Type;
                        Process  : not null access
                          procedure (Item : Key_Cursor));

   procedure Scan_Keys
     (Table : Table_Type;
      Process          : not null access procedure
        (Base   : Table_Type'Class;
         Key    : Key_Cursor));

   procedure Scan_Keys
     (Table    : Table_Type;
      Containing_Field : Kit.Fields.Field_Type'Class;
      Process          : not null access procedure
        (Base   : Table_Type'Class;
         Key    : Key_Cursor));

   procedure Iterate (Table : Table_Type;
                      Process  : not null access
                        procedure (Item : Kit.Fields.Field_Type'Class));

   procedure Iterate_All (Table : Table_Type'Class;
                          Process  : not null access
                            procedure (Table : Table_Type'Class;
                                       Field : Kit.Fields.Field_Type'Class));

   procedure Iterate_All (Table : Table_Type'Class;
                          Process  : not null access
                            procedure (Table : Table_Type'Class;
                                       Field : Field_Cursor));

   function Is_Compound_Key (Position : Key_Cursor)
                             return Boolean;
   function Is_Unique (Position : Key_Cursor)
                       return Boolean;
   function Name (Position : Key_Cursor)
                  return String;
   function Ada_Name (Position : Key_Cursor)
                      return String;
   function Key_Size (Position : Key_Cursor)
                      return Positive;
   function Has_Element (Position : Key_Cursor)
                         return Boolean;

   function To_Storage (Table       : Table_Type'Class;
                        Key_Table   : Table_Type'Class;
                        Object_Name : String;
                        Key         : Key_Cursor)
                        return Aquarius.Drys.Expression'Class;

   function To_Storage (Key_Value_Name   : String;
                        Index_Value_Name : String;
                        Key              : Key_Cursor)
                        return Aquarius.Drys.Expression'Class;

   function Element (Position : Base_Cursor)
                    return Table_Type'Class;
   function Has_Element (Position : Base_Cursor)
                        return Boolean;

   procedure Iterate (Table     : Table_Type;
                      Process   : not null access
                        procedure (Item : Table_Type'Class);
                      Inclusive : Boolean;
                      Table_First : Boolean := False);

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Fields.Field_Type'Class;
      Is_Key    : in     Boolean;
      Is_Unique : in     Boolean   := False);

   procedure Add_Base
     (Table     : in out Table_Type;
      Item      : in     Table_Type'Class);

private

   type Field_Access is access all Kit.Fields.Field_Type'Class;
   type Compound_Field_Access is
     access all Kit.Fields.Compound_Field_Type'Class;

   type Table_Field (Is_Compound : Boolean := False) is
      record
         Is_Key        : Boolean;
         Is_Unique_Key : Boolean;
         case Is_Compound is
            when False =>
               Field    : Field_Access;
            when True =>
               Compound_Field : Compound_Field_Access;
         end case;
      end record;

   type Table_Field_Access is access Table_Field;

   package Field_Vectors is
      new Ada.Containers.Vectors (Positive, Table_Field_Access);

   type Field_Cursor is new Field_Vectors.Cursor;
   type Key_Cursor is new Field_Vectors.Cursor;

   Null_Key_Cursor : constant Key_Cursor :=
                       Key_Cursor (Field_Vectors.No_Element);

   type Table_Access is access all Table_Type'Class;

   package Table_Vectors is
      new Ada.Containers.Vectors (Positive, Table_Access);

   type Base_Cursor is new Table_Vectors.Cursor;

   type Table_Type is
     new Kit.Names.Root_Named_Object with
      record
         Index           : Marlowe.Table_Index;
         Bases           : Table_Vectors.Vector;
         Fields          : Field_Vectors.Vector;
         Magic           : Natural;
         Has_String_Type : Boolean := False;
      end record;

end Kit.Tables;
