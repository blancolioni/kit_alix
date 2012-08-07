private with Ada.Containers.Vectors;

with System.Storage_Elements;

with Aquarius.Drys;

with Marlowe;

with Kit.Schema.Fields;
with Kit.Names;
with Kit.Schema.Types;

package Kit.Schema.Tables is

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

   function Package_Name
     (Item : Table_Type)
      return String;

   function Interface_Name
     (Item : Table_Type)
      return String;

   function Type_Name
     (Item : Table_Type)
      return String;

   function Implementation_Name
     (Item : Table_Type)
      return String;

   function Length
     (Item : Table_Type)
      return System.Storage_Elements.Storage_Count;

   function Reference_Type (Item : Table_Type) return String;
   function References_Table (Item    : Table_Type;
                              Other   : Table_Type'Class)
                              return Boolean;

   function Implementation_Record_Type (Item : Table_Type) return String;

   function Has_String_Type (Item : Table_Type) return Boolean;
   function Has_Key_Field (Item : Table_Type) return Boolean;
   function Has_Compound_Key_Field (Item : Table_Type) return Boolean;

   function Is_Key_Field (Item : Table_Type;
                          Field : Kit.Schema.Fields.Field_Type'Class)
                          return Boolean;

   procedure Scan_Fields
     (Table    : Table_Type;
      Process : not null access procedure
        (Field : Kit.Schema.Fields.Field_Type'Class));

--     procedure Scan_Fields
--       (Table    : Table_Type;
--        Process : not null access procedure
--          (Field       : Kit.Schema.Fields.Field_Type'Class;
--           Field_Start : System.Storage_Elements.Storage_Offset));

   type Field_Cursor is private;
   type Base_Cursor is private;
   type Key_Cursor is private;

   Null_Key_Cursor : constant Key_Cursor;

   function First_Field (Table : Table_Type) return Field_Cursor;
   function Contains_Field (Table : Table_Type;
                            Name     : String)
                           return Boolean;

   function Inherited_Field (Table : Table_Type;
                             Field : Kit.Schema.Fields.Field_Type'Class)
                             return Boolean;

   function First_Base (Table : Table_Type) return Base_Cursor;
   function Contains_Base (Table : Table_Type;
                           Name     : String)
                          return Boolean;

   procedure Next (Position : in out Field_Cursor);
   procedure Next (Position : in out Base_Cursor);

   function Element (Position : Field_Cursor)
                    return Kit.Schema.Fields.Field_Type'Class;
   function Has_Element (Position : Field_Cursor)
                        return Boolean;

   function Field_Start (Table : Table_Type;
                         Field : Kit.Schema.Fields.Field_Type'Class)
                         return System.Storage_Elements.Storage_Offset;

   function Base_Start (Table : Table_Type;
                        Base  : Table_Type'Class)
                        return System.Storage_Elements.Storage_Offset;

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
      Containing_Field : Kit.Schema.Fields.Field_Type'Class;
      Process          : not null access procedure
        (Table  : Table_Type'Class;
         Base   : Table_Type'Class;
         Key    : Key_Cursor));

   procedure Iterate (Table : Table_Type;
                      Process  : not null access
                        procedure (Item : Kit.Schema.Fields.Field_Type'Class));

   procedure Iterate_All
     (Table : Table_Type'Class;
      Process  : not null access
        procedure (Table : Table_Type'Class;
                   Field : Kit.Schema.Fields.Field_Type'Class));

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
   function Standard_Name (Position : Key_Cursor)
                      return String;
   function Key_Size (Position : Key_Cursor)
                      return Positive;
   function Key_Type (Position : Key_Cursor)
                      return Kit.Schema.Types.Kit_Type'Class;
   function Has_Element (Position : Key_Cursor)
                         return Boolean;

   function Field_Count (Position : Key_Cursor) return Natural;
   function Field (Position : Key_Cursor;
                   Index    : Positive)
                   return Kit.Schema.Fields.Field_Type'Class;

   function To_Storage (Table       : Table_Type'Class;
                        Base_Table  : Table_Type'Class;
                        Key_Table   : Table_Type'Class;
                        Object_Name : String;
                        Key         : Key_Cursor;
                        With_Index  : Boolean)
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

   procedure Add_Compound_Key_Field
     (Table        : in out Table_Type;
      Compound_Key : in out Kit.Schema.Fields.Compound_Field_Type;
      Field_Name   : String)
   with Pre => Table.Contains_Field (Field_Name);

   function Compound_Field_Count
     (Key : Key_Cursor)
      return Natural;

   function Compound_Field
     (Key : Key_Cursor;
      Index : Positive)
      return Kit.Schema.Fields.Field_Type'Class;

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Schema.Fields.Field_Type'Class;
      Is_Key    : in     Boolean;
      Is_Unique : in     Boolean   := False);

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Schema.Fields.Compound_Field_Type'Class;
      Is_Unique : in     Boolean);

   procedure Add_Base
     (Table     : in out Table_Type;
      Item      : in     Table_Type'Class);

   function Database_Index_Component
     (Table       : Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type'Class)
      return String;

   function Database_Index_Component
     (Table       : Table_Type'Class;
      Object_Name : String;
      Base_1      : Table_Type'Class;
      Base_2      : Table_Type'Class)
      return String;

   function Base_Component_Name
     (Table : Table_Type'Class)
      return String;

   function Base_Index_Name
     (Table : Table_Type'Class)
      return String;

   function Base_Field_Name
     (Table  : Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type'Class;
      Field       : Kit.Schema.Fields.Field_Type'Class)
      return String;

   function Index_Image
     (Table : Table_Type'Class)
      return String;

   function Key_Reference_Name
     (Table : Table_Type'Class;
      Key   : Key_Cursor)
      return String;

   function Key_To_Storage
     (Table       : Table_Type'Class;
      Key         : Key_Cursor;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

private

   type Field_Access is access all Kit.Schema.Fields.Field_Type'Class;
   type Compound_Field_Access is
     access all Kit.Schema.Fields.Compound_Field_Type'Class;

   type Table_Field (Is_Compound : Boolean := False) is
      record
         Is_Key        : Boolean;
         Is_Unique_Key : Boolean;
         case Is_Compound is
            when False =>
               Start    : System.Storage_Elements.Storage_Offset;
               Length   : System.Storage_Elements.Storage_Count;
               Field    : Field_Access;
            when True =>
               Compound_Field : Compound_Field_Access;
         end case;
      end record;

   type Table_Field_Access is access Table_Field;

   function Same_Field (Left, Right : Table_Field_Access) return Boolean;

   package Field_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Field_Access,
                                 Same_Field);

   type Field_Cursor is new Field_Vectors.Cursor;
   type Key_Cursor is new Field_Vectors.Cursor;

   Null_Key_Cursor : constant Key_Cursor :=
                       Key_Cursor (Field_Vectors.No_Element);

   type Table_Access is access all Table_Type'Class;

   function Same_Table (Left, Right : Table_Access) return Boolean;

   package Table_Vectors is
      new Ada.Containers.Vectors (Positive, Table_Access, Same_Table);

   type Base_Cursor is new Table_Vectors.Cursor;

   package Base_Layout_Vectors is
     new Ada.Containers.Vectors (Positive, Marlowe.Table_Index,
                                 Marlowe."=");

   type Table_Type is
     new Kit.Names.Root_Named_Object with
      record
         Fields_Length          : System.Storage_Elements.Storage_Count := 0;
         Bases_Length           : System.Storage_Elements.Storage_Count := 0;
         Header_Length          : System.Storage_Elements.Storage_Count := 4;
         Index                  : Marlowe.Table_Index;
         Bases                  : Table_Vectors.Vector;
         Base_Layout            : Base_Layout_Vectors.Vector;
         Fields                 : Field_Vectors.Vector;
         Magic                  : Natural;
         Has_String_Type        : Boolean := False;
         Has_Key_Field          : Boolean := False;
         Has_Compound_Key_Field : Boolean := False;
      end record;

end Kit.Schema.Tables;
