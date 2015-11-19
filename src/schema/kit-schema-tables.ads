private with Ada.Containers.Vectors;

with System.Storage_Elements;

with Aquarius.Drys;

with Marlowe;

with Kit.Schema.Fields;
with Kit.Schema.Keys;

with Kit.Names;

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
   function Has_Text_Type (Item : Table_Type) return Boolean;
   function Has_Key_Field (Item : Table_Type) return Boolean;
   function Has_Compound_Key_Field (Item : Table_Type) return Boolean;
   function Has_Display_Field (Item : Table_Type) return Boolean;

   function With_Vector_Package (Item : Table_Type) return Boolean;
   function With_Map_Package (Item : Table_Type) return Boolean;

   procedure Enable_Vector_Package
     (Item : in out Table_Type);

   procedure Enable_Map_Package
     (Item : in out Table_Type);

   procedure Scan_Fields
     (Table    : Table_Type;
      Process : not null access procedure
        (Field : Kit.Schema.Fields.Field_Type));

   function Contains_Field (Table : Table_Type;
                            Name     : String)
                           return Boolean;

   function Inherited_Field (Table : Table_Type;
                             Field : Kit.Schema.Fields.Field_Type)
                             return Boolean;

   function Contains_Base (Table : Table_Type;
                           Name  : String)
                          return Boolean;

   function Base_Index (Table : Table_Type;
                        Base  : Table_Type'Class)
                        return Positive;

   function Field_Start (Table : Table_Type;
                         Field : Kit.Schema.Fields.Field_Type)
                         return System.Storage_Elements.Storage_Offset;

   procedure Scan_Keys
     (Table : Table_Type;
      Process  : not null access
        procedure (Item : Kit.Schema.Keys.Key_Type'Class));

   procedure Scan_Keys
     (Table : Table_Type;
      Process          : not null access procedure
        (Base   : Table_Type'Class;
         Key    : Kit.Schema.Keys.Key_Type'Class);
      Include_Base_Keys : Boolean := True);

   procedure Scan_Keys
     (Table    : Table_Type;
      Containing_Field : Kit.Schema.Fields.Field_Type;
      Process          : not null access procedure
        (Table  : Table_Type'Class;
         Base   : Table_Type'Class;
         Key    : Kit.Schema.Keys.Key_Type'Class));

   procedure Iterate
     (Table : Table_Type;
      Process  : not null access
        procedure (Item : Kit.Schema.Fields.Field_Type));

   procedure Iterate_All
     (Table : Table_Type'Class;
      Process  : not null access
        procedure (Table : Table_Type'Class;
                   Field : Kit.Schema.Fields.Field_Type);
      Table_First : Boolean := False);

   function To_Storage (Table       : Table_Type'Class;
                        Base_Table  : Table_Type'Class;
                        Key_Table   : Table_Type'Class;
                        Object_Name : String;
                        Key         : Kit.Schema.Keys.Key_Type'Class;
                        With_Index  : Boolean)
                        return Aquarius.Drys.Expression'Class;

   function To_Storage (Table       : Table_Type'Class;
                        Base_Table  : Table_Type'Class;
                        Key_Table   : Table_Type'Class;
                        Object_Name : String;
                        Key         : Kit.Schema.Keys.Key_Type'Class;
                        New_Field   : Kit.Schema.Fields.Field_Type;
                        Field_Value : String;
                        With_Index  : Boolean)
                        return Aquarius.Drys.Expression'Class;

   function To_Storage (Key_Value_Name   : String;
                        Index_Value_Name : String;
                        Key         : Kit.Schema.Keys.Key_Type'Class)
                        return Aquarius.Drys.Expression'Class;

   procedure Iterate (Table     : Table_Type;
                      Process   : not null access
                        procedure (Item : Table_Type'Class);
                      Inclusive : Boolean;
                      Table_First : Boolean := False);

   procedure Append
     (Table     : in out Table_Type;
      Item      : in     Kit.Schema.Fields.Field_Type);

   procedure Add_Key
     (Table     : in out Table_Type;
      Key       : in     Kit.Schema.Keys.Key_Type'Class);

   procedure Add_Key_Field
     (Table      : in out Table_Type'Class;
      Key        : in out Kit.Schema.Keys.Key_Type'Class;
      Field_Name : in String);

   function Key
     (Table : Table_Type;
      Name  : String)
     return Kit.Schema.Keys.Key_Type'Class;

   procedure Add_Base
     (Table     : in out Table_Type;
      Item      : in     Table_Type'Class);

   procedure Add_Base_Keys
     (Table     : in out Table_Type);

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

   function Internal_Table_Name
     (Table : Table_Type'Class)
      return String;
   --  Returns a short name for use internally.
   --  Currently, this is Tn_Idx, where n is the table index,
   --  which ranges from 1 .. #tables
   --  It's "Idx" because it used to be used only for
   --  database index fields

   function Base_Index_Name
     (Table : Table_Type'Class)
      return String;

   function Base_Field_Name
     (Table  : Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type'Class;
      Field       : Kit.Schema.Fields.Field_Type)
      return String;

   function Index_Image
     (Table : Table_Type'Class)
      return String;

   function Key_Reference_Name
     (Table : Table_Type'Class;
      Key   : Kit.Schema.Keys.Key_Type'Class)
      return String;

   function Key_Reference_Name
     (Table    : Table_Type'Class;
      Key_Name : String)
      return String;

   function Key_To_Storage
     (Table       : Table_Type'Class;
      Key         : Kit.Schema.Keys.Key_Type'Class;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

private

   type Table_Field is
      record
         Start    : System.Storage_Elements.Storage_Offset;
         Length   : System.Storage_Elements.Storage_Count;
         Field    : Kit.Schema.Fields.Field_Type;
      end record;

   type Table_Field_Access is access Table_Field;

   function Same_Field (Left, Right : Table_Field_Access) return Boolean;

   package Field_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Field_Access,
                                 Same_Field);

   type Table_Key_Access is access all Kit.Schema.Keys.Key_Type'Class;

   function Same_Key (Left, Right : Table_Key_Access) return Boolean;

   package Key_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Key_Access,
                                 Same_Key);

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
         Header_Length          : System.Storage_Elements.Storage_Count := 4;
         Index                  : Marlowe.Table_Index;
         Bases                  : Table_Vectors.Vector;
         Base_Layout            : Base_Layout_Vectors.Vector;
         Fields                 : Field_Vectors.Vector;
         Keys                   : Key_Vectors.Vector;
         Magic                  : Natural;
         Has_String_Type        : Boolean := False;
         Has_Text_Type          : Boolean := False;
         Has_Key_Field          : Boolean := False;
         Has_Compound_Key_Field : Boolean := False;
         Has_Display_Field      : Boolean := False;
         With_Vector_Package    : Boolean := False;
         With_Map_Package       : Boolean := False;
      end record;

   function Find_Key
     (Table : Table_Type'Class;
      Property : not null access
        function (K : Kit.Schema.Keys.Key_Type'Class)
      return Boolean)
      return Kit.Schema.Keys.Key_Type'Class;

   function Base_Field
     (Table : Table_Type'Class;
      Base  : Table_Type'Class)
      return Kit.Schema.Fields.Field_Type;

end Kit.Schema.Tables;
