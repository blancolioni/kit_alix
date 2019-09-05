private with Ada.Containers.Vectors;

with System.Storage_Elements;

with Syn;

with Marlowe;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

with Kit.Names;

package Kit.Schema.Tables is

   type Root_Table_Type is
     new Kit.Names.Root_Named_Object with private;

   type Table_Type is access all Root_Table_Type'Class;

   function Magic_Number
     (Item : Root_Table_Type)
      return Natural;

   function Reference_Index
     (Item : Root_Table_Type)
      return Marlowe.Table_Index;

   function Reference_Type
     (Item : Root_Table_Type)
      return Kit.Schema.Types.Kit_Type;

   function Package_Name
     (Item : Root_Table_Type)
      return String;

   function Interface_Name
     (Item : Root_Table_Type)
      return String;

   function Update_Interface_Name
     (Item : Root_Table_Type)
      return String;

   function Type_Name
     (Item : Root_Table_Type)
      return String;

   function Update_Type_Name
     (Item : Root_Table_Type)
      return String;

   function Implementation_Name
     (Item : Root_Table_Type)
      return String;

   function Update_Implementation_Name
     (Item : Root_Table_Type)
      return String;

   function Length
     (Item : Root_Table_Type)
      return System.Storage_Elements.Storage_Count;

   function Reference_Type_Name (Item : Root_Table_Type) return String;

   function References_Table (Item    : Root_Table_Type;
                              Other   : Table_Type)
                              return Boolean;

   function Implementation_Record_Type (Item : Root_Table_Type) return String;

   function Has_String_Type (Item : Root_Table_Type) return Boolean;
   function Has_Text_Type (Item : Root_Table_Type) return Boolean;
   function Has_Key_Field (Item : Root_Table_Type) return Boolean;
   function Has_Local_Key_Field (Item : Root_Table_Type) return Boolean;
   function Has_Compound_Key_Field (Item : Root_Table_Type) return Boolean;
   function Has_Display_Field
     (Item : not null access Root_Table_Type)
      return Boolean;
   function Has_Inherited_Table (Item : Root_Table_Type) return Boolean;

   function With_Vector_Package (Item : Root_Table_Type) return Boolean;
   function With_Map_Package (Item : Root_Table_Type) return Boolean;

   procedure Enable_Vector_Package
     (Item : in out Root_Table_Type);

   procedure Enable_Map_Package
     (Item : in out Root_Table_Type);

   procedure Scan_Fields
     (Table    : Root_Table_Type;
      Process : not null access procedure
        (Field : Kit.Schema.Fields.Field_Type));

   function Contains_Field (Table : Root_Table_Type;
                            Name     : String)
                           return Boolean;

   function Inherited_Field (Table : Root_Table_Type;
                             Field : Kit.Schema.Fields.Field_Type)
                             return Boolean;

   function Contains_Base (Table : Root_Table_Type;
                           Name  : String)
                          return Boolean;

   function Base_Index (Table : Root_Table_Type;
                        Base  : Table_Type)
                        return Positive;

   function Field_Start (Table : Root_Table_Type;
                         Field : Kit.Schema.Fields.Field_Type)
                         return System.Storage_Elements.Storage_Offset;

   procedure Scan_Keys
     (Table : Root_Table_Type;
      Process  : not null access
        procedure (Item : Kit.Schema.Keys.Key_Type));

   procedure Scan_Keys
     (Table : not null access Root_Table_Type'Class;
      Process          : not null access procedure
        (Base   : Table_Type;
         Key    : Kit.Schema.Keys.Key_Type);
      Include_Base_Keys : Boolean := True);

   procedure Scan_Keys
     (Table : not null access Root_Table_Type'Class;
      Containing_Field : Kit.Schema.Fields.Field_Type;
      Process          : not null access procedure
        (Table  : Table_Type;
         Base   : Table_Type;
         Key    : Kit.Schema.Keys.Key_Type));

   procedure Iterate
     (Table : Root_Table_Type;
      Process  : not null access
        procedure (Item : Kit.Schema.Fields.Field_Type));

   procedure Iterate_All
     (Table : not null access Root_Table_Type'Class;
      Process  : not null access
        procedure (Table : Table_Type;
                   Field : Kit.Schema.Fields.Field_Type);
      Table_First : Boolean := False);

   function To_Storage
     (Table       : Root_Table_Type;
      Base_Table  : Table_Type;
      Key_Table   : Table_Type;
      Object_Name : String;
      Key         : Kit.Schema.Keys.Key_Type;
      With_Index  : Boolean;
      Last_Index  : Natural := 0;
      Fill_Low    : Boolean := True)
      return Syn.Expression'Class;

   function To_Storage
     (Table       : Root_Table_Type;
      Base_Table  : Table_Type;
      Key_Table   : Table_Type;
      Object_Name : String;
      Key         : Kit.Schema.Keys.Key_Type;
      New_Field   : Kit.Schema.Fields.Field_Type;
      Field_Value : String;
      With_Index  : Boolean)
      return Syn.Expression'Class;

   function To_Storage
     (Key_Value_Name   : String;
      Index_Value_Name : String;
      Key              : Kit.Schema.Keys.Key_Type)
      return Syn.Expression'Class;

   procedure Iterate
     (Table     : not null access Root_Table_Type'Class;
      Process   : not null access
        procedure (Item : Table_Type);
      Inclusive : Boolean;
      Table_First : Boolean := False);

   procedure Append
     (Table     : in out Root_Table_Type;
      Item      : in     Kit.Schema.Fields.Field_Type);

   procedure Add_Key
     (Table     : in out Root_Table_Type;
      Key       : in     Kit.Schema.Keys.Key_Type);

   procedure Add_Key_Field
     (Table      : in out Root_Table_Type'Class;
      Key        : Kit.Schema.Keys.Key_Type;
      Field_Name : in String);

   function Key
     (Table : Root_Table_Type;
      Name  : String)
     return Kit.Schema.Keys.Key_Type;

   procedure Add_Base
     (Table     : in out Root_Table_Type;
      Item      : in     Table_Type);

   procedure Add_Base_Keys
     (Table     : in out Root_Table_Type);

   function Database_Index_Component
     (Table       : Root_Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type)
      return String;

   function Database_Index_Component
     (Table       : Root_Table_Type'Class;
      Object_Name : String;
      Base_1      : Table_Type;
      Base_2      : Table_Type)
      return String;

   function Base_Component_Name
     (Table : Root_Table_Type'Class)
      return String;

   function Internal_Table_Name
     (Table : Root_Table_Type'Class)
      return String;
   --  Returns a short name for use internally.
   --  Currently, this is Tn_Idx, where n is the table index,
   --  which ranges from 1 .. #tables
   --  It's "Idx" because it used to be used only for
   --  database index fields

   function Base_Index_Name
     (Table : Root_Table_Type'Class)
      return String;

   function Base_Field_Name
     (Table       : Root_Table_Type'Class;
      Object_Name : String;
      Base        : Table_Type;
      Field       : Kit.Schema.Fields.Field_Type)
      return String;

   function Index_Image
     (Table : Root_Table_Type'Class)
      return String;

   function Key_Reference_Name
     (Table : Root_Table_Type'Class;
      Key   : Kit.Schema.Keys.Key_Type)
      return String;

   function Key_Reference_Name
     (Table    : Root_Table_Type'Class;
      Key_Name : String)
      return String;

   function Key_To_Storage
     (Table       : Root_Table_Type'Class;
      Key         : Kit.Schema.Keys.Key_Type;
      Object_Name : String)
      return Syn.Expression'Class;

   function Create_Table (Name : String) return Table_Type;

private

   type Table_Field is
      record
         Start    : System.Storage_Elements.Storage_Offset;
         Length   : System.Storage_Elements.Storage_Count;
         Field    : Kit.Schema.Fields.Field_Type;
      end record;

   function Same_Field (Left, Right : Table_Field) return Boolean;

   package Field_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Field,
                                 Same_Field);

   package Key_Vectors is
     new Ada.Containers.Vectors
       (Positive, Kit.Schema.Keys.Key_Type, Kit.Schema.Keys."=");

   package Table_Vectors is
      new Ada.Containers.Vectors (Positive, Table_Type);

   type Base_Cursor is new Table_Vectors.Cursor;

   package Base_Layout_Vectors is
     new Ada.Containers.Vectors (Positive, Marlowe.Table_Index,
                                 Marlowe."=");

   type Root_Table_Type is
     new Kit.Names.Root_Named_Object with
      record
         Table_Reference_Type   : Kit.Schema.Types.Kit_Type;
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
         Has_Local_Key_Field    : Boolean := False;
         Has_Key_Field          : Boolean := False;
         Has_Compound_Key_Field : Boolean := False;
         Has_Display_Field      : Boolean := False;
         With_Vector_Package    : Boolean := False;
         With_Map_Package       : Boolean := False;
      end record;

   function Find_Key
     (Table : not null access constant Root_Table_Type'Class;
      Property : not null access
        function (K : Kit.Schema.Keys.Key_Type)
      return Boolean)
      return Kit.Schema.Keys.Key_Type;

   function Base_Field
     (Table : Root_Table_Type'Class;
      Base  : Table_Type)
      return Kit.Schema.Fields.Field_Type;

   function Has_Inherited_Table (Item : Root_Table_Type) return Boolean
   is (not Item.Bases.Is_Empty);

end Kit.Schema.Tables;
