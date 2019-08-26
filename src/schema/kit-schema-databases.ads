private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Schema.Properties;
with Kit.Schema.Tables;

package Kit.Schema.Databases is

   type Root_Database_Type is
     new Kit.Names.Root_Named_Object
     and Kit.Schema.Properties.Has_Properties_Interface
   with private;

   type Table_Cursor is private;

   function Database_Package_Name (Db : Root_Database_Type) return String;
   function Handle_Package_Name (Db : Root_Database_Type) return String;

   function Table_Count (Db : Root_Database_Type) return Natural;

   function First_Table (Database : Root_Database_Type) return Table_Cursor;
   function Contains (Database : Root_Database_Type;
                      Name     : String)
                     return Boolean;
   function Element (Database : Root_Database_Type;
                     Name     : String)
                     return Kit.Schema.Tables.Table_Type;
   function Element (Database : Root_Database_Type;
                     Index    : Positive)
                     return Kit.Schema.Tables.Table_Type;

   procedure Next (Position : in out Table_Cursor);

   function Element (Position : Table_Cursor)
                    return Kit.Schema.Tables.Table_Type;
   function Has_Element (Position : Table_Cursor)
                        return Boolean;

   procedure Iterate (Database : Root_Database_Type;
                      Process  : not null access
                        procedure
                          (Table : Kit.Schema.Tables.Table_Type));

   procedure Append
     (Db   : in out Root_Database_Type;
      Item : in     Kit.Schema.Tables.Table_Type);

   function Has_Display_Field (Db : Root_Database_Type) return Boolean;

   type Database_Type is access all Root_Database_Type'Class;

   function Create_Database
     (Name : in     String)
      return Database_Type;

   procedure With_Database
     (Db     : in out Root_Database_Type'Class;
      Withed : in     Database_Type);

private

   Database_Package_Name_Property : constant String :=
     "database_package_name";
   Handle_Package_Name_Property   : constant String :=
     "handle_package_name";

   package Table_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Kit.Schema.Tables.Table_Type,
        "="          => Kit.Schema.Tables."=");

   type Root_Database_Type is
     new Kit.Names.Root_Named_Object
       and Kit.Schema.Properties.Has_Properties_Interface with
      record
         Tables     : Table_Vectors.Vector;
         Properties : Kit.Schema.Properties.Properties_Map;
      end record;

   overriding function Has_Property
     (Database : Root_Database_Type;
      Name     : String)
      return Boolean
   is (Database.Properties.Has_Property (Name));

   overriding function Get_Property
     (Database   : Root_Database_Type;
      Name       : String)
      return Kit.Schema.Properties.Kit_Property_Value
   is (Database.Properties.Get_Property (Name));

   overriding procedure Set_Property
     (Database   : in out Root_Database_Type;
      Name       : String;
      Value      : Kit.Schema.Properties.Kit_Property_Value);

   function Database_Package_Name (Db : Root_Database_Type) return String
   is (Kit.Schema.Properties.To_String
       (Db.Properties.Get_Property (Database_Package_Name_Property)));

   function Handle_Package_Name (Db : Root_Database_Type) return String
   is (Kit.Schema.Properties.To_String
       (Db.Properties.Get_Property (Handle_Package_Name_Property)));

   type Table_Cursor is new Table_Vectors.Cursor;

end Kit.Schema.Databases;
