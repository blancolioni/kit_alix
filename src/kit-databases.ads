private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Tables;

package Kit.Databases is

   type Database_Type is
     new Kit.Names.Root_Named_Object with private;

   type Table_Cursor is private;

   function First_Table (Database : Database_Type) return Table_Cursor;
   function Contains (Database : Database_Type;
                      Name     : String)
                     return Boolean;
   function Element (Database : Database_Type;
                     Name     : String)
                     return Kit.Tables.Table_Type'Class;

   procedure Next (Position : in out Table_Cursor);

   function Element (Position : Table_Cursor)
                    return Kit.Tables.Table_Type'Class;
   function Has_Element (Position : Table_Cursor)
                        return Boolean;

   procedure Iterate (Database : Database_Type;
                      Process  : not null access
                        procedure (Table : Kit.Tables.Table_Type'Class));

   procedure Append
     (Db   : in out Database_Type;
      Item : in     Kit.Tables.Table_Type'Class);

   procedure Create_Database (Db   : in out Database_Type;
                              Name : in     String);

private

   type Database_Table is access all Kit.Tables.Table_Type'Class;
   package Table_Vectors is
      new Ada.Containers.Vectors (Positive, Database_Table);

   type Database_Type is
     new Kit.Names.Root_Named_Object with
      record
         Tables : Table_Vectors.Vector;
      end record;

   type Table_Cursor is new Table_Vectors.Cursor;

end Kit.Databases;
