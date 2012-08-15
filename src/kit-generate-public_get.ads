with Aquarius.Drys.Declarations;
with Kit.Schema.Keys;
with Kit.Schema.Tables;

package Kit.Generate.Public_Get is

   procedure Create_Selection_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key_Name      : in     String;
      Key_Value     : in     Boolean;
      Bounds        : in     Boolean);

   --  Generate a function which gets a selection from the given Table.
   --  Key_Table should be the table which originally declared the key.
   --  If Bounds is true, a function with both upper and lower bounds is
   --  generated.  If Key_Value is True, the function returns a selection
   --  containing only records with a given value.  Otherwise, the selection
   --  contains the entire table in key order.

   procedure Create_Iterator
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class);

   --  Create the implementation of the iterator interface for selections.

   procedure Create_Reference_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class);

   --  Generate a function which returns a record corresponding to
   --  a particular table reference (which is internally converted
   --  to a database index).

   procedure Create_Get_From_Index
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class);

   --  Generate a procedure which fetches a record given its index

   procedure Create_Default_Key_Functions
     (Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Key           : in     Kit.Schema.Keys.Key_Type'Class);

   procedure Create_Generic_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      First         : in     Boolean;
      Key_Value     : in     Boolean);

end Kit.Generate.Public_Get;
