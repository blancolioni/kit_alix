with Aquarius.Drys.Declarations;
with Kit.Schema.Keys;
with Kit.Schema.Tables;

package Kit.Generate.Public_Get is

   procedure Create_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Schema.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Scan          : in     Boolean;
      First         : in     Boolean;
      Key_Name      : in     String;
      Key_Value     : in     Boolean;
      Bounds        : in     Boolean);

   --  Generate a function which gets a single (iterable) record from
   --  the given Table.  If Key is True, the key found in the Key_Value
   --  cursor is used, and Key_Table should be the table which originally
   --  declared the key.  If First is True, the iterator starts from the
   --  first result and continues forward; otherwise it starts from the
   --  last result and continues backwards.  If Bounds is true, a function
   --  with both upper and lower bounds is generated.


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
