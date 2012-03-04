with Aquarius.Drys.Declarations;
with Kit.Tables;

package Kit.Generate.Public_Get is

   procedure Create_Get_Function
     (Db            : in     Kit.Databases.Database_Type;
      Table         : in     Kit.Tables.Table_Type'Class;
      Key_Table     : in     Kit.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      Scan          : in     Boolean;
      First         : in     Boolean;
      Key           : in     Kit.Tables.Key_Cursor;
      Key_Value     : in     Boolean);

   procedure Create_Generic_Get_Function
     (Db            : in     Kit.Databases.Database_Type;
      Table         : in     Kit.Tables.Table_Type'Class;
      Table_Package : in out Aquarius.Drys.Declarations.Package_Type'Class;
      First         : in     Boolean;
      Key_Value     : in     Boolean);

end Kit.Generate.Public_Get;
