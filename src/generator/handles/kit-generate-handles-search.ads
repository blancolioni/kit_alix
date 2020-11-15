with Kit.Schema.Keys;

package Kit.Generate.Handles.Search is

   procedure Create_Selection_Type
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type;
      Key_Table     : in     Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : in     String;
      Key_Value     : in     Boolean;
      Bounds        : in     Boolean;
      Bounded_Index : in     Natural   := 0);

   procedure Create_Unique_Get_Function
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type;
      Key_Table     : in     Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : in     String);

   procedure Create_First_Last_Functions
     (Db            : in     Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type;
      Key_Table     : in     Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : in     String);

   procedure Create_Default_Key_Functions
     (Db         : Kit.Schema.Databases.Database_Type;
      Table         : in     Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key           : in     Kit.Schema.Keys.Key_Type);

end Kit.Generate.Handles.Search;
