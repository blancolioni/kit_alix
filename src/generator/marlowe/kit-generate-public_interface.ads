with Aquarius.Drys.Declarations;
with Kit.Schema.Databases;
with Kit.Schema.Tables;

package Kit.Generate.Public_Interface is

   function Generate_Public_Interface
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type;
      Top   : in     Aquarius.Drys.Declarations.Package_Type'Class)
     return Aquarius.Drys.Declarations.Package_Type'Class;

end Kit.Generate.Public_Interface;
