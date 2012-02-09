with Aquarius.Drys.Declarations;

with Kit.Databases;
with Kit.Tables;

package Kit.Generate.Get_From_Cache is

   function Generate_Get_From_Cache
     (Db    : in out Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in     Aquarius.Drys.Declarations.Package_Type'Class)
     return Aquarius.Drys.Declarations.Package_Type'Class;

end Kit.Generate.Get_From_Cache;
