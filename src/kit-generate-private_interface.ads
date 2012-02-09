with Aquarius.Drys.Declarations;
with Kit.Databases;
with Kit.Tables;

package Kit.Generate.Private_Interface is

   function Generate_Private_Interface
     (Db    : in out Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
      Top   : in     Aquarius.Drys.Declarations.Package_Type'Class)
     return Aquarius.Drys.Declarations.Package_Type'Class;

end Kit.Generate.Private_Interface;
