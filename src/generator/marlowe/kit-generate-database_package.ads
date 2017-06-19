with Syn.Declarations;

package Kit.Generate.Database_Package is

   function Generate_Database_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type;

end Kit.Generate.Database_Package;
