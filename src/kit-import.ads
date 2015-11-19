with Kit.Schema.Databases;

package Kit.Import is

   function Import_Directory
     (Directory_Path : String)
      return Kit.Schema.Databases.Database_Type;

end Kit.Import;
