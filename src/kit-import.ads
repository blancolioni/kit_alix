with Kit.Schema.Databases;

package Kit.Import is

   function Import_Directory
     (Directory_Path : String;
      Db             : Kit.Schema.Databases.Database_Access)
      return Natural;
   --  Returns the number of files read

end Kit.Import;
