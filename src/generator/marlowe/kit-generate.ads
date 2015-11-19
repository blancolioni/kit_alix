with Aquarius.Drys.Projects;
with Kit.Schema.Databases;

package Kit.Generate is

   type Generated_Database_Type is
     (Btree_Marlowe,
      Memory_Marlowe);

   function Generated_Database return Generated_Database_Type;

   function Data_Store_Package_Name return String;
   function Data_Store_Type_Name return String;
   function Data_Store_Cursor_Name return String;

   function Generate_Database
     (Db : Kit.Schema.Databases.Database_Type)
      return Aquarius.Drys.Projects.Project;

end Kit.Generate;
