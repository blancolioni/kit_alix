with Aquarius.Drys.Projects;
with Kit.Schema.Databases;

package Kit.Generate is

   function Generate_Database (Db : in out Kit.Schema.Databases.Database_Type)
                              return Aquarius.Drys.Projects.Project;

end Kit.Generate;
