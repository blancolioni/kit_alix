with Aquarius.Drys.Projects;
with Kit.Databases;

package Kit.Generate is

   function Generate_Database (Db : in out Kit.Databases.Database_Type)
                              return Aquarius.Drys.Projects.Project;

end Kit.Generate;
