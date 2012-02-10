with Ada.Command_Line;

with Aquarius.Drys;
with Aquarius.Drys.File_Writer;
with Aquarius.Drys.Projects;

with Kit.Databases;
with Kit.Parser;
with Kit.Generate;

procedure Kit.Driver is
   Db : Kit.Databases.Database_Type;
begin
   Kit.Parser.Read_Kit_File (Ada.Command_Line.Argument (1), Db);

   declare
      Project : constant Aquarius.Drys.Projects.Project :=
                  Kit.Generate.Generate_Database (Db);
      File    : Aquarius.Drys.File_Writer.File_Writer;
   begin
      Aquarius.Drys.Projects.Write_Project (Project, File);
   end;

end Kit.Driver;
