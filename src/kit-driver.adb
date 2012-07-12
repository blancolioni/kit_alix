with Ada.Command_Line;
with Ada.Directories;

with Aquarius.Drys;
with Aquarius.Drys.File_Writer;
with Aquarius.Drys.Projects;

with Kit.Schema.Databases;
with Kit.Parser;
with Kit.XML_Reader;
with Kit.Generate;

with Kit.Schema.Types;

with GCS.Errors;

procedure Kit.Driver is
   Db : Kit.Schema.Databases.Database_Type;
begin

   Kit.Schema.Types.Create_Standard_Types;

   declare
      File_Name : constant String :=
                    Ada.Command_Line.Argument (1);
   begin
      if Ada.Directories.Extension (File_Name) = "xml" then
         Kit.XML_Reader.Read_XML_File (File_Name, Db);
      else
         Kit.Parser.Read_Kit_File (Ada.Command_Line.Argument (1), Db);

         if GCS.Errors.Has_Errors then
            Ada.Command_Line.Set_Exit_Status (1);
            return;
         end if;
      end if;
   end;

   declare
      Project : constant Aquarius.Drys.Projects.Project :=
                  Kit.Generate.Generate_Database (Db);
      File    : Aquarius.Drys.File_Writer.File_Writer;
   begin
      Aquarius.Drys.Projects.Write_Project (Project, File);
   end;

end Kit.Driver;
