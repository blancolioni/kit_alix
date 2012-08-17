with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Aquarius.Drys;
with Aquarius.Drys.File_Writer;
with Aquarius.Drys.Projects;

with Kit.Schema.Databases;
with Kit.Parser;
with Kit.XML_Reader;
with Kit.Generate;
with Kit.Import;

with Kit.Schema.Types;

with GCS.Errors;

procedure Kit.Driver is
   Db : constant Kit.Schema.Databases.Database_Access :=
          new Kit.Schema.Databases.Database_Type;
begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("Usage: kit <file or directory>");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Kit.Schema.Types.Create_Standard_Types;


   declare
      use Ada.Directories;
      File_Name : constant String :=
                    Ada.Command_Line.Argument (1);
      Extension : constant String :=
                    Ada.Directories.Extension (File_Name);
   begin
      if Extension = "xml" then
         Kit.XML_Reader.Read_XML_File (File_Name, Db);
      elsif Extension = "kit" or else Extension = "k3" then
         Kit.Parser.Read_Kit_File (Ada.Command_Line.Argument (1), Db.all);

         if GCS.Errors.Has_Errors then
            Ada.Command_Line.Set_Exit_Status (1);
            return;
         end if;
      elsif Ada.Directories.Kind (File_Name) =
        Ada.Directories.Directory
      then
         declare
            Count : constant Natural :=
                      Kit.Import.Import_Directory (File_Name, Db);
         begin
            Ada.Text_IO.Put_Line ("Files imported:"
                                  & Natural'Image (Count));
         end;
      else
         Ada.Text_IO.Put_Line
           ("unknown file type: " & File_Name);
         Ada.Command_Line.Set_Exit_Status (2);
         return;
      end if;

   end;

   declare
      Project : constant Aquarius.Drys.Projects.Project :=
                  Kit.Generate.Generate_Database (Db.all);
      File    : Aquarius.Drys.File_Writer.File_Writer;
   begin
      Aquarius.Drys.Projects.Write_Project (Project, File);
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Skip_Line;
end Kit.Driver;
