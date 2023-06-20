with Ada.Directories;
--  with Ada.Text_IO;

with Kit.XML_Reader;

package body Kit.Import is

   ----------------------
   -- Import_Directory --
   ----------------------

   function Import_Directory
     (Directory_Path : String)
      return Kit.Schema.Databases.Database_Type
   is

      Count : Natural := 0;
      Result : constant Kit.Schema.Databases.Database_Type :=
                 Kit.Schema.Databases.Create_Database
                   (Ada.Directories.Simple_Name (Directory_Path));

      procedure Call_Importer
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -----------------
      -- Call_Reader --
      -----------------

      procedure Call_Importer
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Child_Db : constant Kit.Schema.Databases.Database_Type :=
                      Kit.XML_Reader.Read_XML_File
                        (Full_Name (Directory_Entry));
      begin

         Result.With_Database (Child_Db);
         Count := Count + 1;

      end Call_Importer;

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Name : constant String := Simple_Name (Directory_Entry);
      begin
         if Name = "." or else Name = ".." then
            return;
         end if;

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*.xml",
            Filter         => [Ada.Directories.Ordinary_File => True,
                               others                        => False],
            Process        => Call_Importer'Access);

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*",
            Filter         => [Ada.Directories.Directory     => True,
                               others                        => False],
            Process        => Recurse'Access);
      end Recurse;

   begin
      Ada.Directories.Search
        (Directory      => Directory_Path,
         Pattern        => "*.xml",
         Filter         => [Ada.Directories.Ordinary_File => True,
                            others => False],
         Process        => Call_Importer'Access);

      Ada.Directories.Search
        (Directory      => Directory_Path,
         Pattern        => "*",
         Filter         => [Ada.Directories.Directory     => True,
                            others => False],
         Process        => Recurse'Access);

      return Result;

   end Import_Directory;

end Kit.Import;
