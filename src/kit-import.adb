with Ada.Directories;
--  with Ada.Text_IO;

with Kit.XML_Reader;

package body Kit.Import is

   ----------------------
   -- Import_Directory --
   ----------------------

   function Import_Directory
     (Directory_Path : String;
      Db             : Kit.Schema.Databases.Database_Access)
      return Natural
   is

      Result : Natural := 0;

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
      begin
--           Ada.Text_IO.Put (Simple_Name (Directory_Entry));
--           Ada.Text_IO.Flush;

         Kit.XML_Reader.Read_XML_File (Full_Name (Directory_Entry), Db);

--           Ada.Text_IO.New_Line;

         Result := Result + 1;

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
            Filter         => (Ada.Directories.Ordinary_File => True,
                               others                        => False),
            Process        => Call_Importer'Access);

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*",
            Filter         => (Ada.Directories.Directory     => True,
                               others                        => False),
            Process        => Recurse'Access);
      end Recurse;

   begin
      Ada.Directories.Search
        (Directory      => Directory_Path,
         Pattern        => "*.xml",
         Filter         => (Ada.Directories.Ordinary_File => True,
                            others => False),
         Process        => Call_Importer'Access);

      Ada.Directories.Search
        (Directory      => Directory_Path,
         Pattern        => "*",
         Filter         => (Ada.Directories.Directory     => True,
                            others => False),
         Process        => Recurse'Access);

      return Result;

   end Import_Directory;

end Kit.Import;
