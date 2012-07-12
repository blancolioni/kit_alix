with Input_Sources.File;
with Kit.Sax_Reader;

package body Kit.XML_Reader is

   -------------------
   -- Read_XML_File --
   -------------------

   procedure Read_XML_File
     (Path : String;
      Db   : in out Kit.Schema.Databases.Database_Type)
   is
      pragma Unreferenced (Db);
      Input : Input_Sources.File.File_Input;
      Kit_Reader : Kit.Sax_Reader.Reader;
   begin
      Input_Sources.File.Open (Path, Input);
      Kit_Reader.Parse (Input);
      Input_Sources.File.Close (Input);
   end Read_XML_File;

end Kit.XML_Reader;
