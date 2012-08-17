with XML.Parser;
with Kit.Import.XML_DB;

package body Kit.XML_Reader is

   -------------------
   -- Read_XML_File --
   -------------------

   procedure Read_XML_File
     (Path : String;
      Db   : Kit.Schema.Databases.Database_Access)
   is
      Reader : XML.XML_Document'Class :=
                 Kit.Import.XML_DB.XML_DB_Reader (Db);
   begin
      XML.Parser.Run_Parser (Reader, Path);
   end Read_XML_File;

end Kit.XML_Reader;
