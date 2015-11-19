with Kit.Schema.Databases;

package Kit.XML_Reader is

   function Read_XML_File
     (Path : String)
      return Kit.Schema.Databases.Database_Type;

end Kit.XML_Reader;
