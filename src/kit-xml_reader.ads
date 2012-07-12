with Kit.Schema.Databases;

package Kit.XML_Reader is

   procedure Read_XML_File
     (Path : String;
      Db   : in out Kit.Schema.Databases.Database_Type);

end Kit.XML_Reader;
