with Kit.Schema.Databases;

package Kit.XML_Reader is

   procedure Read_XML_File
     (Path : String;
      Db   : Kit.Schema.Databases.Database_Access);

end Kit.XML_Reader;
