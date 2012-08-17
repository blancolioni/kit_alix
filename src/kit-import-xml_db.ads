with XML;
with Kit.Schema.Databases;

package Kit.Import.XML_DB is

   function XML_DB_Reader (Db : Kit.Schema.Databases.Database_Access)
                           return XML.XML_Document'Class;

--     type XML_DB_Reader is
--       new XML.XML_Document with private;

--  private
--
--     type XML_DB_Reader is
--       new XML.XML_Document with null record;

end Kit.Import.XML_DB;
