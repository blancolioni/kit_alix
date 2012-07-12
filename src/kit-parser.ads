with Kit.Schema.Databases;

package Kit.Parser is

   procedure Read_Kit_File (Path : String;
                            Db   : out Kit.Schema.Databases.Database_Type);

end Kit.Parser;
