with Kit.Databases;

package Kit.Parser is

   procedure Read_Kit_File (Path : String;
                            Db   : out Kit.Databases.Database_Type);

end Kit.Parser;
