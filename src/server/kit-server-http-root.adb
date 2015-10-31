with Templates_Parser;

with Kit.Paths;

with Kit.Db.Kit_Record;

package body Kit.Server.Http.Root is

   ---------------
   -- Root_Page --
   ---------------

   function Root_Page return String is
      Table_Names : Templates_Parser.Vector_Tag;
   begin

      for Table of Kit.Db.Kit_Record.Select_By_Name loop
         declare
            Name : constant String := Table.Name;
         begin
            if Name'Length < 4 or else Name (1 .. 4) /= "kit_" then
               Templates_Parser.Append (Table_Names, Name);
            end if;
         end;
      end loop;

      declare
         Translations : constant Templates_Parser.Translate_Table :=
                       (1 => Templates_Parser.Assoc ("TITLE", "Tables"),
                        2 => Templates_Parser.Assoc ("TABLES", Table_Names));
      begin
         return Templates_Parser.Parse
           (Kit.Paths.Config_File ("templates/root.tmplt"), Translations);
      end;

   end Root_Page;

end Kit.Server.Http.Root;
