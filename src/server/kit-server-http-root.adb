with Ada.Containers.Indefinite_Vectors;

with Kit.Paths;
with Kit.Templates;

with Kit.Db.Kit_Record;

package body Kit.Server.Http.Root is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Table_Names : String_Vectors.Vector;

   function Get_Table_Name (Index : Positive) return String
   is ("<td><a href="""
       & Table_Names (Index) & """>"
       & Table_Names (Index) & "</td>");

   ---------------
   -- Root_Page --
   ---------------

   function Root_Page return String is
      Sub : Kit.Templates.Substitutions;
   begin

      for Table of Kit.Db.Kit_Record.Select_By_Name loop
         declare
            Name : constant String := Table.Name;
         begin
            if Name'Length < 4 or else Name (1 .. 4) /= "kit_" then
               Table_Names.Append (Name);
            end if;
         end;
      end loop;

      Kit.Templates.Add_Substitution
        (Sub, "title", "Database");
      Kit.Templates.Add_Substitution
        (Sub, "row", Table_Names.Last_Index, Get_Table_Name'Access);

      Kit.Templates.Copy_File
        (Source => Kit.Paths.Config_File ("html/root.html"),
         Target => "root.html",
         Map    => Sub);
      return "root.html";
   end Root_Page;

end Kit.Server.Http.Root;
