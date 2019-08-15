with WL.Command_Line;

package body Kit.SQL.Options is

   pragma Style_Checks (Off);

   function Database return String is
   begin
      return WL.Command_Line.Find_Option
               ("database", ' ');
   end Database;

   function SQL_Script return String is
   begin
      return WL.Command_Line.Find_Option
               ("sql-script", ' ');
   end SQL_Script;

end Kit.SQL.Options;
