with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Templates_Parser;

with Kit.Paths;

with Kit.Db.Tables;

package body Kit.Server.Http.Table_Page is

   ---------------
   -- Root_Page --
   ---------------

   function Table_Page (Name : String) return String is
      Table : constant Kit.Db.Tables.Database_Table :=
                Kit.Db.Tables.Get_Table (Name);

      function Format_Heading
        (Internal_Name : String)
         return String;

      --------------------
      -- Format_Heading --
      --------------------

      function Format_Heading (Internal_Name : String) return String is
         use Ada.Characters.Handling;
         Result : String := Internal_Name;
         First  : Boolean := True;
      begin
         for I in Result'Range loop
            if First then
               Result (I) := To_Upper (Result (I));
               First := False;
            elsif Result (I) = ' ' or else Result (I) = '_' then
               Result (I) := ' ';
               First := True;
            end if;
         end loop;
         if Table.Is_Key (Internal_Name) then
            return Result & "*";
         else
            return Result;
         end if;
      end Format_Heading;

      Scan_Time    : Duration;
      Field_Names  : Templates_Parser.Vector_Tag;
      Values       : Templates_Parser.Matrix_Tag;
      Count        : Natural := 0;

   begin

      for I in 0 .. Table.Field_Count loop

         if I = 0 then
            Templates_Parser.Append (Field_Names, "Index");
         else
            Templates_Parser.Append
              (Field_Names, Format_Heading (Table.Field_Name (I)));
         end if;
      end loop;

      declare
         use type Ada.Calendar.Time;
         Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

         procedure Add_Row (Item : Kit.Db.Tables.Database_Record'Class);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row (Item : Kit.Db.Tables.Database_Record'Class) is
            Row : Templates_Parser.Vector_Tag;
         begin
            Count := Count + 1;
            Templates_Parser.Append
              (Row,
               (Ada.Strings.Fixed.Trim
                    (Natural'Image (Count),
                     Ada.Strings.Left)));

            for I in 1 .. Item.Field_Count loop
               Templates_Parser.Append
                 (Row, Item.Get (I));
            end loop;

            Templates_Parser.Append (Values, Row);
         end Add_Row;

      begin
         Table.Iterate ("top_record", Add_Row'Access);
         Scan_Time := Ada.Calendar.Clock - Start_Time;
      end;

      declare
         Translations : constant Templates_Parser.Translate_Table :=
                       (Templates_Parser.Assoc ("TITLE", Name),
                        Templates_Parser.Assoc ("FIELDS", Field_Names),
                        Templates_Parser.Assoc ("VALUES", Values),
                        Templates_Parser.Assoc
                          ("TIME", Natural (Scan_Time * 1000.0)),
                        Templates_Parser.Assoc ("COUNT", Count));
      begin
         return Templates_Parser.Parse
           (Kit.Paths.Config_File ("templates/table.tmplt"), Translations);
      end;

   end Table_Page;

end Kit.Server.Http.Table_Page;
