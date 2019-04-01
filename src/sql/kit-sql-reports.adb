with Ada.Characters.Handling;
with Ada.Text_IO;

package body Kit.SQL.Reports is

   ------------
   -- Report --
   ------------

   procedure Report
     (Table : Kit.SQL.Data_Tables.Data_Table'Class)
   is
      use Ada.Text_IO;
      Col_Width : array (1 .. Table.Columns) of Natural := (others => 0);
      Col_Start : array (1 .. Table.Columns) of Count := (others => 0);
      Last_Col  : Count;

      function Dashes (Index : Column_Index) return String;

      ------------
      -- Dashes --
      ------------

      function Dashes (Index : Column_Index) return String is
      begin
         return S : constant String (1 .. Col_Width (Index) + 2) :=
           (others => '-');
      end Dashes;

   begin

      if Table.Columns = 0 then
         return;
      end if;

      for Col_Index in 1 .. Table.Columns loop
         declare
            Heading : constant String := Table.Column (Col_Index).Name;
         begin
            Col_Width (Col_Index) := Heading'Length;
         end;
      end loop;

      for Index in 1 .. Table.Rows loop
         declare
            use Kit.SQL.Data_Tables;
            Row : constant Row_Cursor := Table.Row (Index);
         begin
            for Col_Index in 1 .. Table.Columns loop
               declare
                  Item : constant String :=
                           Value (Row, Col_Index);
               begin
                  if Item'Length > Col_Width (Col_Index) then
                     Col_Width (Col_Index) := Item'Length;
                  end if;
               end;
            end loop;
         end;
      end loop;

      for I in Col_Width'Range loop
         if I = 1 then
            Col_Start (I) := 1;
         else
            Col_Start (I) :=
              Col_Start (I - 1)
              + Count (Col_Width (I - 1)) + 3;
         end if;
      end loop;

      Last_Col := Col_Start (Col_Start'Last)
        + Count (Col_Width (Col_Width'Last))
        + 3;

      for I in Col_Width'Range loop
         Put ("+" & Dashes (I));
      end loop;
      Put_Line ("+");

      for I in Col_Start'Range loop
         Set_Col (Col_Start (I));
         Put ("| ");
         Put (Ada.Characters.Handling.To_Upper (Table.Column (I).Name));
      end loop;
      Set_Col (Last_Col);
      Put_Line ("|");

      for I in Col_Width'Range loop
         Put ("+" & Dashes (I));
      end loop;
      Put_Line ("+");

      for I in 1 .. Table.Rows loop
         for J in 1 .. Table.Columns loop
            Set_Col (Col_Start (J));
            Put ("| ");
            Put (Kit.SQL.Data_Tables.Value (Table.Row (I), J));
         end loop;
         Set_Col (Last_Col);
         Put_Line ("|");
      end loop;
      for I in Col_Width'Range loop
         Put ("+" & Dashes (I));
      end loop;
      Put_Line ("+");

   end Report;

end Kit.SQL.Reports;
