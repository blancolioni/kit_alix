with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;

package body {database}.Tables.Scanner is

   ----------------
   -- Cell_Count --
   ----------------

   function Cell_Count (Row : Table_Row) return Natural is
   begin
      return Row.Cells.Last_Index;
   end Cell_Count;

   ----------------
   -- Cell_Value --
   ----------------

   function Cell_Value
     (Row : Table_Row;
      Index : Positive)
      return String
   is
   begin
      return Row.Cells.Element (Index);
   end Cell_Value;

   -----------
   -- Close --
   -----------

   procedure Close (Scanner : in out Table_Scanner) is
      procedure Free_Buffer is
        new Ada.Unchecked_Deallocation
          (Scan_Buffer, Buffer_Access);
      procedure Free_Scanner is
        new Ada.Unchecked_Deallocation
          (Table_Scanner_Record, Table_Scanner);
   begin
      Free_Buffer (Scanner.Buffer);
      Free_Scanner (Scanner);
      Scanner := null;
   end Close;

   ---------------
   -- More_Rows --
   ---------------

   function More_Rows (Scanner : Table_Scanner) return Boolean is
   begin
      return not Scanner.Buffer.Finished;
   end More_Rows;

   --------------
   -- Next_Row --
   --------------

   function Next_Row (Scanner : Table_Scanner) return Table_Row is
      Result : Table_Row;
   begin
      Scanner.Buffer.Next_Row (Result);
      return Result;
   end Next_Row;

   --------------
   -- Scanning --
   --------------

   function Scanning (Scanner : Table_Scanner) return Boolean is
   begin
      return Scanner /= null;
   end Scanning;

   ----------------
   -- Start_Scan --
   ----------------

   procedure Start_Scan
     (Scanner      : in out Table_Scanner;
      Table        : Database_Table'Class;
      Key_Name     : String)
   is
   begin
      Scanner := new Table_Scanner_Record;
      Scanner.Buffer := new Scan_Buffer;
      Scanner.Scanner.Start_Scan
        (Scanner.Buffer,
         Table, Key_Name);
   end Start_Scan;

   protected body Scan_Buffer is

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row (Row : Table_Row) is
      begin
         Rows.Append (Row);
      end Add_Row;

      --------------
      -- Finished --
      --------------

      function Finished return Boolean is
      begin
         return Scan_Finished and then Rows.Is_Empty;
      end Finished;

      --------------
      -- Next_Row --
      --------------

      entry Next_Row (Row : out Table_Row)
        when Scan_Finished or else not Rows.Is_Empty
      is
      begin
         if Rows.Is_Empty then
            Row := (Cells => String_Vectors.Empty_Vector);
         else
            Row := Rows.First_Element;
            Rows.Delete_First;
         end if;
      end Next_Row;


      ------------------
      -- Set_Finished --
      ------------------

      procedure Set_Finished is
      begin
         Scan_Finished := True;
      end Set_Finished;

   end Scan_Buffer;

   ------------------------
   -- Table_Scanner_Task --
   ------------------------

   task body Table_Scanner_Task is
      Buf : Buffer_Access;
      Scan_Table : Database_Table;
      Scan_Key   : Ada.Strings.Unbounded.Unbounded_String;
      Count      : Natural;

      procedure Add_Row (Item : Database_Record'Class);

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row (Item : Database_Record'Class) is
         Row : Table_Row;
      begin
         Row.Cells.Append
           (Ada.Strings.Fixed.Trim
              (Marlowe.Database_Index'Image (Item.Index),
               Ada.Strings.Left));
         for I in 1 .. Item.Field_Count loop
            Row.Cells.Append (Item.Get (I));
         end loop;
         Buf.Add_Row (Row);
         Count := Count + 1;
      end Add_Row;

   begin
      accept Start_Scan (Buffer   : Buffer_Access;
                         Table    : Database_Table'Class;
                         Key_Name : String)
      do
         Buf := Buffer;
         Scan_Table := Database_Table (Table);
         Scan_Key   := Ada.Strings.Unbounded.To_Unbounded_String (Key_Name);
      end Start_Scan;

      declare
         use type Ada.Calendar.Time;
         Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Ada.Text_IO.Put_Line ("Start scan");
         Count := 0;
         Scan_Table.Iterate (Ada.Strings.Unbounded.To_String (Scan_Key),
                             Add_Row'Access);
         Ada.Text_IO.Put_Line ("Finished scanning"
                               & Count'Img & " records"
                               & " in"
                               & Natural'Image
                                 (Natural (Ada.Calendar.Clock - Start_Time))
                               & " seconds");
      end;
      Buf.Set_Finished;
   end Table_Scanner_Task;

end {database}.Tables.Scanner;
