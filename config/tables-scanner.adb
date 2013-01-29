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
         return Scan_Finished;
      end Finished;

      --------------
      -- Next_Row --
      --------------

      entry Next_Row (Row : out Table_Row) when not Rows.Is_Empty is
      begin
         Row := Rows.First_Element;
         Rows.Delete_First;
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

      procedure Add_Row (Item : Database_Record'Class);

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row (Item : Database_Record'Class) is
         Row : Table_Row;
      begin
         for I in 1 .. Item.Field_Count loop
            Row.Cells.Append (Item.Get (I));
         end loop;
         Buf.Add_Row (Row);
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

      Ada.Text_IO.Put_Line ("Start scan");
      Scan_Table.Iterate (Ada.Strings.Unbounded.To_String (Scan_Key),
                          Add_Row'Access);
      Ada.Text_IO.Put_Line ("Finished scanning");
      Buf.Set_Finished;
   end Table_Scanner_Task;

end {database}.Tables.Scanner;
