private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;


package {database}.Tables.Scanner is

   type Table_Scanner is private;

   function Scanning (Scanner : Table_Scanner) return Boolean;

   procedure Start_Scan
     (Scanner      : in out Table_Scanner;
      Table        : Database_Table'Class;
      Key_Name     : String);

   function More_Rows (Scanner : Table_Scanner) return Boolean;
   procedure Close (Scanner : in out Table_Scanner);

   type Table_Row is private;

   function Next_Row (Scanner : Table_Scanner) return Table_Row;

   function Cell_Count (Row : Table_Row) return Natural;
   function Cell_Value (Row : Table_Row;
                        Index : Positive)
                        return String;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Table_Row is
      record
         Cells : String_Vectors.Vector;
      end record;

   package Table_Row_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Table_Row);

   protected type Scan_Buffer is
      entry Next_Row (Row : out Table_Row);
      procedure Add_Row (Row : Table_Row);
      function Finished return Boolean;
      procedure Set_Finished;
   private
      Rows : Table_Row_List.List;
      Scan_Finished : Boolean := False;
   end Scan_Buffer;

   type Buffer_Access is access Scan_Buffer;

   task type Table_Scanner_Task is
      entry Start_Scan (Buffer   : Buffer_Access;
                        Table    : Database_Table'Class;
                        Key_Name : String);
   end Table_Scanner_Task;

   type Table_Scanner_Record is limited
      record
         Buffer  : Buffer_Access;
         Scanner : Table_Scanner_Task;
      end record;

   type Table_Scanner is access Table_Scanner_Record;

end {database}.Tables.Scanner;
