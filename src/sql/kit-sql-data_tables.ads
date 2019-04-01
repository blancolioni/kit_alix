private with Ada.Containers.Vectors;

package Kit.SQL.Data_Tables is

   type Data_Column is tagged private;

   function Name (Column : Data_Column'Class) return String;

   type Data_Row is tagged private;

   type Row_Cursor is private;

   function Length (Row : Data_Row'Class) return Column_Count;

   function Value
     (Row   : Row_Cursor;
      Index : Column_Index)
      return String;

   type Row_Update_Cursor is private;

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Integer);

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Float);

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : String);

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Boolean);

   type Data_Table is tagged private;

   function Columns (Table : Data_Table) return Column_Count;
   function Rows (Table : Data_Table) return Row_Count;

   function Column
     (Table : Data_Table;
      Index : Column_Index)
      return Data_Column'Class;

   function Row (Table : Data_Table;
                 Index : Row_Index)
                 return Row_Cursor
     with Pre => Index <= Table.Rows;

   function Row (Table : in out Data_Table;
                 Index : Row_Index)
                 return Row_Update_Cursor
     with Pre => Index <= Table.Rows;

   procedure Clear (Table : in out Data_Table);

   procedure Add_Column
     (Table : in out Data_Table;
      Name  : String);

   function Add_Row
     (Table : in out Data_Table)
      return Row_Update_Cursor;

private

   type Data_Column is tagged
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Name (Column : Data_Column'Class) return String
   is (-Column.Name);

   package Data_Column_Vectors is
     new Ada.Containers.Vectors (Column_Index, Data_Column);

   type Data_Value is
      record
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Data_Value_Vectors is
     new Ada.Containers.Vectors (Column_Index, Data_Value);

   type Data_Row is tagged
      record
         Values : Data_Value_Vectors.Vector;
      end record;

   function Length (Row : Data_Row'Class) return Column_Count
   is (Row.Values.Last_Index);

   package Data_Row_Vectors is
     new Ada.Containers.Vectors (Row_Index, Data_Row);

   type Row_Cursor is
      record
         Table : access constant Data_Table'Class;
         Index : Row_Index;
      end record;

   type Data_Table is tagged
      record
         Cols : Data_Column_Vectors.Vector;
         Rows : Data_Row_Vectors.Vector;
      end record;

   type Row_Update_Cursor is
      record
         Table : access Data_Table'Class;
         Index : Row_Index;
      end record;

   function Columns (Table : Data_Table) return Column_Count
   is (Table.Cols.Last_Index);

   function Rows (Table : Data_Table) return Row_Count
   is (Table.Rows.Last_Index);

   function Column
     (Table : Data_Table;
      Index : Column_Index)
      return Data_Column'Class
   is (Table.Cols.Element (Index));

   function Row (Table : Data_Table;
                 Index : Row_Index)
                 return Row_Cursor
   is ((Table'Unchecked_Access, Index));

   function Row (Table : in out Data_Table;
                 Index : Row_Index)
                 return Row_Update_Cursor
   is ((Table'Unchecked_Access, Index));

   function Value
     (Row   : Row_Cursor;
      Index : Column_Index)
      return String
   is (Ada.Strings.Unbounded.To_String
       (Row.Table.Rows.Element (Row.Index)
        .Values.Element (Index).Image));

end Kit.SQL.Data_Tables;
