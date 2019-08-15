with Ada.Strings.Fixed;

package body Kit.SQL.Data_Tables is

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column
     (Table : in out Data_Table;
      Name  : String)
   is
   begin
      Table.Cols.Append (Data_Column'
                           (Name => +Name));
      for Row of Table.Rows loop
         Row.Values.Append ((others => <>));
      end loop;
   end Add_Column;

   -------------
   -- Add_Row --
   -------------

   function Add_Row
     (Table : in out Data_Table)
      return Row_Update_Cursor
   is
      Row : Data_Row;
   begin
      for I in 1 .. Table.Columns loop
         Row.Values.Append ((others => <>));
      end loop;
      Table.Rows.Append (Row);
      return (Table'Unchecked_Access, Table.Rows.Last_Index);
   end Add_Row;

   -----------
   -- Clear --
   -----------

   procedure Clear (Table : in out Data_Table) is
   begin
      Table.Cols.Clear;
      Table.Rows.Clear;
   end Clear;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Integer)
   is
   begin
      Set_Value (Row, Index,
                 Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Float)
   is
   begin
      Set_Value (Row, Index,
                 Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : String)
   is
      Update_Row : Data_Row renames Row.Table.Rows (Row.Index);
      Update_Cell : Data_Value renames Update_Row.Values (Index);
   begin
      Update_Cell.Image := +Value;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Row   : Row_Update_Cursor;
      Index : Column_Index;
      Value : Boolean)
   is
   begin
      Set_Value (Row, Index, (if Value then "True" else "False"));
   end Set_Value;

end Kit.SQL.Data_Tables;
