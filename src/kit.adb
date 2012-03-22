with Ada.Unchecked_Deallocation;

package body Kit is

   -----------
   -- Close --
   -----------

   procedure Close (Rec : in out Database_Record) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Root_Database_Record'Class,
                                        Database_Record);
   begin
      Free (Rec);
   end Close;

   ---------
   -- Get --
   ---------

   function Get
     (Database     : Root_Database_Interface'Class;
      Table_Name   : String;
      Record_Index : Marlowe.Database_Index)
      return Database_Record
   is
   begin
      return Database.Get (Database.To_Table_Index (Table_Name),
                           Record_Index);
   end Get;

   -----------------------
   -- Scan_By_Key_Value --
   -----------------------

   function Scan_By_Key_Value
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Key_Value      : String)
      return Database_Record
   is
   begin
      return Tables.Scan_By_Key_Values
        (Table_Index    => Tables.To_Table_Index (Table_Name),
         Key_Name       => Table_Name & "_" & Key_Name,
         Low_Key_Value  => Key_Value,
         High_Key_Value => Key_Value);
   end Scan_By_Key_Value;

   ------------------------
   -- Scan_By_Key_Values --
   ------------------------

   function Scan_By_Key_Values
     (Tables         : Root_Database_Interface'Class;
      Table_Name     : String;
      Key_Name       : String;
      Low_Key_Value  : String;
      High_Key_Value : String)
      return Database_Record
   is
   begin
      return Tables.Scan_By_Key_Values
        (Table_Index    => Tables.To_Table_Index (Table_Name),
         Key_Name       => Table_Name & "_" & Key_Name,
         Low_Key_Value  => Low_Key_Value,
         High_Key_Value => High_Key_Value);
   end Scan_By_Key_Values;

end Kit;
