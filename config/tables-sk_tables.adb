with Ada.Text_IO;

with Leander.Builtin;

package body {database}.Tables.SK_Tables is

   ---------------------
   -- Evaluate_Get_By --
   ---------------------

   function Evaluate_Get_By
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object
   is
      T    : constant Database_Table :=
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)));
      Key  : constant String :=
               Leander.Builtin.Object_To_String (Context, Key_Name);
      Value : constant String :=
                Leander.Builtin.Object_To_String (Context, Key_Value);
      Rec   : constant Database_Record'Class :=
                T.Get (Key, Value);
   begin
      return SK.To_Object (Integer (Rec.Index));
   end Evaluate_Get_By;

   ------------------------
   -- Evaluate_Get_Field --
   ------------------------

   function Evaluate_Get_Field
     (Context      : SK.Cells.Managed_Cells;
      Table_Index  : SK.Object;
      Record_Index : SK.Object;
      Field_Name   : SK.Object)
      return SK.Object
   is
      T : constant Database_Table :=
            (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)));
      R : constant Database_Record :=
            T.Get (Record_Reference (SK.Get_Integer (Record_Index)));
      Field : constant String :=
                Leander.Builtin.Object_To_String
                  (Context, Field_Name);
      Result : constant String :=
                 R.Get (Field);
   begin
      return Leander.Builtin.String_To_Object (Context, Result);
   end Evaluate_Get_Field;

   ------------------------
   -- Evaluate_Get_Table --
   ------------------------

   function Evaluate_Get_Table
     (Context    : SK.Cells.Managed_Cells;
      Table_Name : SK.Object)
      return SK.Object
   is
      Name : constant String :=
               Leander.Builtin.Object_To_String (Context, Table_Name);
      T : constant Database_Table :=
            Get_Table (Name);
   begin
      return SK.To_Object (Integer (T.Index));
   end Evaluate_Get_Table;

   -------------------------
   -- Evaluate_Table_Name --
   -------------------------

   function Evaluate_Table_Name
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object)
      return SK.Object
   is
      T    : constant Database_Table :=
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)));
      Name : constant String := T.Name;
   begin
      return Leander.Builtin.String_To_Object (Context, Name);
   end Evaluate_Table_Name;

   ----------------------------
   -- Evaluate_Trace_Indices --
   ----------------------------

   function Evaluate_Trace_Indices
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object
   is
      T    : constant Database_Table :=
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)));
      Key  : constant String :=
               Leander.Builtin.Object_To_String (Context, Key_Name);
      Value : constant String :=
                Leander.Builtin.Object_To_String (Context, Key_Value);
      Count : Natural := 0;

      procedure Process (Rec : Database_Record'Class);

      -------------
      -- Process --
      -------------

      procedure Process (Rec : Database_Record'Class) is
      begin
         Ada.Text_IO.Put_Line (Marlowe.Database_Index'Image (Rec.Index));
         Count := Count + 1;
      end Process;

   begin
      Iterate (T, Key, Value, Process'Access);
      return SK.To_Object (Count);
   end Evaluate_Trace_Indices;

end {database}.Tables.SK_Tables;
