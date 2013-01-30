with Ada.Text_IO;

with Leander.Builtin;

package body {database}.Tables.SK_Tables is

   function Integer_To_Object (Cells : SK.Cells.Managed_Cells;
                               Value : Integer)
                               return SK.Object
   is (SK.To_Object (Value));

   function Object_To_Integer (Cells : SK.Cells.Managed_Cells;
                               Item  : SK.Object)
                               return Integer
   is (SK.Get_Integer (Item));

   type Array_Of_Integers is array (Positive range <>) of Integer;

   package Integer_Array_Conversions is
     new Leander.Builtin.Array_List_Conversions
       (Integer, Array_Of_Integers,
        Integer_To_Object,
        Object_To_Integer);

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
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
                Fields => String_Vectors.Empty_Vector);
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
            (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
                Fields => String_Vectors.Empty_Vector);
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

   ------------------------------
   -- Evaluate_Get_Float_Field --
   ------------------------------

   function Evaluate_Get_Float_Field
     (Context      : SK.Cells.Managed_Cells;
      Table_Index  : SK.Object;
      Record_Index : SK.Object;
      Field_Name   : SK.Object)
      return SK.Object
   is
      T : constant Database_Table :=
            (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
             Fields => String_Vectors.Empty_Vector);
      R : constant Database_Record :=
            T.Get (Record_Reference (SK.Get_Integer (Record_Index)));
      Field : constant String :=
                Leander.Builtin.Object_To_String
                  (Context, Field_Name);
      Result : constant String :=
                 R.Get (Field);
   begin
      return Leander.Builtin.Float_To_Object (Context, Float'Value (Result));
   end Evaluate_Get_Float_Field;

   --------------------------------
   -- Evaluate_Get_Integer_Field --
   --------------------------------

   function Evaluate_Get_Integer_Field
     (Context      : SK.Cells.Managed_Cells;
      Table_Index  : SK.Object;
      Record_Index : SK.Object;
      Field_Name   : SK.Object)
      return SK.Object
   is
      T : constant Database_Table :=
            (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
             Fields => String_Vectors.Empty_Vector);
      R : constant Database_Record :=
            T.Get (Record_Reference (SK.Get_Integer (Record_Index)));
      Field : constant String :=
                Leander.Builtin.Object_To_String
                  (Context, Field_Name);
      Result : constant String :=
                 R.Get (Field);
   begin
      return SK.To_Object (Integer'Value (Result));
   end Evaluate_Get_Integer_Field;

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

   ------------------------
   -- Evaluate_Select_By --
   ------------------------

   function Evaluate_Select_By
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object
   is
      T    : constant Database_Table :=
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
                Fields => String_Vectors.Empty_Vector);
      Key  : constant String :=
               Leander.Builtin.Object_To_String (Context, Key_Name);
      Value : constant String :=
                Leander.Builtin.Object_To_String (Context, Key_Value);
      Refs  : constant Array_Of_References :=
                T.Select_By (Key, Value);
      Int_Refs : Array_Of_Integers (Refs'Range);
   begin
      for I in Refs'Range loop
         Int_Refs (I) := Integer (Refs (I));
      end loop;
      return Integer_Array_Conversions.To_List (Context, Int_Refs);
   end Evaluate_Select_By;

   -------------------------
   -- Evaluate_Table_Name --
   -------------------------

   function Evaluate_Table_Name
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object)
      return SK.Object
   is
      T    : constant Database_Table :=
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
                Fields => String_Vectors.Empty_Vector);
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
               (Index => Marlowe.Table_Index (SK.Get_Integer (Table_Index)),
                Fields => String_Vectors.Empty_Vector);
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
