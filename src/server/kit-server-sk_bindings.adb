with Ada.Containers.Vectors;

with Kit.Server.Export;
with Kit.Server.Kit_XML;
with Kit.Server.Tables;

with Kit.Db.Kit_Record;

with Leander.Builtin;

with SK.Cells;
with SK.Functions;

package body Kit.Server.SK_Bindings is

   package Database_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Database_Record);

   Active_Records : Database_Record_Vectors.Vector;

   function Activate (Item : Database_Record)
                      return Positive;

   procedure Deactivate (Handle : Positive);

   function Evaluate_Table_Count
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Table_Name
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Get_Record
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Close_Record
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Export_Database_XML
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Get_Field_Count
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Get_Field_Name
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Get_Field_Value
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   --------------
   -- Activate --
   --------------

   function Activate (Item : Database_Record)
                      return Positive
   is
      Index : Positive := Active_Records.Last_Index + 1;
   begin
      for I in 1 .. Active_Records.Last_Index loop
         if Active_Records.Element (I) = null then
            Index := I;
            Active_Records.Replace_Element (Index, Item);
            exit;
         end if;
      end loop;

      if Index = Active_Records.Last_Index + 1 then
         Active_Records.Append (Item);
      end if;

      return Index;
   end Activate;

   ------------------------
   -- Create_SK_Bindings --
   ------------------------

   procedure Create_SK_Bindings is
      use SK.Functions;
   begin
      Bind_Function ("#tableCount", 0, Evaluate_Table_Count'Access);
      Bind_Function ("#tableName", 1, Evaluate_Table_Name'Access);
      Bind_Function ("#getRecord", 2, Evaluate_Get_Record'Access);
      Bind_Function ("#closeRecord", 1, Evaluate_Close_Record'Access);
      Bind_Function ("#exportxml", 1, Evaluate_Export_Database_XML'Access);
      Bind_Function ("#getFieldCount", 1, Evaluate_Get_Field_Count'Access);
      Bind_Function ("#getFieldName", 2, Evaluate_Get_Field_Name'Access);
      Bind_Function ("#getFieldValue", 2, Evaluate_Get_Field_Value'Access);
   end Create_SK_Bindings;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Handle : Positive) is
      It : Database_Record := Active_Records.Element (Handle);
   begin
      Close (It);
      Active_Records.Replace_Element (Handle, null);
   end Deactivate;

   ---------------------------
   -- Evaluate_Close_Record --
   ---------------------------

   function Evaluate_Close_Record
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Handle : constant Positive :=
                 SK.Get_Integer
                   (Arguments (Arguments'First));
   begin
      Deactivate (Handle);
      return SK.To_Object (Integer'(0));
   end Evaluate_Close_Record;

   ----------------------------------
   -- Evaluate_Export_Database_XML --
   ----------------------------------

   function Evaluate_Export_Database_XML
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Path : constant String :=
               Leander.Builtin.Object_To_String
                 (Context,
                  Arguments (Arguments'First));
      Exporter : Kit.Server.Export.Root_Exporter'Class :=
                   Kit.Server.Kit_XML.XML_Exporter
                     (Path);
   begin
      Kit.Server.Export.Export
        (Db       => Kit.Server.Tables.Active_Database.all,
         Exporter => Exporter);
      return SK.To_Object (Integer'(1));
   end Evaluate_Export_Database_XML;

   ------------------------------
   -- Evaluate_Get_Field_Count --
   ------------------------------

   function Evaluate_Get_Field_Count
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Handle : constant Positive :=
                 SK.Get_Integer (Arguments (Arguments'First));
      Result : constant Natural :=
                 Active_Records.Element (Handle).Field_Count;
   begin
      return SK.To_Object (Result);
   end Evaluate_Get_Field_Count;

   -----------------------------
   -- Evaluate_Get_Field_Name --
   -----------------------------

   function Evaluate_Get_Field_Name
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Handle : constant Positive :=
                 SK.Get_Integer (Arguments (Arguments'First));
      Index  : constant Positive :=
                 SK.Get_Integer (Arguments (Arguments'First + 1));
      Result     : constant String :=
                     Active_Records.Element (Handle).Field_Name (Index);
   begin
      return Leander.Builtin.String_To_Object
        (Context, Result);
   end Evaluate_Get_Field_Name;

   ------------------------------
   -- Evaluate_Get_Field_Value --
   ------------------------------

   function Evaluate_Get_Field_Value
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Handle : constant Positive :=
                 SK.Get_Integer (Arguments (Arguments'First));
      Field_Name : constant String :=
                     Leander.Builtin.Object_To_String
                       (Context,
                        Arguments (Arguments'First + 1));
      Result     : constant String :=
                     Active_Records.Element (Handle).Get (Field_Name);
   begin
      return Leander.Builtin.String_To_Object
        (Context, Result);
   end Evaluate_Get_Field_Value;

   -------------------------
   -- Evaluate_Get_Record --
   -------------------------

   function Evaluate_Get_Record
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Table_Index : constant Marlowe.Table_Index :=
                      Marlowe.Table_Index
                        (SK.Get_Integer
                           (Arguments (Arguments'First)));
      Record_Index : constant Marlowe.Database_Index :=
                       Marlowe.Database_Index
                         (SK.Get_Integer
                            (Arguments (Arguments'First + 1)));
      Result       : constant Database_Record :=
                       Kit.Server.Tables.Active_Database.Get
                         (Table_Index, Record_Index);
      Handle : constant Positive := Activate (Result);
   begin
      return SK.To_Object (Handle);
   end Evaluate_Get_Record;

   --------------------------
   -- Evaluate_Table_Count --
   --------------------------

   function Evaluate_Table_Count
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      return SK.To_Object
        (Natural (Kit.Server.Tables.Active_Database.Last_Table_Index));
   end Evaluate_Table_Count;

   -------------------------
   -- Evaluate_Table_Name --
   -------------------------

   function Evaluate_Table_Name
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Table_Index : constant Positive :=
                      SK.Get_Integer
                        (Arguments (Arguments'First));
      Rec         : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                      Kit.Db.Kit_Record.Get_By_Table_Index (Table_Index);
   begin
      return Leander.Builtin.String_To_Object (Context, Rec.Name);
   end Evaluate_Table_Name;

end Kit.Server.SK_Bindings;
