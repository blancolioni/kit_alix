with SK.Cells;
with SK.Functions;

with {database}.Tables.SK_Tables;

package body {database}.SK_Bindings is

   function Get_Table_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Table_Name_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Get_By_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Get_Field_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Trace_Indices_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   ------------------------
   -- Create_SK_Bindings --
   ------------------------

   procedure Create_SK_Bindings is
      use SK.Functions;
   begin
      Bind_Function ("#getBy", 3, Get_By_Binding'Access);
      Bind_Function ("#getField", 3, Get_Field_Binding'Access);
      Bind_Function ("#getTable", 1, Get_Table_Binding'Access);
      Bind_Function ("#tableName", 1, Table_Name_Binding'Access);
      Bind_Function ("#traceIndices", 3, Trace_Indices_Binding'Access);
   end Create_SK_Bindings;

   --------------------
   -- Get_By_Binding --
   --------------------

   function Get_By_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      return Tables.SK_Tables.Evaluate_Get_By
        (Context,
         SK.Cells.Evaluate (Context, Arguments (Arguments'First)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 1)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 2)));
   end Get_By_Binding;

   -----------------------
   -- Get_Field_Binding --
   -----------------------

   function Get_Field_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      return Tables.SK_Tables.Evaluate_Get_Field
        (Context,
         SK.Cells.Evaluate (Context, Arguments (Arguments'First)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 1)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 2)));
   end Get_Field_Binding;

   -----------------------
   -- Get_Table_Binding --
   -----------------------

   function Get_Table_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      return Tables.SK_Tables.Evaluate_Get_Table
        (Context,
         SK.Cells.Evaluate (Context, Arguments (Arguments'First)));
   end Get_Table_Binding;

   ------------------------
   -- Table_Name_Binding --
   ------------------------

   function Table_Name_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      return Tables.SK_Tables.Evaluate_Table_Name
        (Context,
         SK.Cells.Evaluate (Context, Arguments (Arguments'First)));
   end Table_Name_Binding;

   function Trace_Indices_Binding
     (Context   : SK.Cells.Managed_Cells;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      return Tables.SK_Tables.Evaluate_Trace_Indices
        (Context,
         SK.Cells.Evaluate (Context, Arguments (Arguments'First)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 1)),
         SK.Cells.Evaluate (Context, Arguments (Arguments'First + 2)));
   end Trace_Indices_Binding;

end {database}.SK_Bindings;
