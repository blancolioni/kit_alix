with SK.Cells;

package {database}.Tables.SK_Tables is

   function Evaluate_Get_Table
     (Context    : SK.Cells.Managed_Cells;
      Table_Name : SK.Object)
      return SK.Object;

   function Evaluate_Table_Name
     (Context    : SK.Cells.Managed_Cells;
      Table_Index : SK.Object)
      return SK.Object;

   function Evaluate_Get_By
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object;

   function Evaluate_Get_Field
     (Context      : SK.Cells.Managed_Cells;
      Table_Index  : SK.Object;
      Record_Index : SK.Object;
      Field_Name   : SK.Object)
      return SK.Object;

   function Evaluate_Select_By
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object;

   function Evaluate_Trace_Indices
     (Context     : SK.Cells.Managed_Cells;
      Table_Index : SK.Object;
      Key_Name    : SK.Object;
      Key_Value   : SK.Object)
      return SK.Object;

end {database}.Tables.SK_Tables;
