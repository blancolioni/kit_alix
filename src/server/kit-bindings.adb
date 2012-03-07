with Kit.Db.Kit_Root_Record;
with Kit.Db.Leander_Interface;

package body Kit.Bindings is

   function Evaluate_First_Root_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Root_Record_Get_Top_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   -------------------------
   -- Create_Kit_Bindings --
   -------------------------

   procedure Create_Kit_Bindings
     (Machine : SK.Machine.SK_Machine)
   is
   begin
      SK.Machine.Import_Function
        (Machine, "#firstRootRecord",
         1, Evaluate_First_Root_Record'Access);
      SK.Machine.Import_Function
        (Machine, "#rootRecordGetTopRecord",
         1, Evaluate_Root_Record_Get_Top_Record'Access);
   end Create_Kit_Bindings;

   --------------------------------
   -- Evaluate_First_Root_Record --
   --------------------------------

   function Evaluate_First_Root_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
      Result : Kit.Db.Kit_Root_Record.Kit_Root_Record_Type :=
                 Kit.Db.Kit_Root_Record.First;
   begin
      return Kit.Db.Leander_Interface.To_SK_Object
        (Result.Reference);
   end Evaluate_First_Root_Record;

   -----------------------------------------
   -- Evaluate_Root_Record_Get_Top_Record --
   -----------------------------------------

   function Evaluate_Root_Record_Get_Top_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Root : Kit.Db.Kit_Root_Record.Kit_Root_Record_Type :=
               Kit.Db.Kit_Root_Record.Get
                 (Kit.Db.Leander_Interface.To_Root_Record_Reference
                             (Arguments (Arguments'First)));
   begin
      return SK.To_Object
        (Kit.Db.Record_Type'Pos (Root.Top_Record));
   end Evaluate_Root_Record_Get_Top_Record;

end Kit.Bindings;
