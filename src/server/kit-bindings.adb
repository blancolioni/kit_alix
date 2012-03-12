--  with Ada.Text_IO;

with Marlowe;

with Kit.Db.Kit_Root_Record;
with Kit.Db.Kit_Named_Item;

with Kit.Db.Leander_Interface;

with Kit.Server.Database;

package body Kit.Bindings is

   function Evaluate_First_Root_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Root_Record_Get_Top_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Named_Item_Get_Name
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Report_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Name_To_Table
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
      SK.Machine.Import_Function
        (Machine, "#namedItemGetName",
         1, Evaluate_Named_Item_Get_Name'Access);
      SK.Machine.Import_Function
        (Machine, "#reportRecord",
         2, Evaluate_Report_Record'Access);
      SK.Machine.Import_Function
        (Machine, "#nameToTable",
         1, Evaluate_Name_To_Table'Access);
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

   ----------------------------
   -- Evaluate_Name_To_Table --
   ----------------------------

   function Evaluate_Name_To_Table
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
   begin
      SK.Machine.Push (Context, Arguments (Arguments'First));
--        Ada.Text_IO.Put_Line ("begin nameToTable");
--        Ada.Text_IO.Put_Line (SK.Machine.Show_Stack_Top (Context));
--        Ada.Text_IO.Put_Line ("end nameToTable");
      return SK.To_Object (1);
   end Evaluate_Name_To_Table;

   ----------------------------------
   -- Evaluate_Named_Item_Get_Name --
   ----------------------------------

   function Evaluate_Named_Item_Get_Name
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Root : Kit.Db.Kit_Named_Item.Kit_Named_Item_Type :=
               Kit.Db.Kit_Named_Item.Get
                 (Kit.Db.Leander_Interface.To_Named_Item_Reference
                    (Arguments (Arguments'First)));
      Name : constant String := Root.Name;
   begin
      SK.Machine.Push (Context, SK.To_Object (1));
      for I in reverse Name'Range loop
         SK.Machine.Push (Context,
                          SK.To_Object (Character'Pos (Name (I))));
         SK.Machine.Push (Context, SK.To_Object (2));
         SK.Machine.Cons (Context);
         SK.Machine.Cons (Context);
      end loop;

      declare
         Result : SK.Object;
      begin
         --  Ada.Text_IO.Put_Line (SK.Machine.Show_Stack_Top (Context));
         SK.Machine.Pop (Context, Result);
         return Result;
      end;
   end Evaluate_Named_Item_Get_Name;

   ----------------------------
   -- Evaluate_Report_Record --
   ----------------------------

   function Evaluate_Report_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Table : constant Marlowe.Table_Index :=
                Marlowe.Table_Index
                  (SK.Get_Integer (Arguments (Arguments'First)));
      Index : constant Marlowe.Database_Index :=
                Marlowe.Database_Index
                  (SK.Get_Integer (Arguments (Arguments'First + 1)));
      Item  : Kit.Server.Database.Database_Record;
   begin
      Item.Get (Table, Index);
      Item.Report;
      return SK.To_Object (Integer (Table));
   end Evaluate_Report_Record;

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
