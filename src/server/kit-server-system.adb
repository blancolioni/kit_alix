with Kit.Db.Kit_Record;

with Kit.Server.Commands;              use Kit.Server.Commands;
with Kit.Server.Database;

package body Kit.Server.System is

   procedure Handle_Report
     (Arguments   : in     Command_Arguments;
      Response    : in out Command_Response);

   -------------------------
   -- Add_System_Commands --
   -------------------------

   procedure Add_System_Commands is
   begin
      Register ("report", (1 => Table_Name), Handle_Report'Access);
      Register ("rpr", (1 => Table_Name), Handle_Report'Access);
   end Add_System_Commands;

   -------------------
   -- Handle_Report --
   -------------------

   procedure Handle_Report
     (Arguments   : in     Command_Arguments;
      Response    : in out Command_Response)
   is
      use Kit.Db.Kit_Record;
      Table_Name : constant String :=
                     Argument (Arguments, 1);
      Table      : constant Kit_Record_Type :=
                     First_By_Name (Argument (Arguments, 1));
   begin
      if Table.Has_Element then
         Kit.Server.Database.Report
           (Marlowe.Table_Index (Table.Table_Index));
      else
         Set_Error (Response, Table_Name & ": no such table");
      end if;
   end Handle_Report;

end Kit.Server.System;
