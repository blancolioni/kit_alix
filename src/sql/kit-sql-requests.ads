private with System.Storage_Elements;
private with Ada.Containers.Indefinite_Holders;

with Kit.SQL.Constraints;
with Kit.SQL.Tables.Lists;
with Kit.SQL.Database;

private package Kit.SQL.Requests is

   type Request_Type is tagged private;

   procedure Create
     (Request     : in out Request_Type'Class;
      Tables      : Kit.SQL.Tables.Lists.List;
      Constraints : Kit.SQL.Constraints.Constraint_List'Class);

   procedure Execute
     (Request   : Request_Type'Class;
      On_Record : not null access
        procedure (Table : Kit.SQL.Database.Table_Reference;
                   Record_Index : Marlowe.Database_Index));

private

   package Storage_Array_Holders is
     new Ada.Containers.Indefinite_Holders
       (System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Request_Type is tagged
      record
         Table                : Kit.SQL.Database.Table_Reference;
         Search_Key           : Kit.SQL.Database.Key_Reference;
         Search_Start         : Storage_Array_Holders.Holder;
         Search_Finish        : Storage_Array_Holders.Holder;
         Search_Start_Closed  : Boolean;
         Search_Finish_Closed : Boolean;
         Constraints          : Kit.SQL.Constraints.Constraint_List;
      end record;

end Kit.SQL.Requests;
