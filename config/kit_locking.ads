with Marlowe;
with {database}.Table_Names;

private package {database}.Kit_Locking is

   type Request_Id is private;
   
   No_Request : constant Request_Id;
   
   function Request_Lock
     (Table     : Table_Names.Table_Reference;
      Index     : Marlowe.Database_Index;
      Location  : Positive)
      return Request_Id;

   procedure Got_Lock (Id : Request_Id);
   procedure Release_Lock (Id : in out Request_Id);

   procedure Start_Scanning;
   procedure Stop_Scanning;

private

   type Request_Id is new Natural;
   subtype Real_Request_Id is Request_Id range 1 .. Request_Id'Last;
   
   No_Request : constant Request_Id := 0;
   
end {database}.Kit_Locking;
