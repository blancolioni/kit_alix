with Marlowe.Data_Stores;

with System.Storage_Elements;

private package {database}.Kit_Deferred_Keys is

   procedure Key_Changed
     (Table     : Marlowe.Table_Index;
      Key       : Marlowe.Data_Stores.Key_Reference;
      Reference : Marlowe.Database_Index;
      Old_Value : System.Storage_Elements.Storage_Array;
      New_Value : System.Storage_Elements.Storage_Array)
     with Pre => Old_Value'Length = New_Value'Length;

   procedure Check_Keys
     (Table : Marlowe.Table_Index);

   procedure Close_Deferred_Keys;

end {database}.Kit_Deferred_Keys;
