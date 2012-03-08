package body Kit.Db.Leander_Interface is

   -----------------------------
   -- To_Named_Item_Reference --
   -----------------------------

   function To_Named_Item_Reference
     (Value : SK.Object)
      return Kit_Named_Item_Reference
   is
   begin
      return Kit_Named_Item_Reference (SK.Get_Integer (Value));
   end To_Named_Item_Reference;

   ------------------------------
   -- To_Root_Record_Reference --
   ------------------------------

   function To_Root_Record_Reference
     (Value : SK.Object)
      return Kit_Root_Record_Reference
   is
   begin
      return Kit_Root_Record_Reference (SK.Get_Integer (Value));
   end To_Root_Record_Reference;

   ------------------
   -- To_SK_Object --
   ------------------

   function To_SK_Object
     (Ref : Kit_Root_Record_Reference)
      return SK.Object
   is
   begin
      return SK.To_Object (Integer (Ref));
   end To_SK_Object;

end Kit.Db.Leander_Interface;
