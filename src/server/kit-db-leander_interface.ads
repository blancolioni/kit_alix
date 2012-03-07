with SK;

package Kit.Db.Leander_Interface is

   function To_SK_Object
     (Ref : Kit_Root_Record_Reference)
      return SK.Object;

   function To_Root_Record_Reference
     (Value : SK.Object)
      return Kit_Root_Record_Reference;

end Kit.Db.Leander_Interface;
