with Marlowe.Btree_Handles;
with Kit.Cache;

generic
   Table       : Marlowe.Table_Index;
   Table_Magic : Natural;
   type Database_Record is private;
   with procedure Write (Ref  : Marlowe.Database_Index;
                         Item : Database_Record);
package Kit.Generic_Cache is

   type Cache_Record is new Kit.Cache.Cache_Entry_Record with
      record
         Db : Database_Record;
      end record;

   overriding
   procedure Write (Item   : Cache_Record;
                    Index  : Marlowe.Database_Index);

   type Cache_Access is access all Cache_Record'Class;

   function Get
     (Handle      : Marlowe.Btree_Handles.Btree_Handle;
      Index       : Marlowe.Database_Index;
      Lock_Result : Boolean := True)
      return Cache_Access;

end Kit.Generic_Cache;
