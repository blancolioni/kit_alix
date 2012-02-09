package body Kit.Locking is

   ------------
   -- S_Lock --
   ------------

   procedure S_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.Shared_Lock;
   end S_Lock;

   --------------
   -- S_Locked --
   --------------

   function S_Locked (Item : Root_Lockable_Type) return Boolean is
   begin
      return Item.Lock.Shared_Lock_Count > 0;
   end S_Locked;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : not null access Root_Lockable_Type) is
   begin
      if Item.Lock.Exclusive_Lock then
         Item.Lock.Unlock;
      else
         Item.Lock.Shared_Unlock;
      end if;
   end Unlock;

   ------------
   -- X_Lock --
   ------------

   procedure X_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.Lock;
   end X_Lock;

   --------------
   -- X_Locked --
   --------------

   function X_Locked (Item : Root_Lockable_Type) return Boolean is
   begin
      return Item.Lock.Exclusive_Lock;
   end X_Locked;

end Kit.Locking;
