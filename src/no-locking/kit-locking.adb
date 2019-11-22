package body Kit.Locking is

   ------------
   -- S_Lock --
   ------------

   procedure S_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.S_Lock_Count := Item.S_Lock_Count + 1;
   end S_Lock;

   ------------
   -- U_Lock --
   ------------

   procedure U_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.U_Locked := True;
   end U_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : not null access Root_Lockable_Type) is
   begin
      pragma Assert (Item.S_Lock_Count > 0 or else Item.X_Locked);
      if Item.S_Lock_Count > 0 then
         Item.S_Lock_Count := Item.S_Lock_Count - 1;
      else
         Item.X_Locked := False;
      end if;
   end Unlock;

   ------------
   -- X_Lock --
   ------------

   procedure X_Lock (Item : not null access Root_Lockable_Type) is
   begin
      pragma Assert (Item.S_Lock_Count = 0);
      Item.U_Locked := False;
      Item.X_Locked := True;
   end X_Lock;

end Kit.Locking;
