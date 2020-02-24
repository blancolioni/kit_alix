with Ada.Finalization;

package Kit.Locking is

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled
     with private;

   procedure S_Lock (Item : not null access Root_Lockable_Type);
   procedure U_Lock (Item : not null access Root_Lockable_Type);
   procedure X_Lock (Item : not null access Root_Lockable_Type);

   procedure Unlock (Item : not null access Root_Lockable_Type);

   function Is_S_Locked (Item : Root_Lockable_Type) return Boolean;
   function Is_U_Locked (Item : Root_Lockable_Type) return Boolean;
   function Is_X_Locked (Item : Root_Lockable_Type) return Boolean;

private

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled with
      record
         S_Lock_Count : Natural := 0;
         U_Locked     : Boolean := False;
         X_Locked     : Boolean := False;
      end record;

   function Is_S_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.S_Lock_Count > 0);

   function Is_U_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.U_Locked);

   function Is_X_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.X_Locked);

end Kit.Locking;
