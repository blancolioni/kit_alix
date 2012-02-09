with Ada.Finalization;
private with Kit.Mutex;

package Kit.Locking is

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled
     with private;

   procedure X_Lock (Item : not null access Root_Lockable_Type);
   procedure S_Lock (Item : not null access Root_Lockable_Type);

   procedure Unlock (Item : not null access Root_Lockable_Type);

   function X_Locked (Item : Root_Lockable_Type) return Boolean;
   function S_Locked (Item : Root_Lockable_Type) return Boolean;

private

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled with
      record
         Lock : Kit.Mutex.Mutex_Type;
      end record;

end Kit.Locking;
