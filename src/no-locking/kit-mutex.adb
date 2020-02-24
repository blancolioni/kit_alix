package body Kit.Mutex is

   ----------
   -- Lock --
   ----------

   procedure Lock (Mutex : in out Mutex_Type) is
   begin
      Mutex.Locked := True;
   end Lock;

   -----------------
   -- Shared_Lock --
   -----------------

   procedure Shared_Lock (Mutex : in out Mutex_Type) is
   begin
      Mutex.Shared := Mutex.Shared + 1;
   end Shared_Lock;

   -------------------
   -- Shared_Unlock --
   -------------------

   procedure Shared_Unlock (Mutex : in out Mutex_Type) is
   begin
      Mutex.Shared := Mutex.Shared - 1;
   end Shared_Unlock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Mutex : in out Mutex_Type) is
   begin
      Mutex.Locked := False;
   end Unlock;

end Kit.Mutex;
