package Kit.Mutex is

   type Mutex_Type is tagged limited private;

   procedure Lock (Mutex : in out Mutex_Type);
   procedure Shared_Lock (Mutex : in out Mutex_Type);
   procedure Unlock (Mutex : in out Mutex_Type);
   procedure Shared_Unlock (Mutex : in out Mutex_Type);

   function Shared_Lock_Count (Mutex : Mutex_Type) return Natural;
   function Exclusive_Lock (Mutex : Mutex_Type) return Boolean;

private

   type Mutex_Type is tagged limited
      record
         Locked : Boolean := False;
         Shared : Natural := 0;
      end record;

   Memory_Mutex : Mutex_Type;

   function Shared_Lock_Count (Mutex : Mutex_Type) return Natural
   is (Mutex.Shared);

   function Exclusive_Lock (Mutex : Mutex_Type) return Boolean
   is (Mutex.Locked);

end Kit.Mutex;
