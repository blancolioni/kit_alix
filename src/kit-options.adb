package body Kit.Options is

   ---------------------------------
   -- Generate_Deadlock_Detection --
   ---------------------------------

   function Generate_Deadlock_Detection return Boolean is
   begin
      return True;
   end Generate_Deadlock_Detection;

   --------------------
   -- Generate_Debug --
   --------------------

   function Generate_Debug return Boolean is
   begin
      return False;
   end Generate_Debug;

end Kit.Options;
