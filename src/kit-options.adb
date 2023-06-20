with WL.Command_Line;

package body Kit.Options is

   pragma Style_Checks (Off);

   function Deadlock_Detection return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("deadlock-detection", ' ');
   end Deadlock_Detection;

   function Debug return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("debug", ' ');
   end Debug;

   function Ada_2022 return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("ada-2022", ' ');
   end Ada_2022;

end Kit.Options;
