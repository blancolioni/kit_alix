package body Kit.Schema.Aspects is

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (To    : in out Root_Aspect_Type'Class;
      Name  : String;
      Value : String)
   is
   begin
      To.Map.Insert (Name, Value);
   end Add_Aspect;

   ------------------
   -- Aspect_Value --
   ------------------

   function Aspect_Value
     (From : Root_Aspect_Type'Class;
      Name : String)
      return String
   is
   begin
      if From.Map.Contains (Name) then
         return From.Map.Element (Name);
      else
         return "";
      end if;
   end Aspect_Value;

end Kit.Schema.Aspects;
