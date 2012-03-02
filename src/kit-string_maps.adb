package body Kit.String_Maps is

   --------------
   -- Contains --
   --------------

   function Contains
     (Map   : String_Map;
      Value : String)
      return Boolean
   is
   begin
      return Map.Contains (To_Unbounded_String (Value));
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Map   : in out String_Map;
      Value : in String)
   is
   begin
      Map.Insert (To_Unbounded_String (Value), True);
   end Insert;

end Kit.String_Maps;
