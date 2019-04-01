package body Kit.SQL is

   ------------
   -- Create --
   ------------

   procedure Create (Element : in out SQL_Element'Class) is
   begin
      Element.File := 1;
      Element.Line := 1;
      Element.Column := 1;
   end Create;

end Kit.SQL;
