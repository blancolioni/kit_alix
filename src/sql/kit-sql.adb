package body Kit.SQL is

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Element : in out SQL_Element) is
   begin
      Element.File := 1;
      Element.Line := 1;
      Element.Column := 1;
   end Initialize;

end Kit.SQL;
