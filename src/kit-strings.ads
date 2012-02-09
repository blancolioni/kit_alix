package Kit.Strings is

   type String_Type (Max_Length : Natural) is
      record
         Text     : String (1 .. Max_Length);
         Length   : Natural;
      end record;

end Kit.Strings;
