package Kit.Schema is

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;

   function Image (Value : Integer_64) return String;

end Kit.Schema;
