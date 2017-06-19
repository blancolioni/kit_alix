with Ada.Strings.Fixed;

package body Kit.Schema is

   -----------
   -- Image --
   -----------

   function Image (Value : Integer_64) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Integer_64'Image (Value), Ada.Strings.Left);
   end Image;

end Kit.Schema;
