generic
   type Real is digits <>;
package Approximate_IO is

   procedure Put (Value : Real);
   procedure Put (To    : out String;
                  Value : Real);

   function Image (Value : Real) return String;

end Approximate_IO;
