with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Approximate_IO is

   function Significant_Digits_Image (Item : Real;
                                      Sig  : Positive)
                                      return String;

   -----------
   -- Image --
   -----------

   function Image (Value : Real) return String is
      Factors    : constant array (1 .. 3) of Real :=
        (1.0E9, 1.0E6, 1.0E3);
      Extensions : constant String := "GMK";
   begin
      for I in Factors'Range loop
         if Value > Factors (I) then
            return Significant_Digits_Image (Value / Factors (I), 3) &
              (1 => Extensions (I));
         end if;
      end loop;

      return Significant_Digits_Image (Value, 3);
   end Image;

   ---------
   -- Put --
   ---------

   procedure Put (Value : Real) is
   begin
      Ada.Text_IO.Put (Image (Value));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (To    : out String;
      Value : Real)
   is
      Text : constant String := Image (Value);
   begin
      if Text'Length > To'Length then
         To := Text (Text'First .. Text'First + To'Length - 1);
      else
         To := (others => ' ');
         To (To'Last - Text'Length + 1 .. To'Last) := Text;
      end if;
   end Put;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image (Item : Real;
                                      Sig  : Positive)
                                     return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Real := Item;
      Boundary  : constant Real := 10.0**Sig;
   begin
      if Item < 1.0 / Boundary then
         return "0.00";
      end if;

      if abs Item >= Boundary then
         return Ada.Strings.Fixed.Trim (Integer'Image (Integer (Item)),
                                        Ada.Strings.Left);
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim (Integer'Image (Integer (Acc - 0.5)),
                                   Ada.Strings.Left);
         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               return Result (1 .. Result'Last - Point) & "." &
                 Result (Result'Last - Point + 1 .. Result'Last);
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                 (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

end Approximate_IO;
