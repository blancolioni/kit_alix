private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

package Kit.String_Maps is

   type String_Map is tagged private;

   procedure Insert (Map   : in out String_Map;
                     Value : in String);

   function Contains (Map   : String_Map;
                      Value : String)
                      return Boolean;

private

   use Ada.Strings.Unbounded;

   package Hashed_String_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                     Element_Type    => Boolean,
                                     Hash            => Hash,
                                     Equivalent_Keys => "=");

   type String_Map is
     new Hashed_String_Maps.Map with null record;

end Kit.String_Maps;
