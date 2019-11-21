with Ada.Containers.Hashed_Maps;
with {database}.{table}_Hashes;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package {database}.{table}_Reference_Maps is

   package Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => {table}_Reference,
        Element_Type    => Element_Type,
        Hash            => {database}.{table}_Hashes.Hash,
        Equivalent_Keys => "=",
        "="             => "=");

end {database}.{table}_Reference_Maps;
