with Ada.Containers.Hashed_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package {database}.{table}_Maps is

   type Map is tagged private;

   function Contains (Container : Map;
                      {table} : {table}_Reference)
                      return Boolean;

   function Element (Container : Map;
                     {table} : {table}_Reference)
                     return Element_Type;

   function Element (Container : Map;
                     {table} : {table}_Reference;
                     Default   : Element_Type)
                     return Element_Type;

   procedure Clear (Container : in out Map);
   
   procedure Insert (Container : in out Map;
                     {table} : {table}_Reference;
                     Element   : Element_Type);

   procedure Replace_Element (Container : in out Map;
                              {table} : {table}_Reference;
                              Element   : Element_Type);

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure ({table} : {table}_Reference;
                   Element   : Element_Type));

private

   function Hash
     (Item : {table}_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item mod Ada.Containers.Hash_Type'Modulus));

   package Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => {table}_Reference,
        Element_Type    => Element_Type,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type Map is tagged
      record
         Map : Maps.Map;
      end record;

end {database}.{table}_Maps;
