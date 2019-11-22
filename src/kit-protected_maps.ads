with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

generic
   type Key_Type is private;
   type Element_Type is private;
   with procedure Load (Key : Key_Type; Element : in out Element_Type);
   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
package Kit.Protected_Maps is

   type Constant_Reference_Type (Element : access constant Element_Type)
   is private
     with Implicit_Dereference => Element;

   type Cursor is private;

   type Map is tagged limited private;

   function Constant_Reference
     (Container : in out Map;
      Key       : Key_Type)
      return Constant_Reference_Type;

   procedure Invalidate
     (Container : in out Map;
      Key       : Key_Type);

private

   type Element_Access is access Element_Type;

   package Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Key_Type,
        Element_Type    => Element_Access,
        Hash            => Hash,
        Equivalent_Keys => Equivalent_Keys,
        "="             => "=");

   package Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Access);

   protected type Protected_Map is

      procedure Get_Cached_Reference
        (Key    : Key_Type;
         Result : out Element_Access);

      procedure Invalidate (Key : Key_Type);

   private

      Map  : Maps.Map;
      Free : Lists.List;

   end Protected_Map;

   type Cursor is
      record
         Position : Maps.Cursor;
      end record;

   type Map is tagged limited
      record
         Internal : Protected_Map;
      end record;

   type Constant_Reference_Type (Element : access constant Element_Type)
   is null record;

end Kit.Protected_Maps;
