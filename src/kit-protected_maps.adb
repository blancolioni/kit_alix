package body Kit.Protected_Maps is

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : in out Map;
      Key       : Key_Type)
      return Constant_Reference_Type
   is
      Element : Element_Access;
   begin
      Container.Internal.Get_Cached_Reference (Key, Element);
      return Constant_Reference_Type'
        (Element => Element);
   end Constant_Reference;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (Container : in out Map;
      Key       : Key_Type) is
   begin
      Container.Internal.Invalidate (Key);
   end Invalidate;

   -------------------
   -- Protected_Map --
   -------------------

   protected body Protected_Map is

      --------------------------
      -- Get_Cached_Reference --
      --------------------------

      procedure Get_Cached_Reference
        (Key    : Key_Type;
         Result : out Element_Access)
      is
         Position : constant Maps.Cursor := Map.Find (Key);
      begin
         if Maps.Has_Element (Position) then
            Result := Maps.Element (Position);
         else
            if Free.Is_Empty then
               Result := new Element_Type;
            else
               Result := Free.First_Element;
               Free.Delete_First;
            end if;
            Load (Key, Result.all);
            Map.Insert (Key, Result);
         end if;
      end Get_Cached_Reference;

      ----------------
      -- Invalidate --
      ----------------

      procedure Invalidate (Key : Key_Type) is
         Position : Maps.Cursor := Map.Find (Key);
      begin
         if Maps.Has_Element (Position) then
            Free.Append (Maps.Element (Position));
            Map.Delete (Position);
         end if;
      end Invalidate;

   end Protected_Map;

end Kit.Protected_Maps;
