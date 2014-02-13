package body {database}.{table}_Maps is

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Map;
      {table} : {table}_Reference)
      return Boolean
   is
   begin
      return Container.Map.Contains ({table});
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      {table} : {table}_Reference)
      return Element_Type
   is
   begin
      return Container.Map.Element ({table});
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      {table} : {table}_Reference;
      Default   : Element_Type)
      return Element_Type
   is
      use Maps;
      Position : constant Cursor := Container.Map.Find ({table});
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return Default;
      end if;
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      {table} : {table}_Reference;
      Element   : Element_Type)
   is
   begin
      Container.Map.Insert ({table}, Element);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure ({table} : {table}_Reference;
                   Element   : Element_Type))
   is
      use Maps;
   begin
      for Position in Container.Map.Iterate loop
         Process (Key (Position), Element (Position));
      end loop;
   end Iterate;

   -------------
   -- Replace --
   -------------

   procedure Replace_Element
     (Container : in out Map;
      {table} : {table}_Reference;
      Element   : Element_Type)
   is
   begin
      Container.Map.Replace_Element
        (Container.Map.Find ({table}), Element);
   end Replace_Element;

end {database}.{table}_Maps;
