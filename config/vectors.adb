with {database}.{table};

package body {database}.{table}_Vectors is

   package Base_{table}_Vectors is
     new Ada.Containers.Vectors (Positive, {table}_Reference);

   Base_{table}_Vector : Base_{table}_Vectors.Vector;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      {table} : {table}_Reference)
      return Element_Type
   is
   begin
      return Container.Container.Element
        (Positive ({table}));
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (V : in out Vector)
   is
   begin
      if Base_{table}_Vector.Last_Index = 0 then
         for {table} of {database}.{table}.Scan_By_Top_Record loop
            Base_{table}_Vector.Append ({table}.Get_{table}_Reference);
         end loop;
      end if;
      V.Container.Set_Length (Base_{table}_Vector.Length);
      for {table} of Base_{table}_Vector loop
         V.Container.Replace_Element (Positive ({table}), Default_Element);
      end loop;
   end Initialize;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access
        procedure ({table} : {table}_Reference;
                   Element   : Element_Type))
   is
   begin
      for I in 1 .. Container.Container.Last_Index loop
         Process ({table}_Reference (I), Container.Container (I));
      end loop;
   end Iterate;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      {table} : {table}_Reference;
      Element   : Element_Type)
   is
   begin
      Container.Container.Replace_Element
        (Positive ({table}), Element);
   end Replace_Element;

   -----------------
   -- Set_Element --
   -----------------

   procedure Set_Element
     (Container : in out Vector;
      {table} : {table}_Reference;
      Element   : Element_Type)
   is
   begin
      Container.Container.Replace_Element
        (Positive ({table}), Element);
   end Set_Element;

end {database}.{table}_Vectors;
