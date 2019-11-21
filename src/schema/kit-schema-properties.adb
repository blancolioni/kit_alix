package body Kit.Schema.Properties is

   type Properties_Access is access all Has_Properties_Interface'Class;

   package Has_Properties_Maps is
     new WL.String_Maps (Properties_Access);

   Has_Properties_Map : Has_Properties_Maps.Map;

   ------------
   -- Append --
   ------------

   procedure Append
     (Value : in out Kit_Property_Value;
      Text  : String)
   is
   begin
      Value.List.Append (Text);
   end Append;

   --------------------
   -- Get_Properties --
   --------------------

   function Get_Properties
     (Name : String)
      return access Has_Properties_Interface'Class
   is
   begin
      return Has_Properties_Map.Element (Name);
   end Get_Properties;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Properties : Properties_Map;
      Name       : String)
      return Kit_Property_Value
   is
   begin
      return Properties.Map.Element (Name);
   end Get_Property;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Properties : Properties_Map;
      Name       : String)
      return Boolean
   is
   begin
      return Properties.Map.Contains (Name);
   end Has_Property;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Value   : Kit_Property_Value;
      Process : not null access procedure (Element : String))
   is
   begin
      case Value.Class is
         when String_Value =>
            Process (To_String (Value));
         when List_Value =>
            for Item of Value.List loop
               Process (Item);
            end loop;
      end case;
   end Iterate;

   ----------------------
   -- Properties_Exist --
   ----------------------

   function Properties_Exist (Name : String) return Boolean is
   begin
      return Has_Properties_Map.Contains (Name);
   end Properties_Exist;

   -------------------------
   -- Register_Properties --
   -------------------------

   procedure Register_Properties
     (Properties : not null access Has_Properties_Interface'Class;
      Name       : String)
   is
   begin
      Has_Properties_Map.Insert (Name, Properties_Access (Properties));
   end Register_Properties;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Properties : in out Properties_Map;
      Name       : String;
      Value      : Kit_Property_Value)
   is
   begin
      if Properties.Map.Contains (Name) then
         Properties.Map.Replace (Name, Value);
      else
         Properties.Map.Insert (Name, Value);
      end if;
   end Set_Property;

end Kit.Schema.Properties;
