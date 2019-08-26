private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

package Kit.Schema.Properties is

   type Kit_Property_Value (<>) is private;

   function Is_String (Value : Kit_Property_Value) return Boolean;
   function Is_List   (Value : Kit_Property_Value) return Boolean;

   function To_String (Value : Kit_Property_Value) return String
     with Pre => Is_String (Value);

   procedure Iterate
     (Value : Kit_Property_Value;
      Process : not null access
        procedure (Element : String));

   function String_Value (Text : String) return Kit_Property_Value;

   function List_Value return Kit_Property_Value;

   procedure Append
     (Value : in out Kit_Property_Value;
      Text  : String);

   type Has_Properties_Interface is interface;

   function Has_Property
     (Properties : Has_Properties_Interface;
      Name       : String)
      return Boolean
      is abstract;

   function Get_Property
     (Properties : Has_Properties_Interface;
      Name       : String)
      return Kit_Property_Value
      is abstract;

   procedure Set_Property
     (Properties : in out Has_Properties_Interface;
      Name       : String;
      Value      : Kit_Property_Value)
      is abstract;

   procedure Register_Properties
     (Properties : not null access Has_Properties_Interface'Class;
      Name       : String);

   function Properties_Exist
     (Name : String)
      return Boolean;

   function Get_Properties
     (Name : String)
      return access Has_Properties_Interface'Class;

   type Properties_Map is
     new Has_Properties_Interface with private;

   overriding function Has_Property
     (Properties : Properties_Map;
      Name       : String)
      return Boolean;

   overriding function Get_Property
     (Properties : Properties_Map;
      Name       : String)
      return Kit_Property_Value;

   overriding procedure Set_Property
     (Properties : in out Properties_Map;
      Name       : String;
      Value      : Kit_Property_Value);

private

   type Value_Class is (String_Value, List_Value);

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Kit_Property_Value (Class : Value_Class) is
      record
         case Class is
            when String_Value =>
               Text : Ada.Strings.Unbounded.Unbounded_String;
            when List_Value =>
               List : String_Lists.List;
         end case;
      end record;

   function Is_String (Value : Kit_Property_Value) return Boolean
   is (Value.Class = String_Value);

   function Is_List   (Value : Kit_Property_Value) return Boolean
   is (Value.Class = List_Value);

   function To_String (Value : Kit_Property_Value) return String
   is (Ada.Strings.Unbounded.To_String (Value.Text));

   function String_Value (Text : String) return Kit_Property_Value
   is (String_Value, Ada.Strings.Unbounded.To_Unbounded_String (Text));

   function List_Value return Kit_Property_Value
   is (List_Value, List => <>);

   package Property_Maps is
     new WL.String_Maps (Kit_Property_Value);

   type Properties_Map is
     new Has_Properties_Interface with
      record
         Map : Property_Maps.Map;
      end record;

end Kit.Schema.Properties;
