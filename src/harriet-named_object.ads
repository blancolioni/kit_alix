package Harriet.Named_Object is

   type Named_Object_Interface is
     limited interface and Table_Search_Record;

   subtype Named_Object_Type is Named_Object_Interface'Class;

   function Name
     (Item : Named_Object_interface)
      return String
      is abstract;

   procedure Set_Name
     (Item  : in out Named_Object_interface;
      Value : in     String)
      is abstract;

   function Get (Reference : Named_Object_Reference)
                return Named_Object_Type;

   function Create return Named_Object_Type;

   function First_By_Name return Named_Object_Type;
   function First_By_Name
     (Value : String)
      return Named_Object_Type;
   function Get_By_Name
     (Value : String)
     return Named_Object_Type;

end Harriet.Named_Object;


