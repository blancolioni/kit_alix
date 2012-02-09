with Harriet.Named_Object;

package Harriet.Unit is

   type Unit_Interface is
     interface
       and Table_Search_Record
       and Harriet.Named_Object.Named_Object_Interface;

   subtype Unit_Type is Unit_Interface'Class;

   function Size (Item : Unit_Interface) return Natural is abstract;
   procedure Set_Size (Item : in out Unit_Interface;
                       Value : in Natural)
      is abstract;

   function Get (Reference : Unit_Reference) return Unit_Type;
   function Create return Unit_Type;
   function First_By_Name return Unit_Type;
   function First_By_Name (Value : String) return Unit_Type;

end Harriet.Unit;


