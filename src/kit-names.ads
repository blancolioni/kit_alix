private with Ada.Strings.Unbounded;

package Kit.Names is

   type Root_Named_Object is abstract tagged private;

   function Name (Item : Root_Named_Object) return String;
   function Ada_Name (Item : Root_Named_Object) return String;
   function Safe_Ada_Name (Item : Root_Named_Object;
                           Safe_Prefix : String)
                           return String;

   function Standard_Name (Item : Root_Named_Object) return String;

   procedure Create (Item : in out Root_Named_Object;
                     Name : String);

   function Ada_Name (Raw_Name : String) return String;
   function Standard_Name (Raw_Name : String) return String;

private

   type Root_Named_Object is abstract tagged
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Kit.Names;
