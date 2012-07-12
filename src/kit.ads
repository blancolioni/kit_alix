with Marlowe;

package Kit is

   type Search_Interface is limited interface;

   function Has_Element (Item : in     Search_Interface) return Boolean
      is abstract;
   procedure Next (Item : in out Search_Interface)
      is abstract;

   type Root_Database_Record is limited interface and Search_Interface;

   function Index (Item : Root_Database_Record) return Marlowe.Database_Index
                   is abstract;

   function Get (Item       : Root_Database_Record;
                 Field_Name : String)
                return String
      is abstract;

   procedure Set (Item       : in out Root_Database_Record;
                  Field_Name : String;
                  Value      : String)
   is abstract;

   function Field_Count (Item : Root_Database_Record) return Natural
                         is abstract;

   function Field_Name (Item : Root_Database_Record;
                        Index : Positive)
                        return String
                        is abstract;

   type Database_Record is access all Root_Database_Record'Class;

   procedure Close (Rec : in out Database_Record);

end Kit;
