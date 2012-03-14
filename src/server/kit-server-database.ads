private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;

with System.Storage_Elements;

with Marlowe;

package Kit.Server.Database is

   type Database_Record is tagged limited private;

   procedure Get
     (Item     : in out Database_Record'Class;
      Table    : in     Marlowe.Table_Index;
      Index    : in     Marlowe.Database_Index);

   function Table_Index
     (Item : Database_Record'Class)
      return Marlowe.Table_Index;

   function Record_Index
     (Item : Database_Record'Class)
      return Marlowe.Database_Index;

   procedure Report (Item     : Database_Record'Class;
                     Width    : Positive := 12;
                     Across   : Boolean := False;
                     Headings : Boolean := False);

   procedure Report (Table : Marlowe.Table_Index);

private

   package Storage_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive,
        System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Database_Record is
      limited new Ada.Finalization.Limited_Controlled with
      record
         Table   : Marlowe.Table_Index;
         Index   : Marlowe.Database_Index;
         Value   : Storage_Vectors.Vector;
      end record;

end Kit.Server.Database;
