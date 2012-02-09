with Harriet.Unit;

with Harriet.Named_Object_Impl;

private package Harriet.Unit_Impl is

   type Unit_Database_Record is
      record
         Ref              : Unit_Reference;
         Named_Object_Ref : Named_Object_Reference;
         Size             : Natural;
      end record;

   procedure Read
     (Ref         : Unit_Reference;
      Unit_Record : out Unit_Database_Record);

   procedure Write
     (Ref         : Unit_Reference;
      Unit_Record : Unit_Database_Record);

   function Create return Unit_Reference;

   type Unit_Implementation is
     new Harriet.Unit.Unit_Interface with
      record
         Named_Object   : Harriet.Named_Object_Impl.Named_Object_Implementation;
         Unit_Object    : Unit_Database_Record;
      end record;

   function Name (Item : Unit_Implementation) return String;
   procedure Set_Name (Item  : in out Unit_Implementation;
                       Value : String);

   function Size (Item : Unit_Implementation) return Natural;
   procedure Set_Size (Item : in out Unit_Implementation;
                       Value : Natural);

   function More (Item : Unit_Implementation) return Boolean;
   function Exists (Item : Unit_Implementation) return Boolean;

end Harriet.Unit_Impl;
