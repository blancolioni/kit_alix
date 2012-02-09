private with Marlowe;

package Harriet is

   type Table_Type is
     (T_No_Table,
      T_Named_Object,
      T_Owner_Object,
      T_Owned_Object,
      T_Enterprise,
      T_Unit);

   type Key_Type is
     (K_No_Key,
      K_Name);

   type Named_Object_Reference is private;
   Null_Named_Reference : constant Named_Object_Reference;

   type Owner_Object_Reference is private;
   Null_Owner_Reference : constant Owner_Object_Reference;

   type Owned_Object_Reference is private;
   Null_Owned_Reference : constant Owned_Object_Reference;

   type Enterprise_Reference is private;
   Null_Enterprise_Reference : constant Enterprise_Reference;

   type Unit_Reference is private;
   Null_Unit_Reference : constant Unit_Reference;

   type Table_Search_Record is limited interface;

   function Has_Element
     (Item : Table_Search_Record)
      return Boolean
      is abstract;

   procedure Next
     (Item : in out Table_Search_Record)
   is abstract;

private

   type Named_Object_Reference is new Marlowe.Database_Index;
   Null_Named_Reference : constant Named_Object_Reference := 0;

   type Owner_Object_Reference is new Marlowe.Database_Index;
   Null_Owner_Reference : constant Owner_Object_Reference := 0;

   type Owned_Object_Reference is new Marlowe.Database_Index;
   Null_Owned_Reference : constant Owned_Object_Reference := 0;

   type Enterprise_Reference is new Marlowe.Database_Index;
   Null_Enterprise_Reference : constant Enterprise_Reference := 0;

   type Unit_Reference is new Marlowe.Database_Index;
   Null_Unit_Reference : constant Unit_Reference := 0;

end Harriet;
