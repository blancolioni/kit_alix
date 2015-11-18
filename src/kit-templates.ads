private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;

package Kit.Templates is

   type Substitutions is private;

   procedure Add_Substitution
     (Map : in out Substitutions;
      Old_Value : String;
      New_Value : String);

   type Get_Item_Callback is
     access function (Index : Positive) return String;

   procedure Add_Substitution
     (Map        : in out Substitutions;
      Loop_Name  : String;
      Loop_Count : Natural;
      Get_Item   : Get_Item_Callback);

   procedure Copy_File
     (Source : String;
      Target : String;
      Map    : Substitutions);

   function Make_File_Name
     (Ada_Package_Name : String;
      File_Extension   : String)
      return String;

private

   package Substitution_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Loop_Map_Record is
      record
         Loop_Count : Natural;
         Get_Item   : Get_Item_Callback;
      end record;

   package Loop_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Loop_Map_Record,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Substitutions is
      record
         Simple_Map : Substitution_Maps.Map;
         Loop_Map   : Loop_Maps.Map;
      end record;

end Kit.Templates;
