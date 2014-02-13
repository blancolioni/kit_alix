private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;

package Kit.Generate.Transformations is

   type Substitutions is private;

   procedure Add_Substitution
     (Map : in out Substitutions;
      Old_Value : String;
      New_Value : String);

   procedure Copy_File
     (Source : String;
      Target : String;
      Map    : Substitutions);

   function Make_File_Name
     (Ada_Package_Name : String;
      File_Extension   : String)
      return String;

   function Substitute
     (Source : String;
      Map    : Substitutions)
      return String;

private

   package Substitution_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Substitutions is
      record
         Map : Substitution_Maps.Map;
      end record;

end Kit.Generate.Transformations;
