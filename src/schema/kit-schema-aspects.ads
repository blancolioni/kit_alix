private with WL.String_Maps;

package Kit.Schema.Aspects is

   type Root_Aspect_Type is tagged private;

   type Kit_Aspect is access all Root_Aspect_Type'Class;

   procedure Add_Aspect
     (To    : in out Root_Aspect_Type'Class;
      Name  : String;
      Value : String);

   function Aspect_Value
     (From : Root_Aspect_Type'Class;
      Name : String)
      return String;

private

   package Aspect_Maps is
     new WL.String_Maps (String);

   type Root_Aspect_Type is tagged
      record
         Map : Aspect_Maps.Map;
      end record;

end Kit.Schema.Aspects;
