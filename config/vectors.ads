private with Ada.Containers.Vectors;
private with Ada.Finalization;

generic
   type Element_Type is private;
   Default_Element : Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package {database}.{table}_Vectors is

   type Vector is tagged private;

   function Element (Container : Vector;
                     {table} : {table}_Reference)
                     return Element_Type;

   procedure Set_Element (Container : in out Vector;
                          {table} : {table}_Reference;
                          Element   : Element_Type);

   procedure Replace_Element
     (Container : in out Vector;
      {table} : {table}_Reference;
      Element   : Element_Type);

   procedure Iterate
     (Container : Vector;
      Process   : not null access
        procedure ({table} : {table}_Reference;
                   Element   : Element_Type));

private

   package Vectors is
     new Ada.Containers.Vectors (Positive,
                                 Element_Type,
                                 "=");

   type Vector is new Ada.Finalization.Controlled with
      record
         Container : Vectors.Vector;
      end record;

   overriding procedure Initialize
     (V : in out Vector);

end {database}.{table}_Vectors;
