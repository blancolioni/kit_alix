with Harriet.Named_Object_Impl;
with Harriet.Unit_Impl;

package body Harriet.Unit is

   ------------
   -- Create --
   ------------

   function Create return Unit_Type is
      Result : Harriet.Unit_Impl.Unit_Implementation;
   begin
      Result.Unit_Object.Ref := Harriet.Unit_Impl.Create;
      Result.Unit_Object.Named_Object_Ref :=
        Harriet.Named_Object_Impl.Create;
      Result.Named_Object.Ref := Result.Unit_Object.Named_Object_Ref;
      return Result;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Reference : Unit_Reference) return Unit_Type is
      Result : Harriet.Unit_Impl.Unit_Implementation;
   begin
      Harriet.Unit_Impl.Read (Reference, Result.Unit_Object);
      Harriet.Named_Object_Impl.Read (Result.Unit_Object.Named_Object_Ref,
                                      Result.Named_Object);
      return Result;
   end Get;

end Harriet.Unit;
