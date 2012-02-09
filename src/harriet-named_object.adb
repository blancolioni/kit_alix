with Ada.Strings.Unbounded;

with Harriet.Named_Object_Impl;

package body Harriet.Named_Object is

   ------------
   -- Create --
   ------------

   function Create return Named_Object_Type
   is
   begin
      return Result : Harriet.Named_Object_Impl.Named_Object_Implementation do
        Result.Named_Object_Record.Ref :=
          Harriet.Named_Object_Impl.Create;
        Result.Created := True;
        Result.Dirty   := True;
      end return;
   end Create;

   -------------------
   -- First_By_Name --
   -------------------

   function First_By_Name return Named_Object_Type is
      use Named_Object_Impl;
      use Named_Object_Impl.Named_Object_Name_Key;
      Handle : constant Key_Handle :=
                 Named_Object_Impl.Named_Object_Name_Key.First;
   begin
      if Handle.Valid then
         return Result : Named_Object_Implementation do
            Result.Valid      := True;
            Result.Dirty      := False;
            Result.Created    := False;
            Result.Searching  := True;
            Result.Ascending  := True;
            Result.Search_Key := K_Name;
            Result.Name_Key_Handle := Handle;
            Harriet.Named_Object_Impl.Read
              (Named_Object_Reference (Reference (Handle)),
               Result.Named_Object_Record);
         end return;
      else
         return Result : Named_Object_Implementation do
            Result.Valid := False;
         end return;
      end if;
   end First_By_Name;

   -------------------
   -- First_By_Name --
   -------------------

   function First_By_Name
     (Value : String)
      return Named_Object_Type
   is
      use Ada.Strings.Unbounded;
      use Named_Object_Impl;
      use Named_Object_Impl.Named_Object_Name_Key;
      Handle : constant Key_Handle :=
                 Named_Object_Impl.Named_Object_Name_Key.First
                   (To_Unbounded_String (Value));
   begin
      if Handle.Valid then
         return Result : Named_Object_Implementation do
            Result.Valid      := True;
            Result.Dirty      := False;
            Result.Created    := False;
            Result.Searching  := True;
            Result.Ascending  := True;
            Result.Search_Key := K_Name;
            Result.Name_Key_Handle := Handle;
            Harriet.Named_Object_Impl.Read
              (Named_Object_Reference (Reference (Handle)),
               Result.Named_Object_Record);
         end return;
      else
         return Result : Named_Object_Implementation do
            Result.Valid := False;
         end return;
      end if;
   end First_By_Name;

   ---------
   -- Get --
   ---------

   function Get (Reference : Named_Object_Reference)
                return Named_Object_Type
   is
   begin
      return Result : Harriet.Named_Object_Impl.Named_Object_Implementation do
        Harriet.Named_Object_Impl.Read (Reference, Result.Named_Object_Record);
      end return;
   end Get;

   function Get_By_Name
     (Value : String)
      return Named_Object_Type
   is
      use Ada.Strings.Unbounded;
      use Named_Object_Impl;
      use Named_Object_Impl.Named_Object_Name_Key;
      Handle : constant Key_Handle :=
                 Named_Object_Impl.Named_Object_Name_Key.First
                   (To_Unbounded_String (Value));
   begin
      if Handle.Valid then
         return Result : Named_Object_Implementation do
            Result.Valid      := True;
            Result.Dirty      := False;
            Result.Created    := False;
            Result.Searching  := False;
            Harriet.Named_Object_Impl.Read
              (Named_Object_Reference (Reference (Handle)),
               Result.Named_Object_Record);
         end return;
      else
         return Result : Named_Object_Implementation do
            Result.Valid := False;
         end return;
      end if;
   end Get_By_Name;

end Harriet.Named_Object;
