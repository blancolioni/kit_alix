with Ada.Characters.Handling;

package body Kit.Names is

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Raw_Name : String) return String is
      use Ada.Characters.Handling;
      Result : String := Raw_Name;
      Capital : Boolean := True;
   begin
      for I in Result'Range loop
         if Capital then
            Result (I) := To_Upper (Result (I));
            Capital := False;
         elsif Result (I) = '_'
           or else Result (I) = '.'
         then
            Capital := True;
         end if;
      end loop;
      return Result;
   end Ada_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Item : Root_Named_Object) return String is
   begin
      return Ada_Name (Name (Root_Named_Object'Class (Item)));
   end Ada_Name;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item : in out Root_Named_Object;
      Name : String)
   is
   begin
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Create;

   ------------------
   -- Haskell_Name --
   ------------------

   function Haskell_Name (Item : Root_Named_Object) return String is
   begin
      return Haskell_Name (Name (Root_Named_Object'Class (Item)));
   end Haskell_Name;

   ------------------
   -- Haskell_Name --
   ------------------

   function Haskell_Name (Raw_Name : String) return String is
      Ada_Text : constant String := Ada_Name (Raw_Name);
      Haskell_Text : String (Ada_Text'Range);
      Haskell_Length : Natural := 0;
   begin
      for I in Ada_Text'Range loop
         if Ada_Text (I) /= '_' then
            Haskell_Length := Haskell_Length + 1;
            Haskell_Text (Haskell_Length) := Ada_Text (I);
         end if;
      end loop;
      return Haskell_Text (1 .. Haskell_Length);
   end Haskell_Name;

   ---------------------------
   -- Haskell_Variable_Name --
   ---------------------------

   function Haskell_Variable_Name (Item : Root_Named_Object) return String is
   begin
      return Haskell_Variable_Name (Name (Root_Named_Object'Class (Item)));
   end Haskell_Variable_Name;

   ---------------------------
   -- Haskell_Variable_Name --
   ---------------------------

   function Haskell_Variable_Name (Raw_Name : String) return String is
      Result : String := Haskell_Name (Raw_Name);
   begin
      Result (Result'First) :=
        Ada.Characters.Handling.To_Lower (Result (Result'First));
      return Result;
   end Haskell_Variable_Name;

   ----------
   -- Name --
   ----------

   function Name (Item : Root_Named_Object) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   -------------------
   -- Safe_Ada_Name --
   -------------------

   function Safe_Ada_Name (Item : Root_Named_Object;
                           Safe_Prefix : String)
                           return String
   is
      Result : constant String :=
                 Root_Named_Object'Class (Item).Ada_Name;
   begin
      if Result = "Type" then
         return Safe_Prefix & Result;
      else
         return Result;
      end if;
   end Safe_Ada_Name;

   -------------------
   -- Standard_Name --
   -------------------

   function Standard_Name (Item : Root_Named_Object) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Item.Ada_Name);
   end Standard_Name;

   -------------------
   -- Standard_Name --
   -------------------

   function Standard_Name (Raw_Name : String) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Raw_Name);
   end Standard_Name;

end Kit.Names;
