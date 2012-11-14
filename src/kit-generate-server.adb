with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Text_IO;

with Kit.Paths;

package body Kit.Generate.Server is

   subtype Substitution_Key is String (1 .. 32);
   package Substitution_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Substitution_Key,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   procedure Copy_File
     (Source : String;
      Target : String;
      Map    : Substitution_Maps.Map);

   function Make_File_Name
     (Ada_Package_Name : String;
      File_Extension   : String)
      return String;

   function Substitute
     (Source : String;
      Map    : Substitution_Maps.Map)
      return String;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Source : String;
      Target : String;
      Map    : Substitution_Maps.Map)
   is
      use Ada.Text_IO;
      F, G : File_Type;
   begin
      Open (F, In_File, Source);
      Create (G, Out_File, Target);

      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            Put_Line (G, Substitute (Line, Map));
         end;
      end loop;
      Close (G);
      Close (F);
   end Copy_File;

   --------------------------
   -- Copy_Server_Packages --
   --------------------------

   procedure Copy_Server_Packages
     (Database_Name    : String;
      Target_Directory : String)
   is
      Standard_Sub : Substitution_Maps.Map;
      Key          : Substitution_Key;

      procedure Copy (Package_Name : String);

      ----------
      -- Copy --
      ----------

      procedure Copy (Package_Name : String) is
         Source_Spec : constant String :=
                         Make_File_Name (Package_Name, "ads");
         Source_Body : constant String :=
                         Make_File_Name (Package_Name, "adb");
         Target_Spec : constant String :=
                         Make_File_Name (Database_Name & "." & Package_Name,
                                         "ads");
         Target_Body : constant String :=
                         Make_File_Name (Database_Name & "." & Package_Name,
                                         "adb");
      begin
         Copy_File (Kit.Paths.Config_Path & "/" & Source_Spec,
                    Target_Directory & "/" & Target_Spec,
                    Standard_Sub);
         Copy_File (Kit.Paths.Config_Path & "/" & Source_Body,
                    Target_Directory & "/" & Target_Body,
                    Standard_Sub);
      end Copy;

   begin
      Ada.Strings.Fixed.Move ("database", Key);
      Standard_Sub.Insert (Key, Database_Name);

      Copy ("Tables");
      Copy ("Tables.SK_Tables");
      Copy ("SK_Bindings");

   end Copy_Server_Packages;

   --------------------
   -- Make_File_Name --
   --------------------

   function Make_File_Name
     (Ada_Package_Name : String;
      File_Extension   : String)
      return String
   is
      Result : String :=
                 Ada.Characters.Handling.To_Lower (Ada_Package_Name);
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;
      return Result & "." & File_Extension;
   end Make_File_Name;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Source : String;
      Map    : Substitution_Maps.Map)
      return String
   is
      use Ada.Strings.Fixed;
      Start : constant Natural := Index (Source, "{");
      Finish : Natural;
      Key   : Substitution_Key;
   begin
      if Start = 0 then
         return Source;
      else
         Finish := Index (Source, "}", Start + 1);
         if Finish = 0 then
            return Source;
         else
            Move (Source (Start + 1 .. Finish - 1), Key);
            return Source (Source'First .. Start - 1)
              & (if Map.Contains (Key)
                 then Map.Element (Key)
                 else Source (Start .. Finish))
              & Substitute (Source (Finish + 1 .. Source'Last), Map);
         end if;
      end if;
   end Substitute;

end Kit.Generate.Server;
