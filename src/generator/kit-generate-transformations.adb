with Ada.Characters.Handling;
with Ada.Text_IO;

package body Kit.Generate.Transformations is

   ----------------------
   -- Add_Substitution --
   ----------------------

   procedure Add_Substitution
     (Map : in out Substitutions;
      Old_Value : String;
      New_Value : String)
   is
   begin
      Map.Map.Insert (Old_Value, New_Value);
   end Add_Substitution;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Source : String;
      Target : String;
      Map    : Substitutions)
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
      Map    : Substitutions)
      return String
   is
      use Ada.Strings.Fixed;
      Start : constant Natural := Index (Source, "{");
      Finish : Natural;
   begin
      if Start = 0 then
         return Source;
      else
         Finish := Index (Source, "}", Start + 1);
         if Finish = 0 then
            return Source;
         else
            declare
               use Substitution_Maps;
               Key : constant String := Source (Start + 1 .. Finish - 1);
               Position : constant Cursor := Map.Map.Find (Key);
            begin
               return Source (Source'First .. Start - 1)
                 & (if Has_Element (Position)
                    then Element (Position)
                    else Source (Start .. Finish))
                 & Substitute (Source (Finish + 1 .. Source'Last), Map);
            end;
         end if;
      end if;
   end Substitute;

end Kit.Generate.Transformations;
