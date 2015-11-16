with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

package body Kit.Templates is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   function Begins_Loop (Line : String) return Boolean;
   function Ends_Loop
     (Line : String;
      Name : String)
      return Boolean;

   function Get_Loop_Name (Line : String) return String
     with Pre => Begins_Loop (Line);

   function File_Changed
     (Original_File_Path : String;
      New_File_Path      : String)
      return Boolean;

   ----------------------
   -- Add_Substitution --
   ----------------------

   procedure Add_Substitution
     (Map : in out Substitutions;
      Old_Value : String;
      New_Value : String)
   is
   begin
      Map.Simple_Map.Insert (Old_Value, New_Value);
   end Add_Substitution;

   ----------------------
   -- Add_Substitution --
   ----------------------

   procedure Add_Substitution
     (Map        : in out Substitutions;
      Loop_Name  : String;
      Loop_Count : Natural;
      Get_Item   : Get_Item_Callback)
   is
   begin
      Map.Loop_Map.Insert
        (Loop_Name, (Loop_Count, Get_Item));
   end Add_Substitution;

   -----------------
   -- Begins_Loop --
   -----------------

   function Begins_Loop (Line : String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Line, "{loop:") > 0;
   end Begins_Loop;

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
      Temp_Path : constant String := Target & "-t";
   begin
      Open (F, In_File, Source);
      Create (G, Out_File, Temp_Path);

      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            if Begins_Loop (Line) then
               declare
                  Loop_Name  : constant String :=
                                 Get_Loop_Name (Line);
                  Loop_Info  : constant Loop_Map_Record :=
                                 Map.Loop_Map.Element (Loop_Name);
                  Loop_Lines : String_Lists.List;
                  Temp_Sub   : Substitutions := Map;
               begin
                  loop
                     declare
                        Loop_Line : constant String :=
                                      Get_Line (F);
                     begin
                        exit when Ends_Loop (Loop_Line, Loop_Name);
                        Loop_Lines.Append (Loop_Line);
                     end;
                  end loop;

                  for I in 1 .. Loop_Info.Loop_Count loop
                     Temp_Sub.Simple_Map.Insert
                       (Loop_Name, Loop_Info.Get_Item (I));
                     for Line of Loop_Lines loop
                        Put_Line (G, Substitute (Line, Temp_Sub));
                     end loop;
                     Temp_Sub.Simple_Map.Delete (Loop_Name);
                  end loop;
               end;
            else
               Put_Line (G, Substitute (Line, Map));
            end if;
         end;
      end loop;
      Close (G);
      Close (F);

      if not Ada.Directories.Exists (Target) then
         Ada.Directories.Rename (Temp_Path, Target);
      elsif File_Changed (Target, Temp_Path) then
         Ada.Directories.Delete_File (Target);
         Ada.Directories.Rename (Temp_Path, Target);
      else
         Ada.Directories.Delete_File (Temp_Path);
      end if;

   end Copy_File;

   ---------------
   -- Ends_Loop --
   ---------------

   function Ends_Loop
     (Line : String;
      Name : String)
      return Boolean
   is
   begin
      return Ada.Strings.Fixed.Index (Line, "{end-loop:" & Name & "}") > 0;
   end Ends_Loop;

   ------------------
   -- File_Changed --
   ------------------

   function File_Changed
     (Original_File_Path : String;
      New_File_Path      : String)
      return Boolean
   is
      use Ada.Text_IO;
      Old_File, New_File : File_Type;
      Changed            : Boolean := False;
   begin
      Open (Old_File, In_File, Original_File_Path);
      Open (New_File, In_File, New_File_Path);
      while not End_Of_File (Old_File) loop
         if End_Of_File (New_File) then
            Changed := True;
            exit;
         end if;

         declare
            Old_Line : constant String := Get_Line (Old_File);
            New_Line : constant String := Get_Line (New_File);
         begin
            if Old_Line /= New_Line then
               Changed := True;
               exit;
            end if;
         end;
      end loop;

      if not End_Of_File (New_File) then
         Changed := True;
      end if;
      Close (Old_File);
      Close (New_File);
      return Changed;
   end File_Changed;

   -------------------
   -- Get_Loop_Name --
   -------------------

   function Get_Loop_Name (Line : String) return String is
      Start : constant Positive :=
                Ada.Strings.Fixed.Index (Line, "{loop:");
      Finish : constant Positive :=
                 Ada.Strings.Fixed.Index (Line, "}", Start);
   begin
      return Line (Start + 6 .. Finish - 1);
   end Get_Loop_Name;

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
               Position : constant Cursor := Map.Simple_Map.Find (Key);
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

end Kit.Templates;
