with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

package body Drys is

   use Ada.Strings.Unbounded;

   function "+" (Left : String)
                 return Unbounded_String
                 renames To_Unbounded_String;
   function "-" (Left : Unbounded_String)
                 return String
                 renames  To_String;

   function Package_File_Name
     (Item : Package_Declaration;
      Spec : Boolean)
      return String;

   function To_Ada_Name (Text : String) return String;
   function To_Ada_Name (Text : Unbounded_String) return String;

   function Single_Line_Statement (Line : String)
                                   return Statement;

   procedure Add_Line (To_Statement : in out Statement;
                       Line         : in     String);

   function Compound_Statement
     (Prefix, Suffix : String;
      Statement_Body : Statement)
      return Statement;

   procedure Create_Subprogram
     (Item        : in out Package_Declaration;
      Name        : in     String;
      Args        : in     Formal_Argument_List;
      Result      : in     String;
      Is_Abstract : in     Boolean;
      Item_Body   : in     Subprogram_Body);

   No_Body : Subprogram_Body;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line (To_Statement : in out Statement;
                       Line         : in     String)
   is
      use Ada.Strings.Unbounded;
   begin
      To_Statement.Lines.Append
        (To_Unbounded_String (Line));
   end Add_Line;

   --------------
   -- Argument --
   --------------

   function Argument
     (Name          : String;
      Type_Name     : String;
      Default_Value : String := "")
      return Subprogram_Formal_Argument
   is
      use Ada.Strings.Unbounded;
   begin
      return (Name         => To_Unbounded_String (Name),
              Mode         => In_Argument,
              Not_Null     => False,
              Acc_Constant => False,
              Arg_Type     => To_Unbounded_String (Type_Name),
              Arg_Default  => To_Unbounded_String (Default_Value));
   end Argument;

   --------------------------
   -- Assignment_Statement --
   --------------------------

   function Assignment_Statement
     (Dst, Src : String)
     return Statement
   is
   begin
      return (Simple,
              String_Vectors.To_Vector
                (To_Unbounded_String
                   (To_Ada_Name (Dst) & " := " & To_Ada_Name (Src) & ";"),
                 1));
   end Assignment_Statement;

   ------------------------
   -- Compound_Statement --
   ------------------------

   function Compound_Statement
     (Prefix, Suffix : String;
      Statement_Body : Statement)
      return Statement
   is
      use Ada.Strings.Unbounded;
   begin
      return S : Statement (Compound) do
         S.Prefix := To_Unbounded_String (Prefix);
         S.S_Body := new Statement'(Statement_Body);
         S.Suffix := To_Unbounded_String (Suffix);
      end return;
   end Compound_Statement;

   -----------------------
   -- Create_Subprogram --
   -----------------------

   procedure Create_Subprogram
     (Item        : in out Package_Declaration;
      Name        : in     String;
      Args        : in     Formal_Argument_List;
      Result      : in     String;
      Is_Abstract : in     Boolean;
      Item_Body   : in     Subprogram_Body)
   is
      Sub : Declaration (Subprogram_Declaration);
   begin
      Sub.Name := +Name;
      Sub.Arguments := new Formal_Argument_List'(Args);
      Sub.Return_Type := +Result;
      Sub.Is_Abstract := Is_Abstract;
      Sub.Sub_Body    := Item_Body;
      Item.Declarations.Append (Sub);
   end Create_Subprogram;

   --------------------
   -- Inout_Argument --
   --------------------

   function Inout_Argument
     (Name          : String;
      Type_Name     : String)
      return Subprogram_Formal_Argument
   is
      use Ada.Strings.Unbounded;
   begin
      return (Name         => To_Unbounded_String (Name),
              Mode         => Inout_Argument,
              Not_Null     => False,
              Acc_Constant => False,
              Arg_Type     => To_Unbounded_String (Type_Name),
              Arg_Default  => Null_Unbounded_String);
   end Inout_Argument;

   ------------------
   -- Out_Argument --
   ------------------

   function Out_Argument
     (Name          : String;
      Type_Name     : String)
      return Subprogram_Formal_Argument
   is
      use Ada.Strings.Unbounded;
   begin
      return (Name         => To_Unbounded_String (Name),
              Mode         => Out_Argument,
              Not_Null     => False,
              Acc_Constant => False,
              Arg_Type     => To_Unbounded_String (Type_Name),
              Arg_Default  => Null_Unbounded_String);
   end Out_Argument;

   ----------------------------
   -- Sequence_Of_Statements --
   ----------------------------

   function Sequence_Of_Statements
     (Statements : Statement_List)
     return Statement
   is
   begin
      return Result : Statement do
        for I in Statements'Range loop
           for J in 1 .. Statements (I).Lines.Last_Index loop
              Result.Lines.Append (Statements (I).Lines.Element (J));
           end loop;
        end loop;
      end return;
   end Sequence_Of_Statements;

   ---------------------------
   -- Single_Line_Statement --
   ---------------------------

   function Single_Line_Statement (Line : String)
                                   return Statement
   is
      use Ada.Strings.Unbounded;
   begin
      return Result : Statement do
         Result.Lines.Append
           (To_Unbounded_String
              (if Line (Line'Last) = ';'
               then Line
               else Line & ';'));
      end return;
   end Single_Line_Statement;

   ---------------------
   -- Access_Argument --
   ---------------------

   function Access_Argument
     (Name          : String;
      Type_Name     : String;
      Not_Null      : Boolean := True;
      Is_Constant   : Boolean := False)
      return Subprogram_Formal_Argument
   is
      use Ada.Strings.Unbounded;
   begin
      return (Name         => To_Unbounded_String (Name),
              Mode         => Access_Argument,
              Not_Null     => Not_Null,
              Acc_Constant => Is_Constant,
              Arg_Type     => To_Unbounded_String (Type_Name),
              Arg_Default  => Null_Unbounded_String);
   end Access_Argument;

   --------------------
   -- Null_Statement --
   --------------------

   function Null_Statement return Statement is
   begin
      return Single_Line_Statement ("null");
   end Null_Statement;

   ----------------------
   -- Return_Statement --
   ----------------------

   function Return_Statement
     (Expression : String)
      return Statement
   is
   begin
      return Single_Line_Statement ("return " & Expression);
   end Return_Statement;

   ----------------------
   -- Return_Statement --
   ----------------------

   function Return_Statement
     (Variable      : String;
      Variable_Type : String;
      Operations    : Statement)
      return Statement
   is
   begin
      return Compound_Statement
        (Prefix => "return " & To_Ada_Name (Variable) & " : " &
         To_Ada_Name (Variable_Type) & " do",
         Suffix => "end return",
         Statement_Body => Operations);
   end Return_Statement;

   ------------------------------
   -- Procedure_Call_Statement --
   ------------------------------

   function Procedure_Call_Statement
     (Name        : String;
      Argument_1  : String   := "";
      Argument_2  : String   := "";
      Argument_3  : String   := "";
      Argument_4  : String   := "";
      Argument_5  : String   := "")
      return Statement
   is
      use Ada.Strings.Unbounded;
      Call : Unbounded_String :=
               To_Unbounded_String (To_Ada_Name (Name));
   begin
      if Argument_1 /= "" then
         Call := Call & " (" & To_Ada_Name (Argument_1);
         if Argument_2 /= "" then
            Call := Call & ", " & To_Ada_Name (Argument_2);
         end if;
         if Argument_3 /= "" then
            Call := Call & ", " & To_Ada_Name (Argument_3);
         end if;
         if Argument_4 /= "" then
            Call := Call & ", " & To_Ada_Name (Argument_4);
         end if;
         if Argument_5 /= "" then
            Call := Call & ", " & To_Ada_Name (Argument_5);
         end if;
         Call := Call & ")";
      end if;
      return Single_Line_Statement (To_String (Call));
   end Procedure_Call_Statement;

   --------------------
   -- Statement_Body --
   --------------------

   function Statement_Body
     (Statements : Statement_List)
      return Subprogram_Body
   is
      Result : Subprogram_Body;
   begin
      for I in Statements'Range loop
         declare
            Item : Statement := Statements (I);
         begin
            for J in 1 .. Item.Lines.Last_Index loop
               Result.Statements.Lines.Append (Item.Lines.Element (J));
            end loop;
         end;
      end loop;

      return Result;

   end Statement_Body;

   --------------------
   -- Statement_Body --
   --------------------

   function Statement_Body
     (Single : Statement)
      return Subprogram_Body
   is
      Result : Subprogram_Body;
   begin
      Result.Statements := Single;
      return Result;
   end Statement_Body;

   -----------------
   -- New_Package --
   -----------------

   function New_Package (Name : String) return Package_Declaration is
   begin
      return Result : Package_Declaration do
         Result.Name :=  To_Unbounded_String (Name);
      end return;
   end New_Package;

   -----------------------
   -- New_Child_Package --
   -----------------------

   function New_Child_Package
     (Parent : Package_Declaration;
      Name   : String)
      return Package_Declaration
   is
      use Ada.Strings.Unbounded;
   begin
      return Result : Package_Declaration do
         Result.Name :=
           Parent.Name & '.' & To_Unbounded_String (Name);
      end return;
   end New_Child_Package;

   ----------------------
   -- Create_Interface --
   ----------------------

   procedure Create_Interface
     (Item    : in out Package_Declaration;
      Name    : in     String)
   is
      Result : Declaration (Interface_Declaration);
   begin
      Result.Name :=
         To_Unbounded_String (Name);
      Item.Declarations.Append (Result);
   end Create_Interface;

   -------------------------------
   -- Create_Class_Wide_Subtype --
   -------------------------------

   procedure Create_Class_Wide_Subtype
     (Item     : in out Package_Declaration;
      For_Type : in String;
      Name     : in String)
   is
      Result : Declaration (Subtype_Declaration);
   begin
      Result.Name :=
         To_Unbounded_String (Name);
      Result.Base_Type :=
         To_Unbounded_String (For_Type);
      Result.Class_Wide := True;
      Item.Declarations.Append (Result);
   end Create_Class_Wide_Subtype;

   -----------------------
   -- Abstract_Function --
   -----------------------

   procedure Abstract_Function
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Arg      : in     Subprogram_Formal_Argument;
      Result   : in     String)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => (1 => Arg),
         Result      => Result,
         Is_Abstract => True,
         Item_Body   => No_Body);
   end Abstract_Function;

   ------------------------
   -- Abstract_Procedure --
   ------------------------

   procedure Abstract_Procedure
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Arg      : in     Subprogram_Formal_Argument)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => (1 => Arg),
         Result      => "",
         Is_Abstract => True,
         Item_Body   => No_Body);
   end Abstract_Procedure;

   -----------------------
   -- Abstract_Function --
   -----------------------

   procedure Abstract_Function
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Args     : in     Formal_Argument_List;
      Result   : in     String)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => Args,
         Result      => Result,
         Is_Abstract => True,
         Item_Body   => No_Body);
   end Abstract_Function;

   ------------------------
   -- Abstract_Procedure --
   ------------------------

   procedure Abstract_Procedure
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Args     : in     Formal_Argument_List)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => Args,
         Result      => "",
         Is_Abstract => True,
         Item_Body   => No_Body);
   end Abstract_Procedure;

   ---------------------
   -- Create_Function --
   ---------------------

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Result    : in     String;
      Item_Body : in     Subprogram_Body)
   is
      Args : Formal_Argument_List (1 .. 0);
   begin
      Create_Function (Item, Name, Args, Result, Item_Body);
   end Create_Function;

   ---------------------
   -- Create_Function --
   ---------------------

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Arg       : in     Subprogram_Formal_Argument;
      Result    : in     String;
      Item_Body : in     Subprogram_Body)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => (1 => Arg),
         Result      => Result,
         Is_Abstract => False,
         Item_Body   => Item_Body);
   end Create_Function;

   ----------------------
   -- Create_Procedure --
   ----------------------

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Item_Body : in     Subprogram_Body)
   is
      Args : Formal_Argument_List (1 .. 0);
   begin
      Create_Procedure (Item, Name, Args, Item_Body);
   end Create_Procedure;

   ----------------------
   -- Create_Procedure --
   ----------------------

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Arg       : in     Subprogram_Formal_Argument;
      Item_Body : in     Subprogram_Body)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => (1 => Arg),
         Result      => "",
         Is_Abstract => False,
         Item_Body   => Item_Body);
   end Create_Procedure;

   ---------------------
   -- Create_Function --
   ---------------------

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Args      : in     Formal_Argument_List;
      Result    : in     String;
      Item_Body : in     Subprogram_Body)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => Args,
         Result      => Result,
         Is_Abstract => False,
         Item_Body   => Item_Body);
   end Create_Function;

   ----------------------
   -- Create_Procedure --
   ----------------------

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Args      : in     Formal_Argument_List;
      Item_Body : in     Subprogram_Body)
   is
   begin
      Create_Subprogram
        (Item        => Item,
         Name        => Name,
         Args        => Args,
         Result      => "",
         Is_Abstract => False,
         Item_Body   => Item_Body);
   end Create_Procedure;

   -----------------------
   -- Package_File_Name --
   -----------------------

   function Package_File_Name
     (Item : Package_Declaration;
      Spec : Boolean)
      return String
   is
      use Ada.Characters.Handling;
      Result : String := -Item.Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;

      if Spec then
         return Result & ".ads";
      else
         return Result & ".adb";
      end if;
   end Package_File_Name;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (Text : String) return String is
      use Ada.Characters.Handling;
      Result : String := Text;
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
   end To_Ada_Name;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (Text : Unbounded_String) return String is
   begin
      return To_Ada_Name (-Text);
   end To_Ada_Name;

   -----------
   -- Write --
   -----------

   procedure Write (Item : Package_Declaration;
                    Directory : String)
   is
      use Ada.Text_IO;
      File : File_Type;

      procedure Write (Dec  : Declaration;
                       Spec : Boolean);

      procedure Write (Sub  : Subprogram_Body);
      procedure Write (Stat   : Statement;
                       Indent : Positive);

      -----------
      -- Write --
      -----------

      procedure Write (Sub  : Subprogram_Body) is
      begin
         Put_Line (File, "   begin");
         Write (Sub.Statements, 6);
      end Write;

      -----------
      -- Write --
      -----------

      procedure Write (Stat   : Statement;
                       Indent : Positive)
      is
      begin
         case Stat.S_Type is
            when Simple =>
               for I in 1 .. Stat.Lines.Last_Index loop
                  Set_Col (File, Ada.Text_IO.Count (Indent));
                  Put_Line (File, -Stat.Lines.Element (I));
               end loop;
            when Compound =>
               Set_Col (File, Ada.Text_IO.Count (Indent));
               Put_Line (File, -Stat.Prefix);
               Write (Stat.S_Body.all, Indent + 3);
               Set_Col (File, Ada.Text_IO.Count (Indent));
               Put_Line (File, -Stat.Suffix);
         end case;
      end Write;

      -----------
      -- Write --
      -----------

      procedure Write (Dec  : Declaration;
                       Spec : Boolean)
      is
      begin
         case Dec.Decl_Type is
            when Empty_Declaration =>
               null;
            when Interface_Declaration =>
               if Spec then
                  Put_Line (File,
                            "   type " &
                            To_Ada_Name (-Dec.Name) &
                            " is interface;");
                  New_Line (File);
               end if;
            when Type_Declaration =>
               null;

            when Subtype_Declaration =>
               if Spec then
                  Put (File,
                       "   subtype " &
                       To_Ada_Name (-Dec.Name) &
                       " is " &
                       To_Ada_Name (-Dec.Base_Type));
                  if Dec.Class_Wide then
                     Put (File, "'Class");
                  end if;
                  Put_Line (File, ";");
                  New_Line (File);
               end if;

            when Subprogram_Declaration =>

               if Spec or else not Dec.Is_Abstract then

                  if not Spec then
                     declare
                        Name_Box : String :=
                                     "-- " & To_Ada_Name (Dec.Name) & " --";
                        Dashes : String (Name_Box'Range) :=
                                   (others => '-');
                     begin
                        Put_Line (File, "   " & Dashes);
                        Put_Line (File, "   " & Name_Box);
                        Put_Line (File, "   " & Dashes);
                        New_Line (File);
                     end;
                  end if;

                  if Dec.Return_Type /= "" then
                     Put (File, "   function");
                  else
                     Put (File, "   procedure");
                  end if;

                  Put (File,
                       " " &
                       To_Ada_Name (-Dec.Name));
                  if Dec.Arguments'Length > 0 then
                     New_Line (File);
                     declare
                        Longest_Name : Natural := 0;
                        This_Length  : Natural;
                     begin
                        for I in Dec.Arguments'Range loop
                           This_Length := Length (Dec.Arguments (I).Name);
                           if This_Length > Longest_Name then
                              Longest_Name := This_Length;
                           end if;
                        end loop;

                        for I in Dec.Arguments'Range loop
                           if I = Dec.Arguments'First then
                              Put (File, "     (");
                           else
                              Put (File, "      ");
                           end if;
                           Put (File, To_Ada_Name (Dec.Arguments (I).Name));
                           Set_Col (File, 6 + Ada.Text_IO.Count (Longest_Name) + 2);
                           Put (File, ": ");
                           case Dec.Arguments (I).Mode is
                              when In_Argument =>
                                 Put (File, "in     ");
                              when Out_Argument =>
                                 Put (File, "   out ");
                              when Inout_Argument =>
                                 Put (File, "in out ");
                              when Access_Argument =>
                                 if Dec.Arguments (I).Not_Null then
                                    Put (File, "not null ");
                                 end if;
                                 Put (File, "access ");
                                 if Dec.Arguments (I).Acc_Constant then
                                    Put (File, "constant ");
                                 end if;
                           end case;
                           Put (File,
                                To_Ada_Name (Dec.Arguments (I).Arg_Type));
                           if I < Dec.Arguments'Last then
                              Put_Line (File, ";");
                           else
                              Put (File, ")");
                           end if;
                        end loop;
                     end;
                  end if;

                  if Dec.Return_Type /= Null_Unbounded_String then
                     if Dec.Arguments'Length > 0 then
                        New_Line (File);
                        Put (File, "     ");
                     end if;
                     Put (File,
                          " return " & To_Ada_Name (Dec.Return_Type));
                  end if;

                  if Spec then
                     if Dec.Is_Abstract then
                  	if Dec.Arguments'Length > 0 then
                           New_Line (File);
                           Put_Line (File, "      is abstract;");
                        else
                           Put_Line (File, " is abstract;");
                        end if;
                     else
                        Put_Line (File, ";");
                     end if;
                  elsif Dec.Arguments'Length > 0 then
                     New_Line (File);
                     Put_Line (File, "   is");
                  else
                     Put_Line (File, " is");
                  end if;

                  if not Spec then
                     Write (Dec.Sub_Body);
                     Put_Line (File,
                               "   end " & To_Ada_Name (Dec.Name) & ";");
                  end if;

                  New_Line (File);

               end if;

            when others =>
               null;
         end case;
      end Write;

   begin
      for Is_Spec in reverse Boolean loop
         declare
            Is_Body : constant Boolean := not Is_Spec;
            Path : constant String :=
                     Ada.Directories.Compose (Directory,
                                              Package_File_Name (Item, Is_Spec));
         begin
            Create (File, Out_File, Path);

            Put_Line (File,
                      "package " &
                      (if Is_Body then "body " else "") &
                      To_Ada_Name (-Item.Name) &
                      " is");
            New_Line (File);

            for I in 1 .. Item.Declarations.Last_Index loop

               Write (Item.Declarations.Element (I), Is_Spec);

            end loop;

            Put_Line (File,
                      "end "
                      & To_Ada_Name (-Item.Name)
                      & ";");
            Close (File);
         end;
      end loop;
   end Write;

end Drys;
