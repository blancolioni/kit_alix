with Ada.Directories;
with Ada.Strings.Fixed;

package body Aquarius.Writer.File_Writer is

   Right_Margin : constant := 78;

   function Temporary_File
     (Path : String)
      return String;

   function File_Changed
     (Original_File_Path : String;
      New_File_Path      : String)
      return Boolean;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (Item : in out File_Writer) is
   begin
      Item.Flush;
      Ada.Text_IO.Close (Item.File);

      if File_Changed (Item.Path.all, Item.Temp_Path.all) then
         Ada.Directories.Delete_File (Item.Path.all);
         Ada.Directories.Rename (Item.Temp_Path.all, Item.Path.all);
      end if;

   end Close;

   ---------
   -- Col --
   ---------

   overriding
   function Col (Writer : File_Writer) return Positive is
   begin
      return Writer.Line_Length + 1;
   end Col;

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (Item : in out File_Writer;
      Path : in     String)
   is
   begin
      Item.Path := new String'(Path);
      Item.Temp_Path := new String'(Temporary_File (Path));
      Ada.Text_IO.Create (Item.File, Ada.Text_IO.Out_File, Item.Temp_Path.all);
      Item.Line_Length := 0;
      Item.Optional_NL := 0;
   end Create;

   -----------
   -- Flush --
   -----------

   procedure Flush (Writer : in out File_Writer) is
   begin
      if Writer.Line_Length > 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Flush;

   ------------
   -- Indent --
   ------------

   overriding
   function Indent (Writer : File_Writer) return Natural is
   begin
      return Writer.Indent;
   end Indent;

   ------------
   -- Indent --
   ------------

   overriding
   procedure Indent (Writer : in out File_Writer;
                     Value  : in     Natural)
   is
   begin
      Writer.Indent := Value;
   end Indent;

   --------------
   -- New_Line --
   --------------

   overriding
   procedure New_Line (Writer : in out File_Writer) is
   begin
      if Writer.First_On_Line
        and then Writer.Have_Empty_Line
      then
         return;
      end if;

      if Writer.First_On_Line then
         Writer.Have_Empty_Line := True;
      end if;

      Ada.Text_IO.Put_Line (Writer.File,
                            Writer.Current_Line (1 .. Writer.Line_Length));
      Writer.Line_Length := 0;
      Writer.Optional_NL := 0;
      Writer.First_On_Line := True;

   end New_Line;

   -----------------------
   -- Optional_New_Line --
   -----------------------

   overriding
   procedure Optional_New_Line
     (Writer : in out File_Writer)
   is
   begin
      Writer.Optional_NL := Writer.Col;
   end Optional_New_Line;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put
     (Writer : in out File_Writer;
      Text   : in     String)
   is
      Trimmed : constant String :=
                  Ada.Strings.Fixed.Trim (Text,
                                          Ada.Strings.Right);
   begin
      if Writer.First_On_Line
        and then Writer.Indent > 1
      then
         Writer.Current_Line (1 .. Writer.Indent) :=
         (others => ' ');
         Writer.Line_Length := Writer.Indent;
      end if;

      if Trimmed'Length > 0
        and then Writer.Line_Length + Writer.Pending_Spaces + Trimmed'Length
          >= Right_Margin
        and then Writer.Optional_NL > 0
      then
         declare
            Flushed_Length : constant Natural := Writer.Optional_NL - 1;
            Kept_Start     : constant Natural := Writer.Optional_NL;
            Flushed_Part : constant String :=
                             Writer.Current_Line (1 .. Flushed_Length);
            Kept_Part    : constant String :=
                             Writer.Current_Line
                               (Kept_Start .. Writer.Line_Length);
            Next_Start   : constant String (1 .. Writer.Indent + 2) :=
                             (others => ' ');
            Next_Line    : constant String :=
                             Next_Start & Kept_Part;

         begin
            Ada.Text_IO.Put_Line (Writer.File, Flushed_Part);
            Writer.Current_Line (1 .. Next_Line'Length) := Next_Line;
            Writer.Line_Length := Next_Line'Length;
            Writer.Optional_NL := 0;
         end;
      end if;

      if not Writer.First_On_Line
        and then Writer.Pending_Spaces > 0
      then
         Writer.Current_Line (Writer.Line_Length + 1 ..
                                Writer.Line_Length + Writer.Pending_Spaces) :=
             (others => ' ');
         Writer.Line_Length := Writer.Line_Length + Writer.Pending_Spaces;
         Writer.Pending_Spaces := 0;
      end if;

      Writer.First_On_Line := False;

      Writer.Pending_Spaces := Text'Length - Trimmed'Length;
      Writer.Current_Line (Writer.Line_Length + 1 ..
                             Writer.Line_Length + Trimmed'Length) :=
          Trimmed;
      Writer.Line_Length := Writer.Line_Length + Trimmed'Length;

      Writer.Have_Empty_Line :=
        Writer.Have_Empty_Line and then Trimmed'Length = 0;

   end Put;

   -------------
   -- Set_Col --
   -------------

   overriding
   procedure Set_Col (Writer : in out File_Writer;
                      Value  : in     Positive)
   is
   begin
      while Writer.Line_Length < Value - 1 loop
         Writer.Line_Length := Writer.Line_Length + 1;
         Writer.Current_Line (Writer.Line_Length) := ' ';
      end loop;
   end Set_Col;

end Aquarius.Writer.File_Writer;
