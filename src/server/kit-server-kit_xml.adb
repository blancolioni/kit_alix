with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Kit.Server.Kit_XML is

   type XML_Export_Type is
     new Kit.Server.Export.Root_Exporter with
      record
         Target : Ada.Strings.Unbounded.Unbounded_String;
         File   : Ada.Text_IO.File_Type;
         Table  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   procedure Start_Export (Exporter : in out XML_Export_Type;
                           Name     : String);

   overriding
   procedure End_Export (Exporter : in out XML_Export_Type);

   overriding procedure Start_Table (Exporter : in out XML_Export_Type;
                                     Name     : String);

   overriding procedure End_Table (Exporter : in out XML_Export_Type);

   overriding procedure Base_Table (Exporter  : in out XML_Export_Type;
                                    Base_Name : in     String)
   is null;

   overriding procedure Field (Exporter   : in out XML_Export_Type;
                               Name       : in String;
                               Field_Type : in String);

   overriding procedure Field (Exporter   : in out XML_Export_Type;
                               Name       : in String;
                               Base       : in String;
                               Field_Type : in String)
   is null;

   overriding procedure Start_Key (Exporter    : in out XML_Export_Type;
                        Name        : in String;
                        Unique      : in Boolean)
   is null;

   overriding procedure End_Key (Exporter : in out XML_Export_Type)
   is null;

   overriding procedure Key_Field (Exporter : in out XML_Export_Type;
                        Field_Name : in String)
   is null;

   overriding procedure Start_Record (Exporter    : in out XML_Export_Type)
   is null;

   overriding procedure Record_Field (Exporter    : in out XML_Export_Type;
                                      Field_Name  : in     String;
                                      Field_Value : in     String)
   is null;

   overriding procedure End_Record (Exporter    : in out XML_Export_Type)
   is null;

   ----------------
   -- End_Export --
   ----------------

   overriding procedure End_Export (Exporter : in out XML_Export_Type) is
   begin
      Ada.Text_IO.Put_Line (Exporter.File, "</db>");
      Ada.Text_IO.Close (Exporter.File);
   end End_Export;

   ---------------
   -- End_Table --
   ---------------

   overriding procedure End_Table (Exporter : in out XML_Export_Type) is
   begin
      Ada.Text_IO.Put_Line (Exporter.File, "</table>");
   end End_Table;

   -----------
   -- Field --
   -----------

   overriding procedure Field (Exporter   : in out XML_Export_Type;
                               Name       : in String;
                               Field_Type : in String)
   is
   begin
      Ada.Text_IO.Put_Line
        (File => Exporter.File,
         Item => "<column name=""" & Name
         & """ type=""" & Field_Type & """/>");
   end Field;

   ------------------
   -- Start_Export --
   ------------------

   overriding procedure Start_Export (Exporter : in out XML_Export_Type;
                                      Name     : String)
   is
      use Ada.Text_IO, Ada.Directories, Ada.Strings.Unbounded;
   begin
      Create (Exporter.File, Out_File,
              Compose (To_String (Exporter.Target), Name & ".xml"));
      Put_Line (Exporter.File,
                "<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line (Exporter.File,
                "<!-- Created by Kit.XML on "
                & Ada.Calendar.Formatting.Image (Ada.Calendar.Clock)
                & " -->");
      Put_Line (Exporter.File, "<db name=""" & Name & """>");
   end Start_Export;

   -----------------
   -- Start_Table --
   -----------------

   overriding procedure Start_Table (Exporter : in out XML_Export_Type;
                                     Name     : String)
   is
   begin
      Ada.Text_IO.Put_Line (Exporter.File,
                            "<table name=""" & Name & """>");
      Exporter.Table :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Start_Table;

   ------------------
   -- XML_Exporter --
   ------------------

   function XML_Exporter
     (Target_Directory : String)
      return Kit.Server.Export.Root_Exporter'Class
   is
   begin
      return Result : XML_Export_Type do
         Result.Target :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Target_Directory);
      end return;
   end XML_Exporter;

end Kit.Server.Kit_XML;
