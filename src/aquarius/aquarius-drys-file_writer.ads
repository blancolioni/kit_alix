with Ada.Text_IO;

package Aquarius.Drys.File_Writer is

   type File_Writer is limited new Writer_Interface with private;

   overriding
   procedure Create (Item : in out File_Writer;
                     Path : in     String);

   overriding
   procedure Close (Item : in out File_Writer);

   overriding
   procedure Put (Writer : in out File_Writer;
                  Text   : in     String);

   overriding
   procedure New_Line (Writer : in out File_Writer);

   overriding
   function Col (Writer : File_Writer) return Positive;

   overriding
   function Line (Writer : File_Writer) return Positive;

   overriding
   procedure Set_Col (Writer : in out File_Writer;
                      Value  : in     Positive);

   overriding
   function Indent (Writer : File_Writer) return Natural;

   overriding
   procedure Indent (Writer : in out File_Writer;
                     Value  : in     Natural);

   overriding
   procedure Optional_New_Line
     (Writer : in out File_Writer);

private

   type File_Writer is
   limited new Writer_Interface with
      record
         Path            : access String;
         Temp_Path       : access String;
         Indent          : Natural := 0;
         First_On_Line   : Boolean := True;
         Have_Empty_Line : Boolean := False;
         File            : Ada.Text_IO.File_Type;
         Pending_Spaces  : Natural := 0;
         Current_Line    : String (1 .. 200);
         Line_Length     : Natural                 := 0;
         Optional_NL     : Natural                 := 0;
      end record;

   procedure Flush (Writer : in out File_Writer);

end Aquarius.Drys.File_Writer;
