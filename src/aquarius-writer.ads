package Aquarius.Writer is

   type Writer_Interface is abstract tagged limited private;

   procedure Create (Writer : in out Writer_Interface;
                     Path   : in     String)
      is abstract;

   procedure Close (Writer : in out Writer_Interface)
      is abstract;

   procedure Put (Writer : in out Writer_Interface;
                  Text   : in     String)
      is abstract;

   procedure Put_Line (Writer : in out Writer_Interface'Class;
                       Text   : in     String);

   procedure New_Line (Writer : in out Writer_Interface)
      is abstract;

   function Col (Writer : Writer_Interface) return Positive
      is abstract;

   procedure Set_Col (Writer : in out Writer_Interface;
                      Value  : in     Positive)
     is abstract;

   function Indent (Writer : Writer_Interface) return Natural
      is abstract;

   procedure Indent (Writer : in out Writer_Interface;
                     Value  : in     Natural)
      is abstract;

   procedure Optional_New_Line
     (Writer : in out Writer_Interface)
   is null;

   procedure Indent (Writer : in out Writer_Interface'Class);

private

   type Writer_Interface is abstract tagged limited
      record
         Default_Indent : Positive := 3;
      end record;

end Aquarius.Writer;
