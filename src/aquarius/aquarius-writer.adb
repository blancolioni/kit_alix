package body Aquarius.Writer is

   ------------
   -- Indent --
   ------------

   procedure Indent (Writer : in out Writer_Interface'Class) is
   begin
      Writer.Indent (Writer.Default_Indent);
   end Indent;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : in     String)
   is
   begin
      Writer.Put (Text);
      Writer.New_Line;
   end Put_Line;

end Aquarius.Writer;
