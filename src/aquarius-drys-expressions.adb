package body Aquarius.Drys.Expressions is

   type Operator_Expression is
     new Expression with
      record
         Operator_Name : access String;
         Left, Right   : access Expression'Class;
         Long          : Boolean := False;
      end record;

   overriding
   procedure Write (Item   : Operator_Expression;
                    Writer : in out Writer_Interface'Class);

   -------------------------
   -- Add_Actual_Argument --
   -------------------------

   procedure Add_Actual_Argument
     (Call      : in out Function_Call_Expression;
      Name      : in     String;
      Value     : in     Expression'Class)
   is
   begin
      if Name /= "" then
         Call.Arguments.Append
           ((new String'(Name),
            new Expression'Class'(Value)));
      else
         Call.Arguments.Append
           ((null,
            new Expression'Class'(Value)));
      end if;
   end Add_Actual_Argument;

   -------------------------
   -- Add_Actual_Argument --
   -------------------------

   procedure Add_Actual_Argument
     (Call      : in out Function_Call_Expression;
      Value     : in     Expression'Class)
   is
   begin
      Add_Actual_Argument (Call, "", Value);
   end Add_Actual_Argument;

   function Long_Operator (Name    : String;
                           Left    : Expression'Class;
                           Right   : Expression'Class)
                           return Expression'Class
   is

   begin
      return Result : Operator_Expression do
         Result.Operator_Name := new String'(Name);
         Result.Left          := new Expression'Class'(Left);
         Result.Right         := new Expression'Class'(Right);
         Result.Long := True;
      end return;
   end Long_Operator;

   -------------------------------
   -- New_Allocation_Expression --
   -------------------------------

   function New_Allocation_Expression
     (Allocated_Type : String)
      return Allocation_Expression
   is
   begin
      return Result : Allocation_Expression do
         Result.Allocated_Type := new String'(Allocated_Type);
      end return;
   end New_Allocation_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String)
      return Function_Call_Expression
   is
   begin
      return F : Function_Call_Expression do
         F.Name := new String'(Procedure_Name);
      end return;
   end New_Function_Call_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String;
      Argument       : Expression'Class)
      return Function_Call_Expression
   is
   begin
      return F : Function_Call_Expression do
         F.Name := new String'(Procedure_Name);
         F.Add_Actual_Argument (Argument);
      end return;
   end New_Function_Call_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class)
      return Function_Call_Expression
   is
   begin
      return F : Function_Call_Expression do
         F.Name := new String'(Procedure_Name);
         F.Add_Actual_Argument (Argument_1);
         F.Add_Actual_Argument (Argument_2);
      end return;
   end New_Function_Call_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String;
      Argument       : String)
      return Function_Call_Expression
   is
   begin
      return New_Function_Call_Expression (Procedure_Name,
                                           Object (Argument));
   end New_Function_Call_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String;
      Argument_1     : String;
      Argument_2     : String)
      return Function_Call_Expression
   is
   begin
      return F : Function_Call_Expression do
         F.Name := new String'(Procedure_Name);
         F.Add_Actual_Argument (Object (Argument_1));
         F.Add_Actual_Argument (Object (Argument_2));
      end return;
   end New_Function_Call_Expression;

   ----------------------------------
   -- New_Function_Call_Expression --
   ----------------------------------

   function New_Function_Call_Expression
     (Procedure_Name : String;
      Argument_1     : String;
      Argument_2     : String;
      Argument_3     : String)
      return Function_Call_Expression
   is
   begin
      return F : Function_Call_Expression do
         F.Name := new String'(Procedure_Name);
         F.Add_Actual_Argument (Object (Argument_1));
         F.Add_Actual_Argument (Object (Argument_2));
         F.Add_Actual_Argument (Object (Argument_3));
      end return;
   end New_Function_Call_Expression;

   --------------
   -- Operator --
   --------------

   function Operator (Name    : String;
                      Left    : Expression'Class;
                      Right   : Expression'Class)
                      return Expression'Class
   is

   begin
      return Result : Operator_Expression do
         Result.Operator_Name := new String'(Name);
         Result.Left          := new Expression'Class'(Left);
         Result.Right         := new Expression'Class'(Right);
      end return;
   end Operator;

   --------------
   -- Operator --
   --------------

   function Operator (Name    : String;
                      Left    : Expression'Class)
                      return Expression'Class
   is

   begin
      return Result : Operator_Expression do
         Result.Operator_Name := new String'(Name);
         Result.Left          := new Expression'Class'(Left);
         Result.Right         := null;
      end return;
   end Operator;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Allocation_Expression;
                    Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("new " & Item.Allocated_Type.all);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Item   : Function_Call_Expression;
      Writer : in out Writer_Interface'Class)
   is
      First : Boolean := True;
   begin
      Writer.Put (Item.Name.all);
      Writer.Optional_New_Line;
      for Arg of Item.Arguments loop
         if First then
            Writer.Put (" (");
            First := False;
         else
            Writer.Put (", ");
            Writer.Optional_New_Line;
         end if;

         if Arg.Name /= null then
            Writer.Put (Arg.Name.all & " => ");
         end if;

         Arg.Value.Write (Writer);
      end loop;

      if not First then
         Writer.Put (")");
      end if;

   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Operator_Expression;
                    Writer : in out Writer_Interface'Class)
   is
      Paren_Left : constant Boolean :=
                     Item.Left.all in Operator_Expression'Class;
      Paren_Right : constant Boolean :=
                      Item.Right /= null
                      and then Item.Right.all in Operator_Expression'Class;
   begin
      if Item.Right /= null then
         if Paren_Left then
            Writer.Put ("(");
         end if;
         Item.Left.Write (Writer);
         if Paren_Left then
            Writer.Put (")");
         end if;
         if Item.Long then
            Writer.New_Line;
         else
            Writer.Put (" ");
         end if;
         Writer.Optional_New_Line;
         Writer.Put (Item.Operator_Name.all & " ");
         if Paren_Right then
            Writer.Put ("(");
         end if;
         Item.Right.Write (Writer);
         if Paren_Right then
            Writer.Put (")");
         end if;
      else
         Writer.Put (Item.Operator_Name.all & " ");
         Item.Left.Write (Writer);
      end if;
   end Write;

end Aquarius.Drys.Expressions;
