package body Aquarius.Drys.Statements is

   type Loop_Statement_Record is
     new Statement with
      record
         Loop_Body : Sequence_Of_Statements;
      end record;

   overriding
   procedure Write (Item        : Loop_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

   type While_Statement_Record is
     new Loop_Statement_Record with
      record
         Condition : access Expression'Class;
      end record;

   overriding
   procedure Write (Item        : While_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

   type Iterate_Statement_Record is
     new Loop_Statement_Record with
      record
         Loop_Variable  : access String;
         Container_Name : access String;
      end record;

   overriding
   procedure Write (Item        : Iterate_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

   type Declare_Statement_Record is
     new Statement with
      record
         Block      : access Aquarius.Drys.Blocks.Block_Type'Class;
      end record;

   overriding
   procedure Write (Item        : Declare_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

   type Raise_Statement_Record is
     new Statement with
      record
         Exception_Name    : access String;
         Exception_Message : access String;
      end record;

   overriding
   procedure Write (Item        : Raise_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

   type Return_Statement is new Statement with
      record
         Expr : access Expression'Class;
      end record;

   overriding
   procedure Write (Item   : Return_Statement;
                    Writer : in out Writer_Interface'Class);

   type Extended_Return_Statement is new Statement with
      record
         Return_Variable : access String;
         Return_Type     : access String;
         Return_Sequence : Sequence_Of_Statements;
      end record;

   overriding
   procedure Write (Item   : Extended_Return_Statement;
                    Writer : in out Writer_Interface'Class);

   -------------------------
   -- Add_Actual_Argument --
   -------------------------

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
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
     (Call      : in out Procedure_Call_Statement;
      Value     : in     Expression'Class)
   is
   begin
      Add_Actual_Argument (Call, "", Value);
   end Add_Actual_Argument;

   -------------------------
   -- Add_Actual_Argument --
   -------------------------

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Value     : in     String)
   is
   begin
      Add_Actual_Argument (Call, Object (Value));
   end Add_Actual_Argument;

   ---------------------
   -- Add_Case_Option --
   ---------------------

   procedure Add_Case_Option
     (Statement : in out Case_Statement_Record'Class;
      Value     : in     String;
      Stats     : in     Sequence_Of_Statements'Class)
   is
   begin
      Statement.Case_Options.Append
        ((new Expression'Class'(Object (Value)),
         Sequence_Of_Statements (Stats)));
   end Add_Case_Option;

   ---------------------
   -- Add_Case_Option --
   ---------------------

   procedure Add_Case_Option
     (To_Case : in out Case_Statement_Record'Class;
      Value   : in     String;
      Stat    : in     Statement'Class)
   is
      Seq : Sequence_Of_Statements;
   begin
      Seq.Append (Stat);
      To_Case.Add_Case_Option (Value, Seq);
   end Add_Case_Option;

   ---------------
   -- Add_Elsif --
   ---------------

   procedure Add_Elsif
     (To_Statement    : in out If_Statement_Record'Class;
      Condition       : in     Expression'Class;
      Elsif_Statement : in     Statement'Class)
   is
      Sequence : Sequence_Of_Statements;
   begin
      Sequence.Append (Elsif_Statement);
      To_Statement.Elsifs.Append
        ((new Expression'Class'(Condition),
         Sequence));
   end Add_Elsif;

   ---------------
   -- Add_Elsif --
   ---------------

   procedure Add_Elsif
     (To_Statement     : in out If_Statement_Record'Class;
      Condition        : in     Expression'Class;
      Elsif_Statements : in     Sequence_Of_Statements)
   is
   begin
      To_Statement.Elsifs.Append
        ((new Expression'Class'(Condition),
         Elsif_Statements));
   end Add_Elsif;

   -----------------------
   -- Add_Others_Option --
   -----------------------

   procedure Add_Others_Option
     (Statement : in out Case_Statement_Record'Class;
      Stats     : in     Sequence_Of_Statements'Class)
   is
   begin
      Statement.Case_Options.Append
        ((null,
         Sequence_Of_Statements (Stats)));
   end Add_Others_Option;

   ------------
   -- Append --
   ------------

   overriding
   procedure Append (To : in out Sequence_Of_Statements;
                     S  : in     Statement'Class)
   is
   begin
      To.Sequence.Append (S);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (To : in out Sequence_Of_Statements;
                     S  : in     String)
   is
   begin
      To.Append (New_Procedure_Call_Statement (S));
   end Append;

   --------------------
   -- Case_Statement --
   --------------------

   function Case_Statement (Case_Expression : String)
                            return Case_Statement_Record'Class
   is
   begin
      return Result : Case_Statement_Record do
         Result.Case_Expression :=
           new Expression'Class'(Object (Case_Expression));
      end return;
   end Case_Statement;

   -----------------------
   -- Declare_Statement --
   -----------------------

   function Declare_Statement
     (Block : Aquarius.Drys.Blocks.Block_Type'Class)
      return Statement'Class
   is
   begin
      return Result : Declare_Statement_Record do
         Result.Block := new Aquarius.Drys.Blocks.Block_Type'Class'(Block);
      end return;
   end Declare_Statement;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements;
      False_Part : Sequence_Of_Statements)
      return If_Statement_Record'Class
   is
   begin
      return Result : If_Statement_Record do
         Result.Condition := new Expression'Class'(Condition);
         Result.True_Part := True_Part;
         Result.False_Part := False_Part;
      end return;
   end If_Statement;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements)
      return If_Statement_Record'Class
   is
   begin
      return Result : If_Statement_Record do
         Result.Condition := new Expression'Class'(Condition);
         Result.True_Part := True_Part;
      end return;
   end If_Statement;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Statement'Class)
      return If_Statement_Record'Class
   is
      Sequence : Sequence_Of_Statements;
   begin
      Sequence.Append (True_Part);
      return If_Statement (Condition, Sequence);
   end If_Statement;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Statement'Class;
      False_Part : Statement'Class)
      return If_Statement_Record'Class
   is
      True_Sequence  : Sequence_Of_Statements;
      False_Sequence : Sequence_Of_Statements;
   begin
      True_Sequence.Append (True_Part);
      False_Sequence.Append (False_Part);
      return If_Statement (Condition, True_Sequence, False_Sequence);
   end If_Statement;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Loop_Variable  : String;
      Container_Name : String;
      Iterate_Body   : Sequence_Of_Statements'Class)
      return Statement'Class
   is
   begin
      return Result : Iterate_Statement_Record do
         Result.Loop_Variable := new String'(Loop_Variable);
         Result.Container_Name := new String'(Container_Name);
         Result.Loop_Body := Sequence_Of_Statements (Iterate_Body);
      end return;
   end Iterate;

   ------------------------------
   -- New_Assignment_Statement --
   ------------------------------

   function New_Assignment_Statement
     (Target : String;
      Value  : Expression'Class)
      return Statement'Class
   is
   begin
      return R : Assignment_Statement do
         R.Target := new String'(Target);
         R.Expr  := new Expression'Class'(Value);
      end return;
   end New_Assignment_Statement;

   ----------------------------------
   -- New_Procedure_Call_Statement --
   ----------------------------------

   function New_Procedure_Call_Statement
     (Procedure_Name : String)
      return Procedure_Call_Statement
   is
   begin
      return P : Procedure_Call_Statement do
         P.Name := new String'(Procedure_Name);
      end return;
   end New_Procedure_Call_Statement;

   ----------------------------------
   -- New_Procedure_Call_Statement --
   ----------------------------------

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument       : Expression'Class)
      return Procedure_Call_Statement
   is
   begin
      return P : Procedure_Call_Statement do
         P.Name := new String'(Procedure_Name);
         P.Add_Actual_Argument (Argument);
      end return;
   end New_Procedure_Call_Statement;

   ----------------------------------
   -- New_Procedure_Call_Statement --
   ----------------------------------

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class)
      return Procedure_Call_Statement
   is
   begin
      return P : Procedure_Call_Statement do
         P.Name := new String'(Procedure_Name);
         P.Add_Actual_Argument (Argument_1);
         P.Add_Actual_Argument (Argument_2);
      end return;
   end New_Procedure_Call_Statement;

   ----------------------------------
   -- New_Procedure_Call_Statement --
   ----------------------------------

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class;
      Argument_3     : Expression'Class)
      return Procedure_Call_Statement
   is
   begin
      return P : Procedure_Call_Statement do
         P.Name := new String'(Procedure_Name);
         P.Add_Actual_Argument (Argument_1);
         P.Add_Actual_Argument (Argument_2);
         P.Add_Actual_Argument (Argument_3);
      end return;
   end New_Procedure_Call_Statement;

   --------------------------
   -- New_Return_Statement --
   --------------------------

   function New_Return_Statement
      return Statement'Class
   is
   begin
      return R : Return_Statement do
         R.Expr := null;
      end return;
   end New_Return_Statement;

   --------------------------
   -- New_Return_Statement --
   --------------------------

   function New_Return_Statement
     (Result : Expression'Class)
      return Statement'Class
   is
   begin
      return R : Return_Statement do
         R.Expr := new Expression'Class'(Result);
      end return;
   end New_Return_Statement;

   --------------------------
   -- New_Return_Statement --
   --------------------------

   function New_Return_Statement
     (Return_Variable   : String;
      Variable_Type     : String;
      Return_Statements : Sequence_Of_Statements'Class)
      return Statement'Class
   is
   begin
      return R : Extended_Return_Statement do
         R.Return_Variable := new String'(Return_Variable);
         R.Return_Type := new String'(Variable_Type);
         R.Return_Sequence :=
           Sequence_Of_Statements (Return_Statements);
      end return;
   end New_Return_Statement;

   ---------------------
   -- Raise_Statement --
   ---------------------

   function Raise_Statement (Exception_Name : String;
                             Message        : String)
                             return Statement'Class
   is
   begin
      return Result : Raise_Statement_Record do
         Result.Exception_Name := new String'(Exception_Name);
         Result.Exception_Message := new String'(Message);
      end return;
   end Raise_Statement;

   ---------------------
   -- While_Statement --
   ---------------------

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Sequence_Of_Statements'Class)
      return Statement'Class
   is
   begin
      return Result : While_Statement_Record do
         Result.Condition := new Expression'Class'(Condition);
         Result.Loop_Body := Sequence_Of_Statements (While_Body);
      end return;
   end While_Statement;

   ---------------------
   -- While_Statement --
   ---------------------

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Statement'Class)
      return Statement'Class
   is
      Seq : Sequence_Of_Statements;
   begin
      Seq.Append (While_Body);
      return While_Statement (Condition, Seq);
   end While_Statement;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : Null_Statement;
      Writer : in out Writer_Interface'Class)
   is
      pragma Unreferenced (Item);
   begin
      Writer.Put ("null");
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : Assignment_Statement;
      Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put (Item.Target.all);
      Writer.Put (" := ");
      Writer.Optional_New_Line;
      Item.Expr.Write (Writer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : Return_Statement;
      Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("return");
      if Item.Expr /= null then
         Writer.Put (" ");
         Item.Expr.Write (Writer);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Extended_Return_Statement;
                    Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("return " & Item.Return_Variable.all
                       & " : " & Item.Return_Type.all & " do");
      Item.Return_Sequence.Write (Writer);
      Writer.Put ("end return");
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Procedure_Call_Statement;
                    Writer : in out Writer_Interface'Class)
   is
      use type Ada.Containers.Count_Type;
      First : Boolean := True;
      Long_Layout : Boolean := False;
   begin

      if (Writer.Col + Item.Name.all'Length >= 40
            and then Item.Arguments.Length > 0)
        or else Item.Arguments.Length > 2
      then
         Long_Layout := True;
      end if;

      for Arg of Item.Arguments loop
         if Arg.Name /= null then
            Long_Layout := True;
            exit;
         end if;
      end loop;

      if Long_Layout then
         Writer.Put_Line (Item.Name.all);
         Writer.Indent (Writer.Indent + 2);

         for Arg of Item.Arguments loop
            if First then
               Writer.Put ("(");
               Writer.Indent (Writer.Indent + 1);
               First := False;
            else
               Writer.Put_Line (",");
            end if;

            if Arg.Name /= null then
               Writer.Put (Arg.Name.all & " => ");
            end if;

            Arg.Value.Write (Writer);
         end loop;

         Writer.Put (")");
         Writer.Indent (Writer.Indent - 3);

      else
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

      end if;

   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Sequence_Of_Statements;
                    Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Indent (Writer.Indent + 3);
      for S of Item.Sequence loop
         S.Write (Writer);
         Writer.Put_Line (";");
      end loop;
      Writer.Indent (Writer.Indent - 3);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : If_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("if ");
      Item.Condition.Write (Writer);
      Writer.Put_Line (" then");
      Item.True_Part.Write (Writer);
      for E of Item.Elsifs loop
         Writer.Put ("elsif ");
         E.Condition.Write (Writer);
         Writer.Put_Line (" then");
         E.Stats.Write (Writer);
      end loop;
      if Item.False_Part.Sequence.Last_Index > 0 then
         Writer.Put_Line ("else");
         Item.False_Part.Write (Writer);
      end if;
      Writer.Put ("end if");
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item : Case_Statement_Record;
                    Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("case ");
      Item.Case_Expression.Write (Writer);
      Writer.Put_Line (" is");
      Writer.Indent (Writer.Indent + 3);
      for Option of Item.Case_Options loop
         Writer.Put ("when ");
         if Option.Value = null then
            Writer.Put ("others");
         else
            Option.Value.Write (Writer);
         end if;
         Writer.Put_Line (" =>");
         Writer.Indent (Writer.Indent + 3);
         Option.Stats.Write (Writer);
         Writer.Indent (Writer.Indent - 3);
      end loop;
      Writer.Indent (Writer.Indent - 3);
      Writer.Put ("end case");
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Declare_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("declare");
      Item.Block.Write (Writer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Raise_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("raise " & Item.Exception_Name.all & " with");
      Writer.Put ("  """ & Item.Exception_Message.all & """");
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Loop_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("loop");
      Item.Loop_Body.Write (Writer);
      Writer.Put ("end loop");
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : While_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("while ");
      Item.Condition.Write (Writer);
      Writer.Put (" ");
      Loop_Statement_Record (Item).Write (Writer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Iterate_Statement_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("for " & Item.Loop_Variable.all & " of "
                  & Item.Container_Name.all & " ");
      Loop_Statement_Record (Item).Write (Writer);
   end Write;

end Aquarius.Drys.Statements;
