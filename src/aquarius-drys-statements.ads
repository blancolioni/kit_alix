private with Ada.Containers.Vectors;

with Aquarius.Drys.Blocks;

package Aquarius.Drys.Statements is

   type Sequence_Of_Statements is
     new Syntax_Root and Statement_Sequencer with private;

   overriding
   procedure Append (To : in out Sequence_Of_Statements;
                     S  : in     Statement'Class);

   procedure Append (To : in out Sequence_Of_Statements;
                     S  : in     String);

   overriding
   procedure Write (Item   : Sequence_Of_Statements;
                    Writer : in out Writer_Interface'Class);

   function Declare_Statement
     (Block : Aquarius.Drys.Blocks.Block_Type'Class)
      return Statement'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements;
      False_Part : Sequence_Of_Statements)
      return Statement'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements)
      return Statement'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Statement'Class)
      return Statement'Class;

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Sequence_Of_Statements'Class)
      return Statement'Class;

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Statement'Class)
      return Statement'Class;

   type Case_Statement_Record is
     new Statement with private;

   function Case_Statement (Case_Expression : String)
                            return Case_Statement_Record'Class;

   procedure Add_Case_Option
     (Statement : in out Case_Statement_Record'Class;
      Value     : in     String;
      Stats     : in     Sequence_Of_Statements'Class);

   overriding
   procedure Write (Item : Case_Statement_Record;
                    Writer : in out Writer_Interface'Class);

   function Raise_Statement (Exception_Name : String;
                             Message        : String)
                             return Statement'Class;

   type Null_Statement is
     new Statement with private;

   overriding
   procedure Write (Item   : Null_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Return_Statement
     (Result : Expression'Class)
      return Statement'Class;

   function New_Return_Statement
     (Return_Variable   : String;
      Variable_Type     : String;
      Return_Statements : Sequence_Of_Statements'Class)
      return Statement'Class;

   type Assignment_Statement is new Statement with private;

   overriding
   procedure Write (Item   : Assignment_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Assignment_Statement
     (Target : String;
      Value  : Expression'Class)
      return Statement'Class;

   type Procedure_Call_Statement is
     new Statement with private;

   overriding
   procedure Write (Item   : Procedure_Call_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Procedure_Call_Statement
     (Procedure_Name : String)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument       : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class;
      Argument_3     : Expression'Class)
      return Procedure_Call_Statement;

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Name      : in     String;
      Value     : in     Expression'Class);

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Value     : in     Expression'Class);

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Value     : in     String);

private

   type Sequence_Of_Statements is
     new Syntax_Root
     and Statement_Sequencer
   with
      record
         Sequence : Statement_Vectors.Vector;
      end record;

   type Null_Statement is new Statement with null record;

   type Assignment_Statement is new Statement with
      record
         Target : access String;
         Expr   : access Expression'Class;
      end record;

   type Procedure_Call_Statement is
     new Statement with
      record
         Name      : access String;
         Arguments : Actual_Argument_Lists.List;
      end record;

   type Case_Option is
      record
         Value : access Expression'Class;
         Stats : Sequence_Of_Statements;
      end record;

   package Case_Option_Vector is
     new Ada.Containers.Vectors (Positive, Case_Option);

   type Case_Statement_Record is
     new Statement with
      record
         Case_Expression : access Expression'Class;
         Case_Options    : Case_Option_Vector.Vector;
      end record;

end Aquarius.Drys.Statements;
