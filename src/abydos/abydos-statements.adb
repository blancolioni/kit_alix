with Abydos.Values;

package body Abydos.Statements is

   type Expression_Statement_Type is
     new Root_Statement_Type with
      record
         E : Abydos.Expressions.Expression;
      end record;

   overriding
   procedure Execute (Item    : Expression_Statement_Type;
                      Env     : Abydos.Environments.Environment);

   type Assignment_Statement_Type is
     new Root_Statement_Type with
      record
         Dst, Src : Abydos.Expressions.Expression;
      end record;

   overriding
   procedure Execute (Item    : Assignment_Statement_Type;
                      Env     : Abydos.Environments.Environment);

   type If_Statement_Type is
     new Root_Statement_Type with
      record
         Condition  : Abydos.Expressions.Expression;
         True_Part  : Statement;
         False_Part : Statement;
      end record;

   overriding
   procedure Execute (Item    : If_Statement_Type;
                      Env     : Abydos.Environments.Environment);

   type Join_Statement_Type is
     new Root_Statement_Type with
      record
         First, Next : Statement;
      end record;

   overriding
   procedure Execute (Item    : Join_Statement_Type;
                      Env     : Abydos.Environments.Environment);

   ------------
   -- Assign --
   ------------

   function Assign
     (Target : Abydos.Expressions.Expression;
      Value  : Abydos.Expressions.Expression)
      return Statement
   is
   begin
      return new Assignment_Statement_Type'(Target, Value);
   end Assign;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Item        : Statement;
      Environment : Abydos.Environments.Environment)
   is
   begin
      Item.Execute (Environment);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item    : Expression_Statement_Type;
                      Env     : Abydos.Environments.Environment)
   is
      Result : constant Abydos.Values.Value :=
                 Abydos.Expressions.Evaluate (Item.E, Env);
      pragma Unreferenced (Result);
   begin
      null;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item    : Assignment_Statement_Type;
                      Env     : Abydos.Environments.Environment)
   is
      Target : constant Abydos.Values.Value :=
                 Abydos.Expressions.Evaluate (Item.Dst, Env);
      Target_Name : constant String :=
                      Abydos.Values.To_String (Target);
      Source      : constant Abydos.Values.Value :=
                      Abydos.Expressions.Evaluate (Item.Src, Env);
   begin
      Abydos.Environments.Update (Env, Target_Name, Source);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item    : If_Statement_Type;
                      Env     : Abydos.Environments.Environment)
   is
   begin
      if Values.To_Boolean (Expressions.Evaluate (Item.Condition, Env)) then
         Item.True_Part.Execute (Env);
      else
         Item.False_Part.Execute (Env);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item    : Join_Statement_Type;
                      Env     : Abydos.Environments.Environment)
   is
   begin
      Item.First.Execute (Env);
      Item.Next.Execute (Env);
   end Execute;

   --------------------------
   -- Expression_Statement --
   --------------------------

   function Expression_Statement
     (Value : Abydos.Expressions.Expression)
      return Statement
   is
   begin
      return new Expression_Statement_Type'(E => Value);
   end Expression_Statement;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Condition : Abydos.Expressions.Expression;
      True_Part : Statement;
      False_Part : Statement)
      return Statement
   is
   begin
      return new If_Statement_Type'(Condition, True_Part, False_Part);
   end If_Statement;

   ----------
   -- Join --
   ----------

   function Join (First, Next : Statement) return Statement is
   begin
      return new Join_Statement_Type'(First, Next);
   end Join;

end Abydos.Statements;
