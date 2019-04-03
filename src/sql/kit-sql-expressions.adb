package body Kit.SQL.Expressions is

   type Boolean_Node is
     new Node_Interface with
      record
         Value : Boolean;
      end record;

   type Integer_Node is
     new Node_Interface with
      record
         Value : Integer;
      end record;

   type Float_Node is
     new Node_Interface with
      record
         Value : Float;
      end record;

   type String_Node is
     new Node_Interface with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Identifier_Node is
     new Node_Interface with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Function_Call_Node is
     new Node_Interface with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function To_Expression
     (Node : Node_Interface'Class)
      return Expression_Element'Class;

   ------------
   -- Append --
   ------------

   procedure Append
     (List       : in out Expression_List;
      Expression : Expression_Element'Class)
   is
   begin
      List.List.Append (Expression);
   end Append;

   ------------------------
   -- Boolean_Expression --
   ------------------------

   function Boolean_Expression
     (Value : Boolean)
      return Expression_Element'Class
   is
   begin
      return To_Expression (Boolean_Node'(Value => Value));
   end Boolean_Expression;

   ----------------------
   -- Float_Expression --
   ----------------------

   function Float_Expression
     (Value : Float)
      return Expression_Element'Class
   is
   begin
      return To_Expression (Float_Node'(Value => Value));
   end Float_Expression;

   ------------------------------
   -- Function_Call_Expression --
   ------------------------------

   function Function_Call_Expression
     (Function_Name : String;
      Arguments     : Expression_List'Class)
      return Expression_Element'Class
   is
      Call : Expression_Element'Class :=
               To_Expression
                 (Function_Call_Node'(Name => +Function_Name));
   begin
      for Arg of Arguments.List loop
         Call.Tree.Copy_Subtree
           (Expression_Trees.First_Child (Call.Tree.Root),
            Before => Expression_Trees.No_Element,
            Source => Arg.Tree.Root);
      end loop;
      return Call;
   end Function_Call_Expression;

   -------------------------------
   -- Get_Predicate_Constraints --
   -------------------------------

   procedure Get_Predicate_Constraints
     (Expression  : Expression_Element'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is
      Top : constant Expression_Trees.Cursor :=
              Expression_Trees.First_Child (Expression.Tree.Root);
      Args : Expression_List;
   begin
      for Child in Expression.Tree.Iterate_Children (Top) loop
         Args.List.Append (To_Expression (Expression_Trees.Element (Child)));
      end loop;

      Expression_Trees.Element (Top).Copy_Constraints
        (Args, Constraints);
   end Get_Predicate_Constraints;

   ---------------------------
   -- Identifier_Expression --
   ---------------------------

   function Identifier_Expression
     (Identifier : String)
      return Expression_Element'Class
   is
   begin
      return To_Expression (Identifier_Node'(Name => +Identifier));
   end Identifier_Expression;

   ------------------------
   -- Integer_Expression --
   ------------------------

   function Integer_Expression
     (Value : Integer)
      return Expression_Element'Class
   is
   begin
      return To_Expression (Integer_Node'(Value => Value));
   end Integer_Expression;

   -----------------------
   -- String_Expression --
   -----------------------

   function String_Expression
     (Value : String)
      return Expression_Element'Class
   is
   begin
      return To_Expression (String_Node'(Value => +Value));
   end String_Expression;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Node : Node_Interface'Class)
      return Expression_Element'Class
   is
      Expr : Expression_Element;
   begin
      Expr.Create;
      Expr.Tree.Insert_Child
        (Expr.Tree.Root, Expression_Trees.No_Element, Node);
      return Expr;
   end To_Expression;

end Kit.SQL.Expressions;
