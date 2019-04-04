package body Kit.SQL.Expressions is

   type Boolean_Node is
     new Root_Expression_Node with
      record
         Value : Boolean;
      end record;

   type Integer_Node is
     new Root_Expression_Node with
      record
         Value : Integer;
      end record;

   overriding function To_Value
     (Node : Integer_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.To_Field_Value (Node.Value));

   type Float_Node is
     new Root_Expression_Node with
      record
         Value : Float;
      end record;

   type String_Node is
     new Root_Expression_Node with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function To_Value
     (Node : String_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.To_Field_Value (-Node.Value));

   type Identifier_Node is
     new Root_Expression_Node with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Add_Table_Field_Constraint
     (Node : Identifier_Node;
      Add  : not null access
        procedure (Table_Name : String;
                   Field_Name : String));

   type Function_Call_Node is
     new Root_Expression_Node with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Operator_Node is
     new Root_Expression_Node with
      record
         Op : Operator_Type;
      end record;

   overriding procedure Copy_Constraints
     (Node        : Operator_Node;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class);

   type And_Node is
     new Root_Expression_Node with
      record
         null;
      end record;

   overriding procedure Copy_Constraints
     (Node        : And_Node;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class);

   function To_Expression
     (Node : Root_Expression_Node'Class)
      return Expression_Element'Class;

   function To_Expression
     (Node      : Root_Expression_Node'Class;
      Arguments : Expression_List'Class)
      return Expression_Element'Class;

   function To_Node
     (Expression : Expression_Element'Class)
      return Root_Expression_Node'Class
   is (Expression_Trees.Element
         (Expression_Trees.First_Child (Expression.Tree.Root)));

   --------------------------------
   -- Add_Table_Field_Constraint --
   --------------------------------

   overriding procedure Add_Table_Field_Constraint
     (Node : Identifier_Node;
      Add  : not null access
        procedure (Table_Name : String;
                   Field_Name : String))
   is
   begin
      Add ("", -Node.Name);
   end Add_Table_Field_Constraint;

   --------------------
   -- And_Expression --
   --------------------

   function And_Expression
     (Arguments     : Expression_List'Class)
      return Expression_Element'Class
   is
      Node : And_Node;
   begin
      return To_Expression (Node, Arguments);
   end And_Expression;

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
   -- Copy_Constraints --
   ----------------------

   overriding procedure Copy_Constraints
     (Node        : Operator_Node;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is
      use Kit.SQL.Constraints;

      Factory : constant Constraint_Factory'Class :=
                  (case Node.Op is
                      when Op_EQ => Equality (Negated => False),
                      when Op_NE => Equality (Negated => True),
                      when Op_LT => Maximum (Inclusive => False),
                      when Op_LE => Maximum (Inclusive => True),
                      when Op_GT => Minimum (Inclusive => False),
                      when Op_GE => Minimum (Inclusive => True));

      procedure Add_Constraint
        (Table_Name : String;
         Field_Name : String);

      --------------------
      -- Add_Constraint --
      --------------------

      procedure Add_Constraint
        (Table_Name : String;
         Field_Name : String)
      is
      begin
         Constraints.Add
           (Factory.Create
              (Table_Name, Field_Name,
               To_Node (Children.List.Last_Element).To_Value));
      end Add_Constraint;

      Left : constant Root_Expression_Node'Class :=
               To_Node (Children.List.First_Element);
   begin
      Left.Add_Table_Field_Constraint
        (Add_Constraint'Access);
   end Copy_Constraints;

   ----------------------
   -- Copy_Constraints --
   ----------------------

   overriding procedure Copy_Constraints
     (Node        : And_Node;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is
      pragma Unreferenced (Node);
   begin
      for Child of Children.List loop
         Child.Get_Predicate_Constraints (Constraints);
      end loop;
   end Copy_Constraints;

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
            Source => Expression_Trees.First_Child (Arg.Tree.Root));
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

   -------------------------
   -- Operator_Expression --
   -------------------------

   function Operator_Expression
     (Operator    : Operator_Type;
      Left, Right : Expression_Element'Class)
      return Expression_Element'Class
   is
      Call : Expression_Element'Class :=
               To_Expression
                 (Operator_Node'(Op => Operator));
   begin
      Call.Tree.Copy_Subtree
        (Expression_Trees.First_Child (Call.Tree.Root),
         Before => Expression_Trees.No_Element,
         Source => Expression_Trees.First_Child (Left.Tree.Root));
      Call.Tree.Copy_Subtree
        (Expression_Trees.First_Child (Call.Tree.Root),
         Before => Expression_Trees.No_Element,
         Source => Expression_Trees.First_Child (Right.Tree.Root));
      return Call;
   end Operator_Expression;

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
     (Node : Root_Expression_Node'Class)
      return Expression_Element'Class
   is
      Expr : Expression_Element;
   begin
      Expr.Create;
      Expr.Tree.Insert_Child
        (Expr.Tree.Root, Expression_Trees.No_Element, Node);
      return Expr;
   end To_Expression;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Node      : Root_Expression_Node'Class;
      Arguments : Expression_List'Class)
      return Expression_Element'Class
   is
      Expr : Expression_Element'Class := To_Expression (Node);
   begin
      for Arg of Arguments.List loop
         Expr.Tree.Copy_Subtree
           (Expression_Trees.First_Child (Expr.Tree.Root),
            Before => Expression_Trees.No_Element,
            Source => Expression_Trees.First_Child (Arg.Tree.Root));
      end loop;
      return Expr;
   end To_Expression;

end Kit.SQL.Expressions;
