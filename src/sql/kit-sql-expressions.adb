with Ada.Containers.Indefinite_Holders;

package body Kit.SQL.Expressions is

   package Expression_Element_Holders is
     new Ada.Containers.Indefinite_Holders (Expression_Element'Class);

   type Boolean_Node is
     new Expression_Element with
      record
         Value : Boolean;
      end record;

   type Integer_Node is
     new Expression_Element with
      record
         Value : Integer;
      end record;

   overriding function To_Value
     (Node : Integer_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.To_Field_Value (Node.Value));

   type Float_Node is
     new Expression_Element with
      record
         Value : Float;
      end record;

   overriding function To_Value
     (Node : Float_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.To_Field_Value (Node.Value));

   type String_Node is
     new Expression_Element with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function To_Value
     (Node : String_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.To_Field_Value (-Node.Value));

   type Identifier_Node is
     new Expression_Element with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Add_Table_Field_Constraint
     (Node : Identifier_Node;
      Add  : not null access
        procedure (Table_Name : String;
                   Field_Name : String));

   type Function_Call_Node is
     new Expression_Element with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Arguments : Expression_Lists.List;
      end record;

   type Operator_Node is
     new Expression_Element with
      record
         Op          : Operator_Type;
         Left, Right : Expression_Element_Holders.Holder;
      end record;

   overriding procedure Copy_Constraints
     (Node        : Operator_Node;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class);

   type And_Node is
     new Expression_Element with
      record
         Arguments : Expression_Lists.List;
      end record;

   overriding procedure Copy_Constraints
     (Node        : And_Node;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class);

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
      Node.Arguments := Arguments.List;
      return Node;
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
      Result : Boolean_Node;
   begin
      Result.Value := Value;
      return Result;
   end Boolean_Expression;

   ----------------------
   -- Copy_Constraints --
   ----------------------

   overriding procedure Copy_Constraints
     (Node        : Operator_Node;
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
              (Table_Name, Field_Name, Node.Right.Element.To_Value));
      end Add_Constraint;

   begin
      Node.Left.Element.Add_Table_Field_Constraint
        (Add_Constraint'Access);
   end Copy_Constraints;

   ----------------------
   -- Copy_Constraints --
   ----------------------

   overriding procedure Copy_Constraints
     (Node        : And_Node;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is
   begin
      for Child of Node.Arguments loop
         Child.Copy_Constraints (Constraints);
      end loop;
   end Copy_Constraints;

   ----------------------
   -- Float_Expression --
   ----------------------

   function Float_Expression
     (Value : Float)
      return Expression_Element'Class
   is
      Result : Float_Node;
   begin
      Result.Value := Value;
      return Result;
   end Float_Expression;

   ------------------------------
   -- Function_Call_Expression --
   ------------------------------

   function Function_Call_Expression
     (Function_Name : String;
      Arguments     : Expression_List'Class)
      return Expression_Element'Class
   is
      Result : Function_Call_Node;
   begin
      Result.Name := +Function_Name;
      Result.Arguments := Arguments.List;
      return Result;
   end Function_Call_Expression;

   -------------------------------
   -- Get_Predicate_Constraints --
   -------------------------------

   procedure Get_Predicate_Constraints
     (Expression  : Expression_Element'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is
   begin
      Expression.Copy_Constraints (Constraints);
   end Get_Predicate_Constraints;

   ---------------------------
   -- Identifier_Expression --
   ---------------------------

   function Identifier_Expression
     (Identifier : String)
      return Expression_Element'Class
   is
      Result : Identifier_Node;
   begin
      Result.Name := +Identifier;
      return Result;
   end Identifier_Expression;

   ------------------------
   -- Integer_Expression --
   ------------------------

   function Integer_Expression
     (Value : Integer)
      return Expression_Element'Class
   is
      Result : Integer_Node;
   begin
      Result.Value := Value;
      return Result;
   end Integer_Expression;

   -------------------------
   -- Operator_Expression --
   -------------------------

   function Operator_Expression
     (Operator    : Operator_Type;
      Left, Right : Expression_Element'Class)
      return Expression_Element'Class
   is
      Result : Operator_Node;
   begin
      Result.Op := Operator;
      Result.Left := Expression_Element_Holders.To_Holder (Left);
      Result.Right := Expression_Element_Holders.To_Holder (Right);
      return Result;
   end Operator_Expression;

   -----------------------
   -- String_Expression --
   -----------------------

   function String_Expression
     (Value : String)
      return Expression_Element'Class
   is
      Result : String_Node;
   begin
      Result.Value := +Value;
      return Result;
   end String_Expression;

end Kit.SQL.Expressions;
