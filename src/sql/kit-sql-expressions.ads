private with Ada.Containers.Indefinite_Multiway_Trees;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Kit.SQL.Constraints;

package Kit.SQL.Expressions is

   type Expression_Element is
     new SQL_Element with private;

   function Boolean_Expression
     (Value : Boolean)
      return Expression_Element'Class;

   function Integer_Expression
     (Value : Integer)
      return Expression_Element'Class;

   function Float_Expression
     (Value : Float)
      return Expression_Element'Class;

   function String_Expression
     (Value : String)
      return Expression_Element'Class;

   function Identifier_Expression
     (Identifier : String)
      return Expression_Element'Class;

   type Operator_Type is
     (Op_Or, Op_And, Op_EQ, Op_NE, Op_LT, Op_GT, Op_LE, Op_GE);

   function Operator_Expression
     (Operator    : Operator_Type;
      Left, Right : Expression_Element'Class)
      return Expression_Element'Class;

   procedure Get_Predicate_Constraints
     (Expression  : Expression_Element'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class);

   type Expression_List is tagged private;

   procedure Append
     (List       : in out Expression_List;
      Expression : Expression_Element'Class);

   function Function_Call_Expression
     (Function_Name : String;
      Arguments     : Expression_List'Class)
      return Expression_Element'Class;

private

   type Root_Expression_Node is abstract tagged null record;

   procedure Copy_Constraints
     (Node        : Root_Expression_Node;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is null;

   procedure Add_Table_Field_Constraint
     (Node : Root_Expression_Node;
      Add  : not null access
        procedure (Table_Name : String;
                   Field_Name : String))
   is null;

   function To_Value
     (Node : Root_Expression_Node)
      return Kit.SQL.Constraints.Field_Value_Type
   is (Kit.SQL.Constraints.No_Value);

   package Expression_Trees is
     new Ada.Containers.Indefinite_Multiway_Trees
       (Root_Expression_Node'Class);

   type Expression_Element is
     new SQL_Element with
      record
         Tree : Expression_Trees.Tree;
      end record;

   package Expression_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Expression_Element'Class);

   type Expression_List is tagged
      record
         List : Expression_Lists.List;
      end record;

end Kit.SQL.Expressions;
