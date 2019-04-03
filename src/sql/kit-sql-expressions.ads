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

   type Node_Interface is interface;

   procedure Copy_Constraints
     (Node        : Node_Interface;
      Children    : Expression_List'Class;
      Constraints : in out Kit.SQL.Constraints.Constraint_List'Class)
   is null;

   package Expression_Trees is
     new Ada.Containers.Indefinite_Multiway_Trees (Node_Interface'Class);

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
