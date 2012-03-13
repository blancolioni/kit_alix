with Abydos.Environments;
with Abydos.Values;

package Abydos.Expressions is

   type Expression is private;

   function To_Expression (V : Values.Value)
                          return Expression;

   function Object (Name : String) return Expression;
   function Select_Field (Rec  : Expression;
                          Field : String)
                         return Expression;

   type Array_Of_Expressions is array (Positive range <>) of Expression;
   function Function_Call (Function_Name : String;
                           Arguments     : Array_Of_Expressions)
                          return Expression;

   function Selection
     (Table      : Expression;
      Field      : Expression;
      Key        : Expression;
      Key_Value  : Expression;
      Condition  : Expression)
     return Expression;

   function Evaluate
     (E : Expression;
      Env : Abydos.Environments.Environment)
     return Abydos.Values.Value;

private

   type Root_Expression_Type is abstract tagged null record;

   function Evaluate (Expr    : Root_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value
      is abstract;

   type Expression is access all Root_Expression_Type'Class;

end Abydos.Expressions;
