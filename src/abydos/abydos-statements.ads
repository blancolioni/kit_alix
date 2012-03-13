with Abydos.Expressions;
with Abydos.Environments;

package Abydos.Statements is

   type Statement is private;

   function Expression_Statement (Value : Abydos.Expressions.Expression)
                                 return Statement;

   function Assign (Target : Abydos.Expressions.Expression;
                    Value  : Abydos.Expressions.Expression)
                   return Statement;

   function If_Statement (Condition : Abydos.Expressions.Expression;
                          True_Part : Statement;
                          False_Part : Statement)
                         return Statement;

   function Join (First, Next : Statement) return Statement;

   procedure Execute (Item        : Statement;
                      Environment : Abydos.Environments.Environment);

private

   type Statement_Record;
   type Statement is access Statement_Record;

end Abydos.Statements;
