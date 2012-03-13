with Kit.Server.Values;

package Kit.Server.Procedures is

   type Kit_Procedure is private;

   function New_Procedure (Name       : String;
                           Arguments  : Kit.Server.Values.Array_Of_Values;
                           Definition : Kit.Server.Statements.Kit_Statement)
                          return Kit_Procedure;

   procedure Execute (Proc        : Kit_Procedure;
                      Arguments   : Kit.Server.Values.Array_Of_Values;
                      Environment : Kit.Server.Environment.Kit_Environment);

end Kit.Server.Procedures;
