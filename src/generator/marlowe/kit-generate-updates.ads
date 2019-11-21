with Syn.Declarations;
with Kit.Schema.Tables;

package Kit.Generate.Updates is

   procedure Generate_Update_Subprograms
     (Table  : in     Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

end Kit.Generate.Updates;
