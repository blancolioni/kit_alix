with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;

with Kit.Options;

package body Kit.Generate.Fetch is

   ----------------------
   -- Fetch_From_Index --
   ----------------------

   procedure Fetch_From_Index
     (Table       : Kit.Schema.Tables.Table_Type'Class;
      Object_Name : String;
      Target      : in out Aquarius.Drys.Statement_Sequencer'Class)
   is

      procedure Get_Base (Base   : Kit.Schema.Tables.Table_Type'Class);

      --------------
      -- Get_Base --
      --------------

      procedure Get_Base (Base   : Kit.Schema.Tables.Table_Type'Class) is
         use Aquarius.Drys;
         use Aquarius.Drys.Expressions;
         use Aquarius.Drys.Statements;

         Base_Target    : constant String :=
                            Object_Name & Base.Base_Component_Name;
         Cache_Package  : constant String :=
                            Base.Ada_Name & "_Cache";
         Index_Variable : constant String :=
                            Table.Database_Index_Component
                              (Object_Name, Base);
      begin
         Target.Append
           (New_Assignment_Statement
              (Base_Target,
               New_Function_Call_Expression
                 (Cache_Package & ".Get",
                  Object ("Marlowe_Keys.Handle"),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Index_Variable))));
      end Get_Base;

   begin
      if Kit.Options.Generate_Debug then
         declare
            use Aquarius.Drys;
            use Aquarius.Drys.Expressions;
            use Aquarius.Drys.Statements;
         begin
            Target.Append
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Operator
                    ("&",
                     Literal
                       ("Fetch " & Table.Ada_Name & ": "),
                     New_Function_Call_Expression
                       ("Marlowe.Database_Index'Image",
                        Object (Object_Name & ".Index")))));
         end;
      end if;

      Table.Iterate (Get_Base'Access,
                     Inclusive   => True,
                     Table_First => True);
   end Fetch_From_Index;

end Kit.Generate.Fetch;
