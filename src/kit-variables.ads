with Marlowe;

package Kit.Variables is

   type Variable is private;

   function Get_Name (Var : Variable) return String;
   function Get_Table_Type (Var : Variable) return Table_Type;
   function Get_Search_Key (Var : Variable) return Generic_Key_Type;
   function Forward (Var : Variable) return Boolean;
   function First (Var : Variable) return Boolean;

   function Get_Database_Index (Var : Variable) return Marlowe.Database_Index;

   function New_Variable (Name : String) return Variable;
   procedure Free (Var : in out Variable);

   procedure Set_Table_Type (Var : Variable;
                             TT  : Table_Type);

   procedure Set_Database_Index (Var   : Variable;
                                 Index : Marlowe.Database_Index);

   function Use_Key_Value (Var : Variable) return Boolean;

   procedure Set_First (Var   : Variable;
                        First : Boolean);

   procedure Set_Search (Var           : Variable;
                         Key           : Generic_Key_Type;
                         Forward       : Boolean;
                         Use_Key_Value : Boolean);

private

   type Variable_Record;

   type Variable is access Variable_Record;

end Kit.Variables;
