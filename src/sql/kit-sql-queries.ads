private with Ada.Containers.Indefinite_Holders;
private with Kit.SQL.Columns.Lists;
private with Kit.SQL.Tables.Lists;

with Kit.SQL.Columns;
with Kit.SQL.Data_Tables;
with Kit.SQL.Expressions;
with Kit.SQL.Tables;

package Kit.SQL.Queries is

   type Query_Element is
     new SQL_Element with private;

   function Is_Empty (Query : Query_Element) return Boolean;

   procedure Clear (Query : in out Query_Element);

   procedure Add_Column
     (Query  : in out Query_Element;
      Column : Kit.SQL.Columns.Column_Element'Class);

   procedure Add_Table
     (Query : in out Query_Element;
      Table : Kit.SQL.Tables.Table_Element'Class);

   procedure Add_Order
     (Query  : in out Query_Element;
      Column : Kit.SQL.Columns.Column_Element'Class);

   procedure Set_Predicate
     (Query     : in out Query_Element;
      Predicate : Kit.SQL.Expressions.Expression_Element'Class);

   procedure Execute
     (Query  : Query_Element;
      Result : in out Kit.SQL.Data_Tables.Data_Table'Class);

   procedure Create
     (Query : in out Query_Element;
      Text  : String);

private

   package Expression_Holders is
     new Ada.Containers.Indefinite_Holders
       (Kit.SQL.Expressions.Expression_Element'Class,
        Kit.SQL.Expressions."=");

   type Query_Element is
     new SQL_Element with
      record
         Columns    : Kit.SQL.Columns.Lists.List;
         Tables     : Kit.SQL.Tables.Lists.List;
         Predicate  : Expression_Holders.Holder;
         Order_By   : Kit.SQL.Columns.Lists.List;
      end record;

   function Is_Empty (Query : Query_Element) return Boolean
   is (Query.Columns.Is_Empty or else Query.Tables.Is_Empty);

end Kit.SQL.Queries;
