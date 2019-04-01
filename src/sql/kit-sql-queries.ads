private with Kit.SQL.Columns.Lists;
private with Kit.SQL.Tables.Lists;

with Kit.SQL.Columns;
with Kit.SQL.Data_Tables;
with Kit.SQL.Tables;

package Kit.SQL.Queries is

   type Query_Element is
     new SQL_Element with private;

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

   procedure Execute
     (Query  : Query_Element;
      Result : in out Kit.SQL.Data_Tables.Data_Table'Class);

   procedure Create
     (Query : in out Query_Element;
      Text  : String);

private

   type Query_Element is
     new SQL_Element with
      record
         Columns  : Kit.SQL.Columns.Lists.List;
         Tables   : Kit.SQL.Tables.Lists.List;
         Order_By : Kit.SQL.Columns.Lists.List;
      end record;

end Kit.SQL.Queries;
