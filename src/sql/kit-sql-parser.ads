with Kit.SQL.Queries;

package Kit.SQL.Parser is

   Exit_Command : exception;

   procedure Parse_Query
     (Query : in out Kit.SQL.Queries.Query_Element'Class);

end Kit.SQL.Parser;
