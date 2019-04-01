with Ada.Containers.Doubly_Linked_Lists;

with Kit.SQL.Lexical;
with Kit.SQL.Parser;

with Kit.Db.Tables;

package body Kit.SQL.Queries is

   type Result_Column is
      record
         Table_Name : Ada.Strings.Unbounded.Unbounded_String;
         Field_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Result_Column_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Result_Column);

   function Get_Query_Column
     (List       : Result_Column_Lists.List;
      Table_Name : String;
      Field_Name : String)
      return Column_Count;

   function Get_Query_Columns
     (Query : Query_Element'Class)
      return Result_Column_Lists.List;

   procedure Execute_Query_Scan
     (Query : Query_Element'Class;
      On_Row : not null access
        procedure (Table : String;
                   Row   : Kit.Db.Tables.Database_Record'Class));

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column
     (Query  : in out Query_Element;
      Column : Kit.SQL.Columns.Column_Element'Class)
   is
   begin
      Query.Columns.Append (Column);
   end Add_Column;

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (Query  : in out Query_Element;
      Column : Kit.SQL.Columns.Column_Element'Class)
   is
   begin
      Query.Order_By.Append (Column);
   end Add_Order;

   ---------------
   -- Add_Table --
   ---------------

   procedure Add_Table
     (Query : in out Query_Element;
      Table : Kit.SQL.Tables.Table_Element'Class)
   is
   begin
      Query.Tables.Append (Table);
   end Add_Table;

   -----------
   -- Clear --
   -----------

   procedure Clear (Query : in out Query_Element) is
   begin
      Query := (others => <>);
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create
     (Query : in out Query_Element;
      Text  : String)
   is
   begin
      Query.Clear;
      Kit.SQL.Lexical.Open_String (Text);
      Query.Create;
      Kit.SQL.Parser.Parse_Query (Query);
      Kit.SQL.Lexical.Close;
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Query  : Query_Element;
      Result : in out Kit.SQL.Data_Tables.Data_Table'Class)
   is
      Cols : constant Result_Column_Lists.List :=
               Get_Query_Columns (Query);

      Qualified_Table_Names : constant Boolean :=
                                Natural (Query.Tables.Length) > 1;

      procedure Add_Row
        (Table_Name : String;
         Table_Row  : Kit.Db.Tables.Database_Record'Class);

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row
        (Table_Name : String;
         Table_Row  : Kit.Db.Tables.Database_Record'Class)
      is
         use Kit.SQL.Data_Tables;
         Row : constant Row_Update_Cursor :=
                 Result.Add_Row;
      begin
         for I in 1 .. Table_Row.Field_Count loop
            declare
               Col : constant Column_Count :=
                       Get_Query_Column
                         (Cols, Table_Name, Table_Row.Field_Name (I));
            begin
               if Col > 0 then
                  Set_Value (Row, Col, Table_Row.Get (I));
               end if;
            end;
         end loop;
      end Add_Row;

   begin
      Result.Clear;
      for Col of Cols loop
         Result.Add_Column
           ((if Qualified_Table_Names
            then (-Col.Table_Name) & "." else "")
            & (-Col.Field_Name));
      end loop;

      Execute_Query_Scan (Query, Add_Row'Access);
   end Execute;

   ------------------------
   -- Execute_Query_Scan --
   ------------------------

   procedure Execute_Query_Scan
     (Query  : Query_Element'Class;
      On_Row : not null access
        procedure (Table : String;
                   Row   : Kit.Db.Tables.Database_Record'Class))
   is
   begin
      for Query_Table of Query.Tables loop
         declare
            Table : constant Kit.Db.Tables.Database_Table :=
                      Kit.Db.Tables.Get_Table (Query_Table.Table_Name);

            procedure Handle_Row
              (Item : Kit.Db.Tables.Database_Record'Class);

            ----------------
            -- Handle_Row --
            ----------------

            procedure Handle_Row
              (Item : Kit.Db.Tables.Database_Record'Class)
            is
            begin
               On_Row (Query_Table.Table_Name, Item);
            end Handle_Row;

         begin
            Table.Iterate ("top_record", Handle_Row'Access);
         end;
      end loop;
   end Execute_Query_Scan;

   ----------------------
   -- Get_Query_Column --
   ----------------------

   function Get_Query_Column
     (List       : Result_Column_Lists.List;
      Table_Name : String;
      Field_Name : String)
      return Column_Count
   is
      Index : Column_Count := 0;
   begin
      for Col of List loop
         Index := Index + 1;
         if Same (Col.Table_Name, Table_Name)
           and then Same (Col.Field_Name, Field_Name)
         then
            return Index;
         end if;
      end loop;
      return 0;
   end Get_Query_Column;

   -----------------------
   -- Get_Query_Columns --
   -----------------------

   function Get_Query_Columns
     (Query : Query_Element'Class)
      return Result_Column_Lists.List
   is
   begin
      return Result : Result_Column_Lists.List do
         for Query_Column of Query.Columns loop
            for Query_Table of Query.Tables loop
               declare
                  Table_Name : constant String :=
                                 Query_Table.Table_Name;
                  Table      : constant Kit.Db.Tables.Database_Table :=
                                 Kit.Db.Tables.Get_Table (Table_Name);
               begin
                  for I in 1 .. Table.Field_Count loop
                     declare
                        Field_Name : constant String :=
                                       Table.Field_Name (I);
                     begin
                        if Query_Column.References_Column
                          (Table_Name, Field_Name)
                        then
                           Result.Append
                             ((+Table_Name, +Field_Name));
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end loop;
      end return;
   end Get_Query_Columns;

end Kit.SQL.Queries;
