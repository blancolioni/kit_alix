with Ada.Containers.Doubly_Linked_Lists;

with Kit.SQL.Lexical;
with Kit.SQL.Parser;

with Kit.SQL.Constraints;
with Kit.SQL.Database;
with Kit.SQL.Requests;

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
        procedure (Table : Kit.SQL.Database.Table_Reference;
                   Row   : Kit.SQL.Database.Record_Reference));

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
      Query := (Ada.Finalization.Controlled with others => <>);
      Query.Initialize;
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
        (Table_Reference  : Kit.SQL.Database.Table_Reference;
         Record_Reference : Kit.SQL.Database.Record_Reference);

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row
        (Table_Reference  : Kit.SQL.Database.Table_Reference;
         Record_Reference : Kit.SQL.Database.Record_Reference)
      is
         use Kit.SQL.Data_Tables;
         Table_Name : constant String :=
                        Kit.SQL.Database.Get_Name (Table_Reference);
         Row : constant Row_Update_Cursor :=
                 Result.Add_Row;
      begin
         for I in 1 .. Kit.SQL.Database.Get_Field_Count (Table_Reference) loop
            declare
               Field_Reference : constant Kit.SQL.Database.Field_Reference :=
                                   Kit.SQL.Database.Get_Field
                                     (Table_Reference, I);
               Field_Name      : constant String :=
                                   Kit.SQL.Database.Get_Field_Name
                                     (Field_Reference);
               Field_Type      : constant Kit.SQL.Database.Data_Type'Class :=
                                   Kit.SQL.Database.Get_Field_Type
                                     (Field_Reference);
               Col             : constant Column_Count :=
                                   Get_Query_Column
                                     (Cols, Table_Name, Field_Name);
            begin
               if Col > 0 then
                  Set_Value (Row, Col,
                             Field_Type.To_String
                               (Kit.SQL.Database.Get_Field_Value
                                  (Record_Reference, I)));
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
        procedure (Table : Kit.SQL.Database.Table_Reference;
                   Row   : Kit.SQL.Database.Record_Reference))
   is
      Constraints : Kit.SQL.Constraints.Constraint_List;
      Request     : Kit.SQL.Requests.Request_Type;

      procedure On_Record
        (Table        : Kit.SQL.Database.Table_Reference;
         Record_Index : Marlowe.Database_Index);

      ---------------
      -- On_Record --
      ---------------

      procedure On_Record
        (Table        : Kit.SQL.Database.Table_Reference;
         Record_Index : Marlowe.Database_Index)
      is
      begin
         On_Row (Table,
                 Kit.SQL.Database.Get_Record_Reference (Table, Record_Index));
      end On_Record;

   begin
      if not Query.Predicate.Is_Empty then
         Query.Predicate.Element.Get_Predicate_Constraints (Constraints);
      end if;
      Request.Create (Query.Tables, Constraints);
      Request.Execute
        (On_Record'Access);

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

   -------------------
   -- Set_Predicate --
   -------------------

   procedure Set_Predicate
     (Query     : in out Query_Element;
      Predicate : Kit.SQL.Expressions.Expression_Element'Class)
   is
   begin
      Query.Predicate.Replace_Element (Predicate);
   end Set_Predicate;

end Kit.SQL.Queries;
