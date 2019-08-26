package body Kit.Schema.Databases is

   ------------
   -- Append --
   ------------

   procedure Append
     (Db   : in out Root_Database_Type;
      Item : in     Kit.Schema.Tables.Table_Type)
   is
   begin
      Db.Tables.Append (Item);
   end Append;

   --------------
   -- Contains --
   --------------

   function Contains
     (Database : Root_Database_Type;
      Name     : String)
      return Boolean
   is
   begin
      for T of Database.Tables loop
         if T.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ---------------------
   -- Create_Database --
   ---------------------

   function Create_Database
     (Name : in     String)
      return Database_Type
   is
      Result : constant Database_Type :=
                 new Root_Database_Type;
   begin
      Result.Create (Name);
      Result.Set_Property
        ("database_package_name",
         Kit.Schema.Properties.String_Value (Name));
      Result.Set_Property
        ("handle_package_name",
         Kit.Schema.Properties.String_Value (Name & ".Handles"));

      Result.Register_Properties (Name);

      return Result;
   end Create_Database;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Position : Table_Cursor)
      return Kit.Schema.Tables.Table_Type
   is
   begin
      return Table_Vectors.Element (Table_Vectors.Cursor (Position));
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Database : Root_Database_Type;
                     Name     : String)
                     return Kit.Schema.Tables.Table_Type
   is
   begin
      for T of Database.Tables loop
         if T.Name = Name then
            return T;
         end if;
      end loop;
      raise Constraint_Error with
        "table " & Name & " not found in database " & Database.Name;
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Database : Root_Database_Type;
                     Index    : Positive)
                     return Kit.Schema.Tables.Table_Type
   is
   begin
      return Database.Tables.Element (Index);
   end Element;

   -----------------
   -- First_Table --
   -----------------

   function First_Table (Database : Root_Database_Type) return Table_Cursor is
   begin
      return Table_Cursor (Database.Tables.First);
   end First_Table;

   -----------------------
   -- Has_Display_Field --
   -----------------------

   function Has_Display_Field (Db : Root_Database_Type) return Boolean is
   begin
      for T of Db.Tables loop
         if T.Has_Display_Field then
            return True;
         end if;
      end loop;
      return False;
   end Has_Display_Field;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element
     (Position : Table_Cursor)
      return Boolean
   is
   begin
      return Table_Vectors.Has_Element (Table_Vectors.Cursor (Position));
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Database : Root_Database_Type;
                      Process  : not null access
                        procedure (Table : Kit.Schema.Tables.Table_Type))
   is
      procedure Call_Process (Position : Table_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Table_Vectors.Cursor) is
      begin
         Process (Table_Vectors.Element (Position));
      end Call_Process;

   begin
      Database.Tables.Iterate (Call_Process'Access);
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (Position : in out Table_Cursor) is
   begin
      Table_Vectors.Next (Table_Vectors.Cursor (Position));
   end Next;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Database   : in out Root_Database_Type;
      Name       : String;
      Value      : Kit.Schema.Properties.Kit_Property_Value)
   is
   begin
      Database.Properties.Set_Property (Name, Value);
   end Set_Property;

   -----------------
   -- Table_Count --
   -----------------

   function Table_Count (Db : Root_Database_Type) return Natural is
   begin
      return Db.Tables.Last_Index;
   end Table_Count;

   -------------------
   -- With_Database --
   -------------------

   procedure With_Database
     (Db     : in out Root_Database_Type'Class;
      Withed : in     Database_Type)
   is
   begin
      for T of Withed.Tables loop
         Db.Append (T);
      end loop;
   end With_Database;

end Kit.Schema.Databases;
