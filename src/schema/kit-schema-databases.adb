package body Kit.Schema.Databases is

   ------------
   -- Append --
   ------------

   procedure Append
     (Db   : in out Database_Type;
      Item : in     Kit.Schema.Tables.Table_Type'Class)
   is
      New_Item : constant Database_Table :=
                   new Kit.Schema.Tables.Table_Type'Class'(Item);
   begin
      if False then
         for I in 1 .. Db.Tables.Last_Index loop
            if Item.References_Table (Db.Tables.Element (I).all) then
               Db.Tables.Insert (I, New_Item);
               return;
            end if;
         end loop;
      end if;
      Db.Tables.Append (new Kit.Schema.Tables.Table_Type'Class'(Item));
   end Append;

   --------------
   -- Contains --
   --------------

   function Contains
     (Database : Database_Type;
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

   procedure Create_Database (Db   : in out Database_Type;
                              Name : in     String)
   is
   begin
      Kit.Names.Create (Kit.Names.Root_Named_Object (Db), Name);
   end Create_Database;

   -------------
   -- Element --
   -------------

   function Element
     (Position : Table_Cursor)
      return Kit.Schema.Tables.Table_Type'Class
   is
   begin
      return Table_Vectors.Element (Table_Vectors.Cursor (Position)).all;
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Database : Database_Type;
                     Name     : String)
                     return Kit.Schema.Tables.Table_Type'Class
   is
   begin
      for T of Database.Tables loop
         if T.Name = Name then
            return T.all;
         end if;
      end loop;
      raise Constraint_Error with
        "table " & Name & " not found in database " & Database.Name;
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Database : Database_Type;
                     Index    : Positive)
                     return Kit.Schema.Tables.Table_Type'Class
   is
   begin
      return Database.Tables.Element (Index).all;
   end Element;

   -----------------
   -- First_Table --
   -----------------

   function First_Table (Database : Database_Type) return Table_Cursor is
   begin
      return Table_Cursor (Database.Tables.First);
   end First_Table;

   -----------------------
   -- Has_Display_Field --
   -----------------------

   function Has_Display_Field (Db : Database_Type) return Boolean is
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

   procedure Iterate (Database : Database_Type;
                      Process  : not null access
                        procedure (Table : Kit.Schema.Tables.Table_Type'Class))
   is
      procedure Call_Process (Position : Table_Vectors.Cursor);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Position : Table_Vectors.Cursor) is
      begin
         Process (Table_Vectors.Element (Position).all);
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

   -----------------
   -- Table_Count --
   -----------------

   function Table_Count (Db : Database_Type) return Natural is
   begin
      return Db.Tables.Last_Index;
   end Table_Count;

   -------------------
   -- With_Database --
   -------------------

   procedure With_Database (Db     : in out Database_Type'Class;
                            Withed : in     Database_Type'Class)
   is
   begin
      for T of Withed.Tables loop
         Db.Append (T.all);
      end loop;
   end With_Database;

   -----------
   -- Write --
   -----------

   procedure Write (Db     : Database_Type;
                    Writer : in out Aquarius.Writer.Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("package " & Db.Name & " is");
      Writer.Put_Line ("end " & Db.Name & ";");
   end Write;

end Kit.Schema.Databases;
