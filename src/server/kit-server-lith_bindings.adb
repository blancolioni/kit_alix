with Kit.Db.Database;
with Kit.Db.Kit_Record;

with Kit.Db.Tables;

with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

package body Kit.Server.Lith_Bindings is

   function Evaluate_Attach
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Detach
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Table_Fields
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Table_List
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Table_Select
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Table_Select_All
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   procedure Push_Record
     (Store : in out Lith.Objects.Object_Store'Class;
      Item  : Kit.Db.Tables.Database_Record'Class);

   ---------------------
   -- Create_Bindings --
   ---------------------

   procedure Create_Bindings is
   begin
      Lith.Objects.Interfaces.Define_Function
        (Name           => "attach",
         Argument_Count => 1,
         Eval           => Evaluate_Attach'Access);
      Lith.Objects.Interfaces.Define_Function
        (Name           => "detach",
         Argument_Count => 0,
         Eval           => Evaluate_Detach'Access);
      Lith.Objects.Interfaces.Define_Function
        (Name           => "table-fields",
         Argument_Count => 1,
         Eval           => Evaluate_Table_Fields'Access);
      Lith.Objects.Interfaces.Define_Function
        (Name           => "table-list",
         Argument_Count => 0,
         Eval           => Evaluate_Table_List'Access);
      Lith.Objects.Interfaces.Define_Function
        (Name           => "table-select",
         Argument_Count => 4,
         Eval           => Evaluate_Table_Select'Access);
      Lith.Objects.Interfaces.Define_Function
        (Name           => "table-select-all",
         Argument_Count => 2,
         Eval           => Evaluate_Table_Select_All'Access);
   end Create_Bindings;

   ---------------------
   -- Evaluate_Attach --
   ---------------------

   function Evaluate_Attach
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      if False then
         Kit.Db.Database.Open (Store.To_String (Store.Argument (1)));
      end if;
      return Lith.Objects.No_Value;
   end Evaluate_Attach;

   ---------------------
   -- Evaluate_Detach --
   ---------------------

   function Evaluate_Detach
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
   begin
      Kit.Db.Database.Close;
      return Lith.Objects.No_Value;
   end Evaluate_Detach;

   ---------------------------
   -- Evaluate_Table_Fields --
   ---------------------------

   function Evaluate_Table_Fields
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Table_Name : constant String :=
                     Lith.Objects.Symbols.Get_Name
                       (Lith.Objects.To_Symbol
                          (Store.Argument (1)));
      Table : constant Kit.Db.Tables.Database_Table :=
                Kit.Db.Tables.Get_Table (Table_Name);
      Count      : Natural := 0;
   begin

      if Table.Has_Element then
         Count := Table.Field_Count;
         for I in 1 .. Table.Field_Count loop
            Store.Push
              (Lith.Objects.To_Object
                 (Lith.Objects.Symbols.Get_Symbol
                      (Table.Field_Name (I))));
         end loop;
      end if;

      Store.Push (Lith.Objects.Nil);

      for I in 1 .. Count loop
         Store.Cons;
      end loop;

      return Store.Pop;
   end Evaluate_Table_Fields;

   -------------------------
   -- Evaluate_Table_List --
   -------------------------

   function Evaluate_Table_List
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Marlowe;
      Count : Natural := 0;
   begin

      for Table of Kit.Db.Kit_Record.Select_By_Name loop
         declare
            Name : constant String := Table.Name;
         begin
            if Name'Length <= 4
              or else Name (1 .. 4) /= "kit_"
            then
               Store.Push
                 (Lith.Objects.To_Object
                    (Lith.Objects.Symbols.Get_Symbol
                         (Table.Name)));
               Count := Count + 1;
            end if;
         end;
      end loop;

      Store.Push (Lith.Objects.Nil);

      for I in 1 .. Count loop
         Store.Cons;
      end loop;

      return Store.Pop;
   end Evaluate_Table_List;

   ---------------------------
   -- Evaluate_Table_Select --
   ---------------------------

   function Evaluate_Table_Select
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Table_Name : constant String :=
                     Lith.Objects.Symbols.Get_Name
                       (Lith.Objects.To_Symbol
                          (Store.Argument (1)));
      Key_Name   : constant String :=
                     Lith.Objects.Symbols.Get_Name
                       (Lith.Objects.To_Symbol
                          (Store.Argument (2)));
      Key_Value   : constant String :=
                      Lith.Objects.Symbols.Get_Name
                        (Lith.Objects.To_Symbol
                           (Store.Argument (3)));
      Table : constant Kit.Db.Tables.Database_Table :=
                      Kit.Db.Tables.Get_Table (Table_Name);
      Count : Natural := 0;
   begin
      if Table.Has_Element then
         declare
            Refs : constant Kit.Db.Tables.Array_Of_References :=
                     Table.Select_By (Key_Name, Key_Value);
         begin
            for I in Refs'Range loop
               Push_Record (Store,
                            Table.Get (Refs (I)));
            end loop;
            Count := Refs'Length;
         end;
      end if;

      Store.Push (Lith.Objects.Nil);
      for I in 1 .. Count loop
         Store.Cons;
      end loop;

      return Store.Pop;
   end Evaluate_Table_Select;

   -------------------------------
   -- Evaluate_Table_Select_All --
   -------------------------------

   function Evaluate_Table_Select_All
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Table_Name : constant String :=
                     Lith.Objects.Symbols.Get_Name
                       (Lith.Objects.To_Symbol
                          (Store.Argument (1)));
      Table : constant Kit.Db.Tables.Database_Table :=
                      Kit.Db.Tables.Get_Table (Table_Name);
      Key_Name   : constant String :=
        (if Store.Argument_Count > 1
         then Lith.Objects.Symbols.Get_Name
           (Lith.Objects.To_Symbol
              (Store.Argument (2)))
         else Table.Default_Key);

      Key_Value   : constant String :=
                      (if Store.Argument_Count > 2
                       then Lith.Objects.Symbols.Get_Name
                         (Lith.Objects.To_Symbol
                            (Store.Argument (3)))
                       else "");
      Count      : Natural := 0;

      procedure Push_Selected_Record
        (Item : Kit.Db.Tables.Database_Record'Class);

      --------------------------
      -- Push_Selected_Record --
      --------------------------

      procedure Push_Selected_Record
        (Item : Kit.Db.Tables.Database_Record'Class)
      is
      begin
         Push_Record (Store, Item);
         Count := Count + 1;
      end Push_Selected_Record;

   begin
      if Table.Has_Element then
         if Key_Value /= "" then
            Table.Iterate (Key_Name, Key_Value, Push_Selected_Record'Access);
         else
            Table.Iterate (Key_Name, Push_Selected_Record'Access);
         end if;
      end if;

      Store.Push (Lith.Objects.Nil);
      for I in 1 .. Count loop
         Store.Cons;
      end loop;

      return Store.Pop;
   end Evaluate_Table_Select_All;

   -----------------
   -- Push_Record --
   -----------------

   procedure Push_Record
     (Store : in out Lith.Objects.Object_Store'Class;
      Item  : Kit.Db.Tables.Database_Record'Class)
   is
   begin
      for I in 1 .. Item.Field_Count loop
         Store.Push
           (Lith.Objects.To_Object
              (Lith.Objects.Symbols.Get_Symbol
                   (Item.Field_Name (I))));
         Store.Push
           (Lith.Objects.To_Object
              (Lith.Objects.Symbols.Get_Symbol
                   (Item.Get (I))));
         Store.Push (Lith.Objects.Nil);
         Store.Cons;
         Store.Cons;
      end loop;

      Store.Push (Lith.Objects.Nil);
      for I in 1 .. Item.Field_Count loop
         Store.Cons;
      end loop;

   end Push_Record;

end Kit.Server.Lith_Bindings;
