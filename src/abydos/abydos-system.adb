with Ada.Text_IO;

with Marlowe;
with Kit.Db.Kit_Record;

package body Abydos.System is

   function List_Names (Args : Environments.Argument_List'Class;
                        Env  : Environments.Environment'Class)
                        return Values.Value;

   function Report (Args : Environments.Argument_List'Class;
                    Env  : Environments.Environment'Class)
                    return Values.Value;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate
     (Item : System_Program;
      Args : Environments.Argument_List'Class;
      Env  : Environments.Environment'Class)
      return Values.Value
   is
   begin
      return Item.Exec (Args, Env);
   end Evaluate;

   ---------------------------
   -- Formal_Argument_Count --
   ---------------------------

   overriding
   function Formal_Argument_Count
     (Item : System_Program)
      return Natural
   is
   begin
      return Item.Args.Last_Index;
   end Formal_Argument_Count;

   --------------------------
   -- Formal_Argument_Name --
   --------------------------

   overriding
   function Formal_Argument_Name
     (Item : System_Program;
      Index : Positive)
      return String
   is
   begin
      return Item.Args.Element (Index);
   end Formal_Argument_Name;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Top : Environments.Environment) is
      Report_Arguments : Formal_Argument_Vectors.Vector;
   begin
      Report_Arguments.Append ("table");
      Report_Arguments.Append ("key");

      Top.Insert
        ("listnames",
         System_Program'
           (List_Names'Access,
            Formal_Argument_Vectors.Empty_Vector));
      Top.Insert
        ("report",
         System_Program'
           (Report'Access,
            Report_Arguments));
   end Initialise;

   ----------------
   -- List_Names --
   ----------------

   function List_Names (Args : Environments.Argument_List'Class;
                        Env  : Environments.Environment'Class)
                        return Values.Value
   is
      pragma Unreferenced (Args);
      pragma Unreferenced (Env);
      use Kit.Db.Kit_Record;
      Rec : Kit_Record_Type := First_By_Name;
   begin
      while Rec.Has_Element loop
         Ada.Text_IO.Put_Line (Rec.Name);
         Rec.Next;
      end loop;
      return Values.Null_Value;
   end List_Names;

   ------------
   -- Report --
   ------------

   function Report (Args : Environments.Argument_List'Class;
                    Env  : Environments.Environment'Class)
                    return Values.Value
   is
      use Abydos.Values;
      Table_Name : constant String := To_String (Args.Item (1));
      Key_Name   : constant String := To_String (Args.Item (2));
      Table      : constant Marlowe.Table_Index :=
                     Env.To_Table_Index (Table_Name);

      function First_Result return Kit.Database_Record;

      ------------------
      -- First_Result --
      ------------------

      function First_Result return Kit.Database_Record is
      begin
         if Key_Name = "" then
            return Env.Get (Table, 1);
         else
            return Env.First_By_Key (Table, Key_Name);
         end if;
      end First_Result;

      Rec        : Kit.Database_Record := First_Result;
      Count      : Natural := 0;
   begin
      while Rec.Has_Element loop
         Ada.Text_IO.Put (Rec.Get (To_String (Args.Item (2))));
         for I in 3 .. 10 loop
            exit when To_String (Args.Item (I)) = "";
            Ada.Text_IO.Put (' ' & Rec.Get (To_String (Args.Item (I))));
         end loop;
         Ada.Text_IO.New_Line;

         Rec.Next;
         Count := Count + 1;
      end loop;
      Kit.Close (Rec);
      return To_Value (Count);
   end Report;

end Abydos.System;
