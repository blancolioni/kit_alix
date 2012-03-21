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

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Top : Environments.Environment) is
   begin
      Top.Insert ("listnames", System_Program'(Exec => List_Names'Access));
      Top.Insert ("report", System_Program'(Exec => Report'Access));
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

      Rec        : constant Kit.Database_Record := First_Result;
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
      Rec.Close;
      return To_Value (Count);
   end Report;

end Abydos.System;
