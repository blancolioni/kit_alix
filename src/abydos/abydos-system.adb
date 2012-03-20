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
      Key_Value  : constant String := To_String (Args.Item (3));
      Table      : constant Marlowe.Table_Index :=
                     Env.To_Table_Index (Table_Name);

      function First_Result return Kit.Root_Database_Record'Class;

      ------------------
      -- First_Result --
      ------------------

      function First_Result return Kit.Root_Database_Record'Class is
      begin
         if Key_Name = "" then
            return Env.Get (Table, 1);
         elsif Key_Value = "" then
            return Env.First_By_Key (Table, Key_Name);
         else
            return Env.First_By_Key_Value
              (Table, Key_Name, Key_Value);
         end if;
      end First_Result;

      Rec        : Kit.Root_Database_Record'Class := First_Result;
      Count      : Natural := 0;
   begin
      while Rec.Has_Element loop
         Ada.Text_IO.Put_Line
           (Marlowe.Database_Index'Image (Rec.Index));
         Rec.Next;
         Count := Count + 1;
      end loop;
      return To_Value (Count);
   end Report;

end Abydos.System;
