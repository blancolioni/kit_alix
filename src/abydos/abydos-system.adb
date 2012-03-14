with Ada.Text_IO;

with Kit.Db.Kit_Record;

package body Abydos.System is

   function List_Names (Args : Values.Array_Of_Values;
                        Env  : Environments.Environment'Class)
                        return Values.Value;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Item : System_Program;
      Args : Values.Array_Of_Values;
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
   end Initialise;

   ----------------
   -- List_Names --
   ----------------

   function List_Names (Args : Values.Array_Of_Values;
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

end Abydos.System;
