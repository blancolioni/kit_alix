with Ada.Containers.Vectors;

with Kit.Server.Tables;

with Leander.Builtin;

package body Kit.Server.SK_Bindings is

   package Database_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Database_Record);

   Active_Records : Database_Record_Vectors.Vector;

   function Activate (Item : Database_Record)
                      return Positive;

   procedure Deactivate (Handle : Positive);

   function Evaluate_Get_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Close_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   function Evaluate_Get_Field
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object;

   --------------
   -- Activate --
   --------------

   function Activate (Item : Database_Record)
                      return Positive
   is
      Index : Positive := Active_Records.Last_Index + 1;
   begin
      for I in 1 .. Active_Records.Last_Index loop
         if Active_Records.Element (I) = null then
            Index := I;
            Active_Records.Replace_Element (Index, Item);
            exit;
         end if;
      end loop;

      if Index = Active_Records.Last_Index + 1 then
         Active_Records.Append (Item);
      end if;

      return Index;
   end Activate;

   ------------------------
   -- Create_SK_Bindings --
   ------------------------

   procedure Create_SK_Bindings
     (Machine  : SK.Machine.SK_Machine)
   is
   begin

      SK.Machine.Import_Function
        (Machine, "#getRecord",
         2, Evaluate_Get_Record'Access);
      SK.Machine.Import_Function
        (Machine, "#closeRecord",
         1, Evaluate_Close_Record'Access);
      SK.Machine.Import_Function
        (Machine, "#getField",
         2, Evaluate_Get_Field'Access);
   end Create_SK_Bindings;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Handle : Positive) is
      It : Database_Record := Active_Records.Element (Handle);
   begin
      Close (It);
      Active_Records.Replace_Element (Handle, null);
   end Deactivate;

   ---------------------------
   -- Evaluate_Close_Record --
   ---------------------------

   function Evaluate_Close_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Handle : constant Positive :=
                 SK.Get_Integer
                   (Arguments (Arguments'First));
   begin
      Deactivate (Handle);
      return SK.To_Object (Integer'(0));
   end Evaluate_Close_Record;

   ------------------------
   -- Evaluate_Get_Field --
   ------------------------

   function Evaluate_Get_Field
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      Handle : constant Positive :=
                 SK.Get_Integer (Arguments (Arguments'First));
      Field_Name : constant String :=
                     Leander.Builtin.Object_To_String
                       (Context,
                        Arguments (Arguments'First + 1));
      Result     : constant String :=
                     Active_Records.Element (Handle).Get (Field_Name);
   begin
      return Leander.Builtin.String_To_Object
        (Context, Result);
   end Evaluate_Get_Field;

   -------------------------
   -- Evaluate_Get_Record --
   -------------------------

   function Evaluate_Get_Record
     (Context   : SK.Machine.Function_Call_Context;
      Arguments : SK.Array_Of_Objects)
      return SK.Object
   is
      pragma Unreferenced (Context);
      Table_Index : constant Marlowe.Table_Index :=
                      Marlowe.Table_Index
                        (SK.Get_Integer
                           (Arguments (Arguments'First)));
      Record_Index : constant Marlowe.Database_Index :=
                       Marlowe.Database_Index
                         (SK.Get_Integer
                            (Arguments (Arguments'First + 1)));
      Result       : constant Database_Record :=
                       Kit.Server.Tables.Active_Database.Get
                         (Table_Index, Record_Index);
      Handle : constant Positive := Activate (Result);
   begin
      return SK.To_Object (Handle);
   end Evaluate_Get_Record;

end Kit.Server.SK_Bindings;
