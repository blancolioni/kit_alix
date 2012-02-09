package body Harriet.Named_Object_Impl is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Item : in out Named_Object_Implementation) is
   begin
      if Item.Dirty then
         Named_Object_Table.Write
           (Marlowe.Database_Index (Item.Named_Object_Record.Ref),
            Item.Named_Object_Record);
         if Item.Created then
            Named_Object_Name_Key.Insert
              (Item.Named_Object_Record.Name,
               Marlowe.Database_Index (Item.Named_Object_Record.Ref));
         end if;
      end if;
      Item.Valid := False;
      Item.Dirty := False;
      Item.Searching := False;
   end Finalize;

   ---------------------------
   -- Named_Object_Get_Name --
   ---------------------------

   function Named_Object_Get_Name (Named_Object : Named_Object_Database_Record)
                                  return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Named_Object.Name;
   end Named_Object_Get_Name;

   ----------
   -- Read --
   ----------

   procedure Read
     (Ref                 : Named_Object_Reference;
      Named_Object_Record : out Named_Object_Database_Record)
   is
   begin
      Named_Object_Table.Read (Marlowe.Database_Index (Ref), Named_Object_Record);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ref                 : Named_Object_Reference;
      Named_Object_Record : Named_Object_Database_Record)
   is
   begin
      Named_Object_Table.Write (Marlowe.Database_Index (Ref), Named_Object_Record);
   end Write;

   ------------
   -- Create --
   ------------

   function Create return Named_Object_Reference is
   begin
      return Named_Object_Reference (Named_Object_Table.New_Record);
   end Create;

   ----------
   -- Name --
   ----------

   function Name (Item : Named_Object_Implementation) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Named_Object_Record.Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Item  : in out Named_Object_Implementation;
      Value : in     String)
   is
      use Ada.Strings.Unbounded;
   begin
      Item.X_Lock;
      if Item.Named_Object_Record.Name /= Value then
         declare
            New_Value : constant Unbounded_String :=
              To_Unbounded_String (Value);
         begin
            if not Item.Created then
               Named_Object_Name_Key.Replace
                 (Item.Named_Object_Record.Name,
                  New_Value,
                  Marlowe.Database_Index (Item.Named_Object_Record.Ref));
            end if;

            Item.Named_Object_Record.Name := New_Value;
            Item.Dirty := True;
         end;
      end if;
   end Set_Name;

   ----------
   -- More --
   ----------

   procedure Next
     (Item : in out Named_Object_Implementation)
   is
      use type Marlowe.Database_Index;
   begin
      if not Item.Searching then
         return;
      end if;
      case Item.Search_Key is
         when K_No_Key =>
            Item.Current_Index := Item.Current_Index + 1;
            Item.Valid :=
              Item.Current_Index <= Named_Object_Table.Last_Record_Index;

         when K_Name =>
            Named_Object_Name_Key.Next (Item.Name_Key_Handle);
            Item.Valid := Named_Object_Name_Key.Valid (Item.Name_Key_Handle);
            if Item.Valid then
               Item.Current_Index := Named_Object_Name_Key.Reference (Item.Name_Key_Handle);
            end if;

      end case;
      if Item.Valid then
         Named_Object_Table.Read (Item.Current_Index,
                                  Item.Named_Object_Record);
      end if;

   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Item : Named_Object_Implementation)
      return Boolean
   is
   begin
      return Item.Valid;
   end Has_Element;

end Harriet.Named_Object_Impl;
