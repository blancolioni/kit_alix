with Ada.Finalization;
with Ada.Strings.Unbounded;

with Marlowe.Tables;
with Marlowe.Keys;

with Kit.Locking;

with Harriet.Named_Object;

private package Harriet.Named_Object_Impl is

   type Named_Object_Database_Record is
      record
         Ref  : Named_Object_Reference;
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Read
     (Ref                 : Named_Object_Reference;
      Named_Object_Record : out Named_Object_Database_Record);

   procedure Write
     (Ref                 : Named_Object_Reference;
      Named_Object_Record : Named_Object_Database_Record);

   function Create return Named_Object_Reference;

   package Named_Object_Table is
      new Marlowe.Tables (Named_Object_Database_Record);

   function Named_Object_Get_Name (Named_Object : Named_Object_Database_Record)
                                  return Ada.Strings.Unbounded.Unbounded_String;

   package Named_Object_Name_Key is
      new Marlowe.Keys (Ada.Strings.Unbounded.Unbounded_String,
                        True,
                        Ada.Strings.Unbounded."=",
                        Ada.Strings.Unbounded."<",
                        Named_Object_Table,
                        Named_Object_Get_Name);

   type Named_Object_Implementation is
     new Kit.Locking.Root_Lockable_Type
     and Harriet.Named_Object.Named_Object_Interface with
      record
         Valid               : Boolean  := False;
         Dirty               : Boolean  := False;
         Created             : Boolean  := False;
         Searching           : Boolean  := False;
         Ascending           : Boolean  := True;
         Search_Key          : Key_Type := K_No_Key;
         Current_Index       : Marlowe.Database_Index;
         Name_Key_Handle     : Named_Object_Name_Key.Key_Handle;
         Named_Object_Record : Named_Object_Database_Record;
      end record;

   procedure Finalize (Item : in out Named_Object_Implementation);

   function Name (Item : Named_Object_Implementation) return String;

   procedure Set_Name (Item  : in out Named_Object_Implementation;
                       Value : in     String);

   function Has_Element
     (Item : Named_Object_Implementation)
      return Boolean ;

   procedure Next
     (Item : in out Named_Object_Implementation);

end Harriet.Named_Object_Impl;
