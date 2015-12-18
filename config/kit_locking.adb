with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Kit.Mutex;

package body {database}.Kit_Locking is

   package List_Of_Request_Ids is
     new Ada.Containers.Doubly_Linked_Lists (Request_Id);

   Free_Ids : List_Of_Request_Ids.List;

   type Granted_Lock_Record is
      record
         Active   : Boolean;
         Granted  : Boolean;
         Time     : Ada.Calendar.Time;
         Table    : Table_Names.Table_Reference;
         Index    : Marlowe.Database_Index;
         Location : Positive;
      end record;

   package Granted_Lock_Vectors is
     new Ada.Containers.Vectors (Real_Request_Id, Granted_Lock_Record);

   Granted_Locks : Granted_Lock_Vectors.Vector;

   Grant_Mutex : Kit.Mutex.Mutex_Type;

   procedure Check_Deadlock;
   procedure Report_Problem
     (Id : Request_Id;
      Message : String);

   task Deadlock_Scanner is
      entry Start;
      entry Stop;
   end Deadlock_Scanner;

   --------------------
   -- Check_Deadlock --
   --------------------

   procedure Check_Deadlock is
      use Ada.Calendar, Ada.Containers;
      Waiting : List_Of_Request_Ids.List;
   begin
      Grant_Mutex.Lock;
      for Id in 1 .. Granted_Locks.Last_Index loop
         declare
            Grant : Granted_Lock_Record renames Granted_Locks (Id);
         begin
            if Grant.Active
              and then not Grant.Granted
              and then Ada.Calendar.Clock - Grant.Time > 4.0
            then
               Waiting.Append (Id);
            end if;
         end;
      end loop;

      if not Waiting.Is_Empty then
         if Waiting.Length = 1 then
            Report_Problem (Waiting.First_Element,
                            "suspiciously long wait");
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Probable deadlock detected; waiting locks follow");
            for Id of Waiting loop
               Report_Problem (Id, "Waiting lock");
            end loop;
         end if;
      end if;
      Grant_Mutex.Unlock;
   end Check_Deadlock;

   ----------------------
   -- Deadlock_Scanner --
   ----------------------

   task body Deadlock_Scanner is
   begin
      loop
         select
            accept Start;
            loop
               select
                  accept Stop;
                  exit;
               else
                  delay 5.0;
                  Check_Deadlock;
               end select;
            end loop;
         or
            terminate;
         end select;
      end loop;
   end Deadlock_Scanner;

   --------------
   -- Got_Lock --
   --------------

   procedure Got_Lock (Id : Request_Id) is
   begin
      Grant_Mutex.Lock;
      Granted_Locks (Id).Granted := True;
      Grant_Mutex.Unlock;
   end Got_Lock;

   ------------------
   -- Release_Lock --
   ------------------

   procedure Release_Lock (Id : in out Request_Id) is
   begin
      Grant_Mutex.Lock;
      Granted_Locks (Id).Active := False;
      Granted_Locks (Id).Granted := False;
      Free_Ids.Append (Id);
      Id := No_Request;
      Grant_Mutex.Unlock;
   end Release_Lock;

   --------------------
   -- Report_Problem --
   --------------------

   procedure Report_Problem
     (Id : Request_Id;
      Message : String)
   is
      Grant : Granted_Lock_Record renames Granted_Locks (Id);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Message
         & ": "
         & Table_Names.Table_Name (Grant.Table)
         & Marlowe.Database_Index'Image (Grant.Index)
         & ": "
         & Table_Names.Table_Source (Grant.Table)
         & ":"
         & Ada.Strings.Fixed.Trim
           (Positive'Image (Grant.Location),
            Ada.Strings.Both));
   end Report_Problem;

   ------------------
   -- Request_Lock --
   ------------------

   function Request_Lock
     (Table     : Table_Names.Table_Reference;
      Index     : Marlowe.Database_Index;
      Location  : Positive)
      return Request_Id
   is
      Lock : constant Granted_Lock_Record :=
               (True, False, Ada.Calendar.Clock, Table, Index, Location);
      Result : Request_Id;
   begin

      Grant_Mutex.Lock;

      if Free_Ids.Is_Empty then
         Granted_Locks.Append (Lock);
         Result := Granted_Locks.Last_Index;
      else
         Result := Free_Ids.First_Element;
         Free_Ids.Delete_First;
         Granted_Locks (Result) := Lock;
      end if;

      Grant_Mutex.Unlock;

      return Result;

   end Request_Lock;

   --------------------
   -- Start_Scanning --
   --------------------

   procedure Start_Scanning is
   begin
      Deadlock_Scanner.Start;
   end Start_Scanning;

   -------------------
   -- Stop_Scanning --
   -------------------

   procedure Stop_Scanning is
   begin
      Deadlock_Scanner.Stop;
   end Stop_Scanning;

end {database}.Kit_Locking;
