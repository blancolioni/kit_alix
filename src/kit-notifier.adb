private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;

package body Kit.Notifier is

   package Table_Notify_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Table_Notify_Interface'Class);

   package Record_Notify_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Record_Notify_Interface'Class);

   type Record_Notify_Entry is
      record
         On_Record_Changed : Record_Notify_Lists.List;
      end record;

   package Record_Notify_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Marlowe.Database_Index,
        Element_Type => Record_Notify_Entry,
        "<"          => Marlowe."<");

   type Table_Notifier_Entry is
      record
         On_Table_Change  : Table_Notify_Lists.List;
         On_New_Record    : Record_Notify_Lists.List;
         On_Delete_Record : Record_Notify_Lists.List;
         Record_Notifiers : Record_Notify_Maps.Map;
      end record;

   type Table_Notifier_Index is range 1 .. Marlowe.Table_Index'Last;

   package Table_Notifier_Vectors is
     new Ada.Containers.Vectors (Table_Notifier_Index, Table_Notifier_Entry);

   type Table_Notification is (Table_Changed);
   type Record_Notification is (Record_Added, Record_Deleted,
                                Record_Changed);

   procedure Add
     (Vector       : in out Table_Notifier_Vectors.Vector;
      Notification : Table_Notification;
      Table        : Marlowe.Table_Index;
      Handler      : Table_Notify_Interface'Class);

   procedure Add
     (Vector       : in out Table_Notifier_Vectors.Vector;
      Notification : Record_Notification;
      Table        : Marlowe.Table_Index;
      Index        : Marlowe.Database_Index;
      Handler      : Record_Notify_Interface'Class);

   procedure Handle
     (Vector       : Table_Notifier_Vectors.Vector;
      Notification : Table_Notification;
      Table        : Marlowe.Table_Index);

   procedure Handle
     (Vector       : Table_Notifier_Vectors.Vector;
      Notification : Record_Notification;
      Table        : Marlowe.Table_Index;
      Index        : Marlowe.Database_Index);

   task Notifier_Task is
      entry Add_Table_Notification
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index;
         Handler      : Table_Notify_Interface'Class);
      entry Add_Record_Notification
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index;
         Handler      : Record_Notify_Interface'Class);

      entry Handle_Table_Notification
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index);

      entry Handle_Record_Notification
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index);
   end Notifier_Task;

   ---------
   -- Add --
   ---------

   procedure Add
     (Vector       : in out Table_Notifier_Vectors.Vector;
      Notification : Table_Notification;
      Table        : Marlowe.Table_Index;
      Handler      : Table_Notify_Interface'Class)
   is
      Notifier_Index : constant Table_Notifier_Index :=
                         Table_Notifier_Index (Table);
   begin
      while Vector.Last_Index < Notifier_Index loop
         Vector.Append ((others => <>));
      end loop;

      case Notification is
         when Table_Changed =>
            Vector (Notifier_Index).On_Table_Change.Append (Handler);
      end case;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Vector       : in out Table_Notifier_Vectors.Vector;
      Notification : Record_Notification;
      Table        : Marlowe.Table_Index;
      Index        : Marlowe.Database_Index;
      Handler      : Record_Notify_Interface'Class)
   is
      Notifier_Index : constant Table_Notifier_Index :=
                         Table_Notifier_Index (Table);
   begin
      while Vector.Last_Index < Notifier_Index loop
         Vector.Append ((others => <>));
      end loop;

      case Notification is
         when Record_Added =>
            Vector (Notifier_Index).On_New_Record.Append (Handler);
         when Record_Deleted =>
            Vector (Notifier_Index).On_Delete_Record.Append (Handler);
         when Record_Changed =>
            declare
               Map : Record_Notify_Maps.Map renames
                       Vector (Notifier_Index).Record_Notifiers;
               Position : constant Record_Notify_Maps.Cursor :=
                            Map.Find (Index);
            begin
               if not Record_Notify_Maps.Has_Element (Position) then
                  declare
                     List : Record_Notify_Lists.List;
                  begin
                     List.Append (Handler);
                     Map.Insert (Index, (On_Record_Changed => List));
                  end;
               else
                  Map (Position).On_Record_Changed.Append (Handler);
               end if;
            end;
      end case;
   end Add;

   -------------------------------
   -- Add_Record_Change_Handler --
   -------------------------------

   procedure Add_Record_Change_Handler
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Notifier_Task.Add_Record_Notification
        (Notification => Record_Changed,
         Table        => Table,
         Index        => Index,
         Handler      => Handler);
   end Add_Record_Change_Handler;

   -------------------------------
   -- Add_Record_Create_Handler --
   -------------------------------

   procedure Add_Record_Create_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Notifier_Task.Add_Record_Notification
        (Notification => Record_Added,
         Table        => Table,
         Index        => 0,
         Handler      => Handler);
   end Add_Record_Create_Handler;

   -------------------------------
   -- Add_Delete_Record_Handler --
   -------------------------------

   procedure Add_Record_Delete_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Notifier_Task.Add_Record_Notification
        (Notification => Record_Deleted,
         Table        => Table,
         Index        => 0,
         Handler      => Handler);
   end Add_Record_Delete_Handler;

   ------------------------------
   -- Add_Table_Change_Handler --
   ------------------------------

   procedure Add_Table_Change_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Table_Notify_Interface'Class)
   is
   begin
      Notifier_Task.Add_Table_Notification
        (Notification => Table_Changed,
         Table        => Table,
         Handler      => Handler);
   end Add_Table_Change_Handler;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Vector       : Table_Notifier_Vectors.Vector;
      Notification : Table_Notification;
      Table        : Marlowe.Table_Index)
   is
      Notifier_Index : constant Table_Notifier_Index :=
                         Table_Notifier_Index (Table);
   begin
      if Vector.Last_Index >= Notifier_Index then
         case Notification is
            when Table_Changed =>
               for Handler of
                 Vector (Notifier_Index).On_Table_Change
               loop
                  Handler.Notify_Table_Change;
               end loop;
         end case;
      end if;
   end Handle;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Vector       : Table_Notifier_Vectors.Vector;
      Notification : Record_Notification;
      Table        : Marlowe.Table_Index;
      Index        : Marlowe.Database_Index)
   is
      Notifier_Index : constant Table_Notifier_Index :=
                         Table_Notifier_Index (Table);
   begin
      if Vector.Last_Index >= Notifier_Index then
         case Notification is
            when Record_Added =>
               for Handler of
                 Vector (Notifier_Index).On_New_Record
               loop
                  Handler.Notify_Record_Change (Index);
               end loop;
            when Record_Deleted =>
               for Handler of
                 Vector (Notifier_Index).On_Delete_Record
               loop
                  Handler.Notify_Record_Change (Index);
               end loop;
            when Record_Changed =>
               declare
                  Map      : Record_Notify_Maps.Map renames
                               Vector (Notifier_Index).Record_Notifiers;
                  Position : constant Record_Notify_Maps.Cursor :=
                               Map.Find (Index);
               begin
                  if Record_Notify_Maps.Has_Element (Position) then
                     for Handler of
                       Map (Position).On_Record_Changed
                     loop
                        Handler.Notify_Record_Change (Index);
                     end loop;
                  end if;
               end;
         end case;
      end if;
   end Handle;

   -------------------
   -- Notifier_Task --
   -------------------

   task body Notifier_Task is
      Vector : Table_Notifier_Vectors.Vector;
      Local_Table_Notification  : Table_Notification;
      Local_Record_Notification : Record_Notification;
      Local_Table               : Marlowe.Table_Index;
      Local_Record_Index        : Marlowe.Database_Index;
   begin
      loop
         select
            accept Add_Table_Notification
              (Notification : in Table_Notification;
               Table : in Marlowe.Table_Index;
               Handler : in Table_Notify_Interface'Class)
            do
               Add (Vector, Notification, Table, Handler);
            end Add_Table_Notification;
         or
            accept Add_Record_Notification
              (Notification : in Record_Notification;
               Table : in Marlowe.Table_Index;
               Index  : Marlowe.Database_Index;
               Handler : in Record_Notify_Interface'Class)
            do
               Add (Vector, Notification, Table, Index, Handler);
            end Add_Record_Notification;
         or
            accept Handle_Table_Notification
              (Notification : in Table_Notification;
               Table        : in Marlowe.Table_Index)
            do
               Local_Table_Notification := Notification;
               Local_Table := Table;
            end Handle_Table_Notification;
            Handle (Vector, Local_Table_Notification, Local_Table);
         or
            accept Handle_Record_Notification
              (Notification : in Record_Notification;
               Table        : in Marlowe.Table_Index;
               Index        : in Marlowe.Database_Index)
            do
               Local_Record_Notification := Notification;
               Local_Table := Table;
               Local_Record_Index := Index;
            end Handle_Record_Notification;
            Handle (Vector, Local_Record_Notification,
                    Local_Table, Local_Record_Index);
         or
            terminate;
         end select;
      end loop;
   end Notifier_Task;

   --------------------
   -- Record_Changed --
   --------------------

   procedure Record_Changed
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Changed, Table, Index);
   end Record_Changed;

   --------------------
   -- Record_Created --
   --------------------

   procedure Record_Created
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Added, Table, Index);
   end Record_Created;

   --------------------
   -- Record_Deleted --
   --------------------

   procedure Record_Deleted
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Deleted, Table, Index);
   end Record_Deleted;

   -------------------
   -- Table_Changed --
   -------------------

   procedure Table_Changed
     (Table   : Marlowe.Table_Index)
   is
   begin
      Notifier_Task.Handle_Table_Notification
        (Table_Changed, Table);
   end Table_Changed;

end Kit.Notifier;
