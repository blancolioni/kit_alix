private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;

package body Kit.Notifier is

   package Handler_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Notification_Handler);

   Table_Change_Handlers : Handler_Lists.List;
   New_Record_Handlers   : Handler_Lists.List;

   package Reference_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Reference_Type,
        Element_Type => Handler_Lists.List,
        "<"          => "<",
        "="          => Handler_Lists."=");

   Record_Change_Handlers : Reference_Maps.Map;
   Record_Delete_Handlers : Reference_Maps.Map;

   ----------------------------
   -- Add_New_Record_Handler --
   ----------------------------

   procedure Add_New_Record_Handler (Handler : Notification_Handler) is
   begin
      New_Record_Handlers.Append (Handler);
   end Add_New_Record_Handler;

   -------------------------------
   -- Add_Record_Change_Handler --
   -------------------------------

   procedure Add_Record_Change_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler)
   is
   begin
      Record_Change_Handlers (Reference).Append (Handler);
   end Add_Record_Change_Handler;

   -------------------------------
   -- Add_Record_Delete_Handler --
   -------------------------------

   procedure Add_Record_Delete_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler)
   is
   begin
      Record_Delete_Handlers (Reference).Append (Handler);
   end Add_Record_Delete_Handler;

   ------------------------------
   -- Add_Table_Change_Handler --
   ------------------------------

   procedure Add_Table_Change_Handler (Handler : Notification_Handler) is
   begin
      Table_Change_Handlers.Append (Handler);
   end Add_Table_Change_Handler;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (Reference : Reference_Type)
   is
      Position : constant Reference_Maps.Cursor :=
                   Record_Change_Handlers.Find (Reference);
   begin
      for Handler of Table_Change_Handlers loop
         Handler (Reference);
      end loop;
      if Reference_Maps.Has_Element (Position) then
         for Handler of Record_Change_Handlers (Position) loop
            Handler (Reference);
         end loop;
      end if;

   end On_Changed;

   -------------------------------
   -- Remove_New_Record_Handler --
   -------------------------------

   procedure Remove_New_Record_Handler (Handler : Notification_Handler) is
      Position : Handler_Lists.Cursor := New_Record_Handlers.Find (Handler);
   begin
      New_Record_Handlers.Delete (Position);
   end Remove_New_Record_Handler;

   ----------------------------------
   -- Remove_Record_Change_Handler --
   ----------------------------------

   procedure Remove_Record_Change_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler)
   is
      List : Handler_Lists.List renames
               Record_Change_Handlers
                 (Record_Change_Handlers.Find (Reference));
      Position : Handler_Lists.Cursor := List.Find (Handler);
   begin
      List.Delete (Position);
   end Remove_Record_Change_Handler;

   ----------------------------------
   -- Remove_Record_Delete_Handler --
   ----------------------------------

   procedure Remove_Record_Delete_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler)
   is
      List     : Handler_Lists.List renames
                   Record_Change_Handlers
                     (Record_Delete_Handlers.Find (Reference));
      Position : Handler_Lists.Cursor := List.Find (Handler);
   begin
      List.Delete (Position);
   end Remove_Record_Delete_Handler;

   ---------------------------------
   -- Remove_Table_Change_Handler --
   ---------------------------------

   procedure Remove_Table_Change_Handler (Handler : Notification_Handler) is
      Position : Handler_Lists.Cursor := Table_Change_Handlers.Find (Handler);
   begin
      Table_Change_Handlers.Delete (Position);
   end Remove_Table_Change_Handler;

end Kit.Notifier;
