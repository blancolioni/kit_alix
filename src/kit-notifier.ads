generic
   type Reference_Type is private;
   with function "<" (Left, Right : Reference_Type) return Boolean is <>;
package Kit.Notifier is

   type Notification_Handler is access
     procedure (Reference : Reference_Type);

   procedure Add_Table_Change_Handler (Handler : Notification_Handler);
   procedure Add_New_Record_Handler (Handler : Notification_Handler);

   procedure Add_Record_Change_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler);
   procedure Add_Record_Delete_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler);

   procedure Remove_Table_Change_Handler (Handler : Notification_Handler);
   procedure Remove_New_Record_Handler (Handler : Notification_Handler);
   procedure Remove_Record_Change_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler);
   procedure Remove_Record_Delete_Handler
     (Reference : Reference_Type;
      Handler   : Notification_Handler);

   procedure On_Changed
     (Reference : Reference_Type);

end Kit.Notifier;
