private with Ada.Finalization;
private with Ada.Strings.Unbounded;

with Marlowe.Btree_Handles;

package Kit.Access_Control is

   Client_Side : constant Boolean := False;
   Server_Side : constant Boolean := True;

   type Access_Handle is tagged private;

   procedure Logon (Host     : in     String;
                    Name     : in     String;
                    Password : in     String;
                    Handle   :    out Access_Handle;
                    Contact  :    out Boolean;
                    Success  :    out Boolean);

   function Automatic_Logon (Name    : String) return Access_Handle;

   function Valid (A : Access_Handle)
                  return Boolean;

   procedure Logout (A : in out Access_Handle);

   function Get_Login_Name (From : Access_Handle) return String;

   function Send_Message (Handle  : Access_Handle;
                          Message : String)
                         return String;

   procedure Send_Message (Handle  : Access_Handle;
                           Message : String);

   type Notifier is access
     procedure (Handle : in Access_Handle;
                Table  : in String;
                Index  : in Marlowe.Database_Index);

private

   type Access_Handle is
     new Ada.Finalization.Controlled with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Kit.Access_Control;
