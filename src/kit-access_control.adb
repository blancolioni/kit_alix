with Ada.Unchecked_Deallocation;

with Kit.Mutex;

package body Kit.Access_Control is

   ---------------------
   -- Automatic_Logon --
   ---------------------

   function Automatic_Logon (Name      : String)
                            return Access_Handle
   is
   begin
      return Result : Access_Handle do
          Result.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      end return;
   end Automatic_Logon;

   --------------------
   -- Get_Login_Name --
   --------------------

   function Get_Login_Name (From : Access_Handle) return String is
   begin
      return Ada.Strings.Unbounded.To_String (From.Name);
   end Get_Login_Name;

   -----------
   -- Logon --
   -----------

   procedure Logon (Host     : in     String;
                    Name     : in     String;
                    Password : in     String;
                    Handle   :    out Access_Handle;
                    Contact  :    out Boolean;
                    Success  :    out Boolean)
   is
      pragma Unreferenced (Host);
      User   : System_User.System_User_Type;
   begin

      Contact := True;

      Handle  := new Access_Handle_Record;
      Handle.Login_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);

      System_User.First_By_System_User_Name (User, Handle, Name);

      if not System_User.Exists (User) or else
        System_User.Get_System_User_Password (User) /= Password
      then
         Free (Handle);
         Success := False;
         Handle := null;
      else
         Success := True;
      end if;
   end Logon;

   ------------
   -- Logout --
   ------------

   procedure Logout (A : in out Access_Handle) is
   begin
      Free (A);
      A := null;
   end Logout;

   ------------------
   -- Send_Message --
   ------------------

   function Send_Message (Handle  : Access_Handle;
                          Message : String)
                         return String
   is
   begin
      if Message (Message'First) = '>' then
         return "Leander not supported in this version";

--           if not Leander_Started then
--              Kit_Leander.Start_Leander;
--              Leander_Started := True;
--           end if;
--           return Kit_Leander.Evaluate
--             (Message (Message'First + 1 .. Message'Last));
      else
         declare
            Cmd : constant Commands.Command :=
              Commands.Parser.Parse_Command (Message);
            Result : constant String :=
              Commands.Handlers.Execute_Command (Handle, Cmd);
         begin
            return Result;
         end;
      end if;
   end Send_Message;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message (Handle  : Access_Handle;
                           Message : String)
   is
      Result : constant String := Send_Message (Handle, Message);
      pragma Unreferenced (Result);
   begin
      null;
   end Send_Message;

   -----------
   -- Valid --
   -----------

   function Valid (A : Access_Handle)
                  return Boolean
   is
   begin
      return A /= null;
   end Valid;

   ----------------------------
   -- Variable_Name_To_Index --
   ----------------------------

   function Variable_Name_To_Index (Name : String) return Positive is
   begin
      return Positive'Value (Name (Name'First + 1 .. Name'Last));
   end Variable_Name_To_Index;

end Kit.Access_Control;
