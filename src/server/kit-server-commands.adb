with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;

package body Kit.Server.Commands is

   type Command_Entry is
      record
         Handler   : Command_Handler;
         Arg_Spec  : access Argument_Type_Array;
      end record;

   package Command_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Command_Entry,
        Hash            => Hash,
        Equivalent_Keys => "=");

   Command_Table : Command_Maps.Map;

   --------------
   -- Argument --
   --------------

   function Argument
     (Arguments : Command_Arguments;
      Index     : Positive)
      return String
   is
   begin
      return To_String (Arguments.Arguments.Element (Index));
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (Arguments : Command_Arguments) return Natural is
   begin
      return Arguments.Arguments.Last_Index;
   end Argument_Count;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Env      : in out Abydos.Environments.Environment;
      Line     : String;
      Response : in out Command_Response)
   is
      Command_Line : constant String :=
                       Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both)
                       & ' ';
      Start        : Positive := Line'First;
      Command      : Unbounded_String;
      Got_Command  : Boolean  := False;
      Skip_Space   : Boolean  := True;
      Arguments    : Command_Arguments;
   begin
      for I in Command_Line'Range loop
         if Command_Line (I) = ' ' then
            if Skip_Space then
               Start := I + 1;
            else
               declare
                  Item : constant String :=
                           Command_Line (Start .. I - 1);
                  U_Item : constant Unbounded_String :=
                             To_Unbounded_String (Item);
               begin
                  Start := I + 1;
                  Skip_Space := True;
                  if Got_Command then
                     Arguments.Arguments.Append (U_Item);
                  else
                     Command := U_Item;
                     Got_Command := True;
                  end if;
               end;
            end if;
         else
            Skip_Space := False;
         end if;
      end loop;

      if not Got_Command then
         Response.Set_Error (Command_Line & ": cannot understand");
      elsif Command_Table.Contains (Command) then
         Command_Table.Element (Command).Handler (Env, Arguments, Response);
      else
         Response.Set_Error (To_String (Command) & ": unknown command");
      end if;

   end Execute_Command;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Response : in out Command_Response) is
   begin
      Response.Lines.Append (Response.Current_Line);
      Response.Current_Line := Null_Unbounded_String;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put
     (Response : in out Command_Response;
      Text     : in     String)
   is
   begin
      Append (Response.Current_Line, Text);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Response : in out Command_Response;
      Text     : in     String)
   is
   begin
      Put (Response, Text);
      New_Line (Response);
   end Put_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Command   : String;
      Arguments : Argument_Type_Array;
      Handler   : Command_Handler)
   is
   begin
      Command_Table.Insert
        (Key      => To_Unbounded_String (Command),
         New_Item => (Handler, new Argument_Type_Array'(Arguments)));
   end Register;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Response : in out Command_Response;
      Message  : in     String)
   is
   begin
      Response.Error := True;
      Response.Put_Line (Message);
   end Set_Error;

   -----------
   -- Write --
   -----------

   procedure Write (Response : Command_Response) is
      use Ada.Text_IO;
   begin
      for Line of Response.Lines loop
         if Response.Error then
            Put (Standard_Error, "error: ");
            Put_Line (Standard_Error, To_String (Line));
         else
            Put_Line (To_String (Line));
         end if;
      end loop;

      if Response.Current_Line /= Null_Unbounded_String then
         if Response.Error then
            Put (Standard_Error, "error: ");
            Put_Line (Standard_Error, To_String (Response.Current_Line));
         else
            Put_Line (To_String (Response.Current_Line));
         end if;
      end if;

   end Write;

end Kit.Server.Commands;
