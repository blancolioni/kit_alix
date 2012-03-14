private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Abydos.Environments;

package Kit.Server.Commands is

   type Command_Response is tagged limited private;

   procedure Set_Error (Response : in out Command_Response;
                        Message  : in     String);

   procedure Put (Response : in out Command_Response;
                  Text     : in     String);
   procedure Put_Line (Response : in out Command_Response;
                       Text     : in     String);
   procedure New_Line (Response : in out Command_Response);

   procedure Write (Response : Command_Response);

   procedure Execute_Command
     (Env      : in out Abydos.Environments.Environment;
      Line     : String;
      Response : in out Command_Response);

   type Command_Arguments is tagged limited private;

   function Argument_Count (Arguments : Command_Arguments) return Natural;
   function Argument (Arguments : Command_Arguments;
                      Index     : Positive)
                      return String;

   type Command_Handler is access
     procedure (Env       : in out Abydos.Environments.Environment;
                Arguments : in     Command_Arguments;
                Response  : in out Command_Response);

   type Argument_Type is (Any, Table_Name);

   type Argument_Type_Array is array (Positive range <>) of Argument_Type;

   procedure Register (Command   : String;
                       Arguments : Argument_Type_Array;
                       Handler   : Command_Handler);

private

   use Ada.Strings.Unbounded;

   package String_Vectors is
     new Ada.Containers.Vectors (Positive, Unbounded_String);

   type Command_Arguments is tagged limited
      record
         Arguments : String_Vectors.Vector;
      end record;

   type Command_Response is tagged limited
      record
         Error        : Boolean;
         Current_Line : Unbounded_String;
         Lines        : String_Vectors.Vector;
      end record;

end Kit.Server.Commands;
