private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Drys is

   type Subprogram_Formal_Argument is private;

   function Argument (Name          : String;
                      Type_Name     : String;
                      Default_Value : String := "")
                      return Subprogram_Formal_Argument;

   function In_Argument (Name          : String;
                         Type_Name     : String;
                         Default_Value : String := "")
                         return Subprogram_Formal_Argument
                         renames Argument;

   function Inout_Argument (Name          : String;
                            Type_Name     : String)
                            return Subprogram_Formal_Argument;

   function Out_Argument (Name          : String;
                          Type_Name     : String)
                          return Subprogram_Formal_Argument;

   function Access_Argument (Name          : String;
                             Type_Name     : String;
                             Not_Null      : Boolean := True;
                             Is_Constant   : Boolean := False)
                             return Subprogram_Formal_Argument;

   type Formal_Argument_List is
     array (Positive range <>) of Subprogram_Formal_Argument;

   type Statement is private;

   type Statement_List is
     array (Positive range <>) of Statement;

   function Null_Statement return Statement;

   function Return_Statement (Expression : String)
                              return Statement;

   function Return_Statement
     (Variable      : String;
      Variable_Type : String;
      Operations    : Statement)
      return Statement;

   function Procedure_Call_Statement
     (Name        : String;
      Argument_1  : String   := "";
      Argument_2  : String   := "";
      Argument_3  : String   := "";
      Argument_4  : String   := "";
      Argument_5  : String   := "")
      return Statement;

   function Sequence_Of_Statements
     (Statements : Statement_List)
     return Statement;

   function Assignment_Statement
     (Dst, Src : String)
     return Statement;

   type Subprogram_Body is private;

   function Statement_Body
     (Statements : Statement_List)
      return Subprogram_Body;

   function Statement_Body
     (Single : Statement)
      return Subprogram_Body;

   type Package_Declaration is tagged private;

   function New_Package (Name : String) return Package_Declaration;

   function New_Child_Package
     (Parent : Package_Declaration;
      Name   : String)
      return Package_Declaration;

   procedure Create_Interface
     (Item    : in out Package_Declaration;
      Name    : in     String);

   procedure Create_Class_Wide_Subtype
     (Item     : in out Package_Declaration;
      For_Type : in String;
      Name     : in String);

   procedure Abstract_Function
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Arg      : in     Subprogram_Formal_Argument;
      Result   : in     String);

   procedure Abstract_Procedure
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Arg      : in     Subprogram_Formal_Argument);

   procedure Abstract_Function
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Args     : in     Formal_Argument_List;
      Result   : in     String);

   procedure Abstract_Procedure
     (Item     : in out Package_Declaration;
      Name     : in     String;
      Args     : in     Formal_Argument_List);

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Result    : in     String;
      Item_Body : in     Subprogram_Body);

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Item_Body : in     Subprogram_Body);

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Arg       : in     Subprogram_Formal_Argument;
      Result    : in     String;
      Item_Body : in     Subprogram_Body);

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Arg       : in     Subprogram_Formal_Argument;
      Item_Body : in     Subprogram_Body);

   procedure Create_Function
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Args      : in     Formal_Argument_List;
      Result    : in     String;
      Item_Body : in     Subprogram_Body);

   procedure Create_Procedure
     (Item      : in out Package_Declaration;
      Name      : in     String;
      Args      : in     Formal_Argument_List;
      Item_Body : in     Subprogram_Body);

   procedure Write (Item : Package_Declaration;
                    Directory : String);

private

   subtype Drys_Name is
     Ada.Strings.Unbounded.Unbounded_String;

   type Argument_Mode is
     (In_Argument, Out_Argument, Inout_Argument,
      Access_Argument);

   type Subprogram_Formal_Argument is
      record
         Name         : Drys_Name;
         Mode         : Argument_Mode;
         Not_Null     : Boolean;
         Acc_Constant : Boolean;
         Arg_Type     : Drys_Name;
         Arg_Default  : Drys_Name;
      end record;

   type Declaration_Type is
     (Empty_Declaration,
      Interface_Declaration,
      Type_Declaration,
      Subtype_Declaration,
      Subprogram_Declaration);

   package String_Vectors is
     new Ada.Containers.Vectors (Positive,
                                 Ada.Strings.Unbounded.Unbounded_String,
                                 Ada.Strings.Unbounded."=");

   type Formal_Argument_Access is
     access Formal_Argument_List;

   type Declaration (Decl_Type : Declaration_Type := Empty_Declaration) is
      record
         Name     : Drys_Name;
         case Decl_Type is
            when Empty_Declaration =>
               null;
            when Interface_Declaration =>
               Interface_Parents : String_Vectors.Vector;
            when Type_Declaration =>
               null;
            when Subtype_Declaration =>
               Base_Type         : Drys_Name;
               Class_Wide        : Boolean;
            when Subprogram_Declaration =>
               Arguments         : Formal_Argument_Access;
               Return_Type       : Drys_Name;
               Is_Abstract       : Boolean;
               Sub_Body          : Subprogram_Body;
         end case;

      end record;

   type Source_Line is
      record
         Indent    : Integer := 0;
         Line      : Drys_Name;
      end record;

   package Line_Vectors is
     new Ada.Containers.Vectors (Positive, Source_Line);

   type Statement_Type is (Simple, Compound);

   type Statement (S_Type : Statement_Type := Simple) is
      record
         case S_Type is
            when Simple =>
               Lines : String_Vectors.Vector;
            when Compound =>
               Prefix : Drys_Name;
               S_Body : access Statement;
               Suffix : Drys_Name;
         end case;
      end record;

   type Subprogram_Body is
      record
         Statements : Statement;
      end record;

   package Declaration_Vectors is
     new Ada.Containers.Vectors (Positive, Declaration);

   type Package_Declaration is tagged
      record
         Name         : Drys_Name;
         Declarations : Declaration_Vectors.Vector;
      end record;

end Drys;
