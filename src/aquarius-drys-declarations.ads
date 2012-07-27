private with Ada.Containers.Vectors;

with Aquarius.Drys.Blocks;

package Aquarius.Drys.Declarations is

   function Use_Type (Type_Name : String)
                      return  Declaration'Class;

   function New_Pragma (Pragma_Name : String;
                        Argument    : String)
                        return Declaration'Class;

   type Specification_Separator is
     new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Specification_Separator)
     return Boolean;

   overriding
   function Has_Body
     (Item : Specification_Separator)
     return Boolean;

   overriding
   procedure Write (Item        : Specification_Separator;
                    Writer      : in out Writer_Interface'Class);

   overriding
   function Pseudo_Declaration
     (Item : Specification_Separator)
     return Boolean;

   function New_Separator return Specification_Separator;

   function Renaming_Declaration
     (New_Identifier : String;
      New_Type       : String;
      Renamed_Expression : Expression'Class)
      return Declaration'Class;

   type Object_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Object_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Object_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Object_Declaration;
                    Writer      : in out Writer_Interface'Class);

   type Defining_Identifier_List is private;

   function Identifier (Id : String) return Defining_Identifier_List;

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class)
     return Object_Declaration'Class;
   function New_Object_Declaration
     (Name        : String;
      Object_Type : String)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : String;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Constant_Declaration
     (Name        : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   type Type_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Type_Declaration)
     return Boolean;

   overriding
   function Has_Body_Spec
     (Item : Type_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Type_Declaration;
                    Writer      : in out Writer_Interface'Class);

   function New_Full_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class)
     return Type_Declaration;

   function New_Private_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class)
     return Type_Declaration;

   function New_Deferred_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class)
      return Type_Declaration;

   type Subtype_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Subtype_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Subtype_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Subtype_Declaration;
                    Writer      : in out Writer_Interface'Class);

   function New_Subtype_Declaration
     (Identifier  : String;
      Definition  : Subtype_Indication'Class)
     return Subtype_Declaration;

   type Formal_Argument is new Syntax_Root with private;

   function New_Formal_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   function New_Formal_Argument
     (Name             : String;
      Argument_Type    : Subtype_Indication'Class;
      Argument_Default : Expression'Class)
      return Formal_Argument'Class;

   function New_In_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class
     renames New_Formal_Argument;

   function New_Out_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   function New_Inout_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   overriding
   procedure Write (Item        : Formal_Argument;
                    Writer      : in out Writer_Interface'Class);

   type Subprogram_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Subprogram_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Subprogram_Declaration)
      return Boolean;

   overriding
   function Has_Body_Spec
     (Item : Subprogram_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Subprogram_Declaration;
                    Writer      : in out Writer_Interface'Class);

   procedure Add_Local_Declaration
     (Subprogram : in out Subprogram_Declaration;
      Dec        : in     Declaration'Class);

   type Argument_Mode is (In_Argument, Out_Argument, Inout_Argument,
                          Access_Argument);

   function New_Abstract_Function
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration;

   function New_Abstract_Function
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration;

   function New_Abstract_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class)
     return Subprogram_Declaration;

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class)
     return Subprogram_Declaration;

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Argument_3  : Formal_Argument'Class)
      return Subprogram_Declaration;

   function New_Abstract_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration;

   function New_Abstract_Procedure
     (Name        : String)
     return Subprogram_Declaration;

   function New_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration;

   function New_Function
     (Name        : String;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration;

   function New_Function
     (Name        : String;
      Result_Type : String;
      Result      : Expression'Class)
     return Subprogram_Declaration;

   function New_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration;

   function New_Procedure
     (Name        : String;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration;

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Argument   : in     Formal_Argument'Class);

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Type   : in     String);

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Mode   : in     Argument_Mode;
      Arg_Type   : in     String);

   procedure Add_Formal_Argument
     (Subprogram  : in out Subprogram_Declaration'Class;
      Arg_Name    : in     String;
      Arg_Type    : in     String;
      Arg_Default : in     Expression'Class);

   procedure Set_Overriding
     (Item : in out Subprogram_Declaration);

   type Package_Type is new Declaration with private;

   overriding
   procedure Write (Item        : Package_Type;
                    Writer      : in out Writer_Interface'Class);

   procedure Add_Separator (Item : in out Package_Type);

   function New_Child_Package
     (Parent : Package_Type;
      Name   : String)
     return Package_Type;

   function New_Package_Type
     (Name   : String)
     return Package_Type;

   procedure With_Package
     (Item         : in out Package_Type;
      Withed       : in     String;
      Private_With : Boolean := False;
      Body_With    : Boolean := False);

   procedure Set_Generic_Instantion
     (Item                 : in out Package_Type;
      Instantiated_Package : in     String);

   procedure Set_Private
     (Item : in out Package_Type);

   procedure Add_Generic_Actual_Argument
     (Item  : in out Package_Type;
      Value : in     String);

   procedure Add_Generic_Actual_Argument
     (Item  : in out Package_Type;
      Value : in     Integer);

   procedure Append
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class);

   procedure Append_To_Body
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class);

   overriding
   function Has_Private_Part
     (Item : Package_Type)
     return Boolean;

   overriding
   function Has_Body
     (Item : Package_Type)
     return Boolean;

private

   type Specification_Separator is
     new Declaration with null record;

   type Defining_Identifier_List is
      record
         List : String_Vector.Vector;
      end record;

   type Object_Declaration is new Declaration with
      record
         Objects     : Defining_Identifier_List;
         Object_Type : access Subtype_Indication'Class;
         Initialiser : access Expression'Class;
         Is_Constant : Boolean;
         Is_Aliased  : Boolean;
         Is_Deferred : Boolean;
         Is_Argument : Boolean       := False;
         Mode        : Argument_Mode := In_Argument;
      end record;

   function Defining_Identifiers_Length
     (Item : Object_Declaration'Class)
     return Natural;

   type Type_Declaration is new Declaration with
      record
         Name        : String_Access;
         Definition  : access Type_Definition'Class;
         Is_Private  : Boolean := False;
         Is_Deferred : Boolean := False;
      end record;

   type Subtype_Declaration is new Declaration with
      record
         Name        : String_Access;
         Definition  : access Subtype_Indication'Class;
      end record;

   type Formal_Argument is new Object_Declaration with null record;

   package Formal_Argument_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, Formal_Argument'Class);

   type Subprogram_Declaration is new Declaration with
      record
         Name           : String_Access;
         Arguments      : Formal_Argument_Vectors.Vector;
         Result_Type    : access Subtype_Indication'Class;
         Sub_Body       : access Blocks.Block_Type'Class;
         Is_Function    : Boolean := False;
         Is_Abstract    : Boolean := False;
         Is_Private     : Boolean := False;
         Is_Overriding  : Boolean := False;
         Arg_Name_Width : Natural := 0;
      end record;

   type With_Context_Clause is
      record
         Withed_Package : String_Access;
         Is_Private     : Boolean        := False;
         Is_Body        : Boolean        := False;
         Elaborate_All  : Boolean        := False;
      end record;

   package Context_Clause_Vectors is
      new Ada.Containers.Vectors (Positive, With_Context_Clause);

   type Package_Type is new Declaration with
      record
         Name                 : String_Access;
         Instantiated_Package : String_Access;
         Withed_Packages      : Context_Clause_Vectors.Vector;
         Declarations         : Declaration_Vector.Vector;
         Formal_Arguments     : Declaration_Vector.Vector;
         Actual_Arguments     : String_Vector.Vector;
         Is_Generic           : Boolean                    := False;
         Is_Instantiation     : Boolean                    := False;
         Is_Private           : Boolean                    := False;
         Has_Private          : Boolean                    := False;
         Has_Body             : Boolean                    := False;
      end record;

end Aquarius.Drys.Declarations;
