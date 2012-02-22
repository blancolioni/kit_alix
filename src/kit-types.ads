with Aquarius.Drys;

with Kit.Names;

package Kit.Types is

   type Kit_Type is
     abstract new Kit.Names.Root_Named_Object with private;

   function Return_Subtype (Item : Kit_Type) return String is abstract;
   function Record_Subtype (Item : Kit_Type) return String;
   function Unconstrained_Record_Subtype (Item : Kit_Type) return String;
   function Argument_Subtype (Item : Kit_Type) return String;

   function Is_String (Item : Kit_Type) return Boolean;

   function Size (Item : Kit_Type) return Natural;

   function To_Storage_Array
     (Item        : Kit_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

   procedure Set_Value
     (Value_Type  : Kit_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Aquarius.Drys.Statement_Sequencer'Class);

   function Return_Value
     (Value_Type  : Kit_Type;
      Target_Name : String)
      return Aquarius.Drys.Expression'Class;

   function To_Declaration
     (From_Type : Kit_Type)
      return Aquarius.Drys.Declaration'Class;

   function Standard_Integer    return Kit_Type'Class;
   function Standard_Positive   return Kit_Type'Class;
   function Standard_Natural    return Kit_Type'Class;
   function Standard_Float      return Kit_Type'Class;
   function Standard_Long_Float return Kit_Type'Class;
   function Standard_Boolean    return Kit_Type'Class;

   function Standard_String (Length : Positive) return Kit_Type'Class;

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type'Class;

   procedure New_Type (New_Type : Kit.Types.Kit_Type'Class);

   function Is_Type_Name (Name : String) return Boolean;
   function Get_Type (Name : String) return Kit_Type'Class;

   procedure Create_Standard_Types;

   procedure Iterate_User_Defined_Types
     (Process : not null access procedure (User_Type : Kit_Type'Class));

private

   type Kit_Type is
     abstract new Kit.Names.Root_Named_Object with
      record
         Size         : Natural;
         User_Defined : Boolean := True;
      end record;

end Kit.Types;
