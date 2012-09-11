with System.Storage_Elements;

with Aquarius.Drys;

with Kit.Names;

package Kit.Schema.Types is

   type Kit_Type is
     abstract new Kit.Names.Root_Named_Object with private;

   function Return_Subtype (Item : Kit_Type) return String is abstract;
   function Record_Subtype (Item : Kit_Type) return String;
   function Unconstrained_Record_Subtype (Item : Kit_Type) return String;
   function Argument_Subtype (Item : Kit_Type) return String;
   function Convert_To_String (Item   : Kit_Type;
                               Object_Name : String)
                               return Aquarius.Drys.Expression'Class;
   function Convert_From_String (Item   : Kit_Type;
                                 Object_Name : String)
                                 return Aquarius.Drys.Expression'Class;
   function Has_Default_Value (Item : Kit_Type)
                               return Boolean;
   function Default_Value (Item : Kit_Type)
                           return Aquarius.Drys.Expression'Class
                           is abstract;

   function Is_String (Item : Kit_Type) return Boolean;
   function Is_Table_Reference (Item : Kit_Type) return Boolean;

   function Size (Item : Kit_Type) return Natural;

   function To_Storage_Array
     (Item        : Kit_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class;

   function Storage_Array_Transfer
     (Item          : Kit_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Aquarius.Drys.Statement'Class;

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

   function Reference_Database_Type
     (Of_Kit_Type : Kit_Type)
      return Aquarius.Drys.Expression'Class;

   function Create_Database_Record
     (For_Type : Kit_Type)
      return Aquarius.Drys.Statement'Class
      is abstract;

   function First_Value (Of_Type : Kit_Type)
                         return Aquarius.Drys.Expression'Class;

   function Last_Value (Of_Type : Kit_Type)
                        return Aquarius.Drys.Expression'Class;

   function Standard_Integer     return Kit_Type'Class;
   function Standard_Positive    return Kit_Type'Class;
   function Standard_Natural     return Kit_Type'Class;
   function Standard_Float       return Kit_Type'Class;
   function Standard_Long_Float  return Kit_Type'Class;
   function Standard_Boolean     return Kit_Type'Class;
   function Standard_Record_Type return Kit_Type'Class;

   function Standard_String (Length : Positive) return Kit_Type'Class;

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type'Class;

   function Is_Reference_To
     (Item       : Kit_Type;
      Table_Name : String)
      return Boolean;

   function Referenced_Table_Name
     (Item : Kit_Type'Class)
      return String
   with Pre => Item.Is_Table_Reference;

   procedure New_Type (New_Type : Kit.Schema.Types.Kit_Type'Class);

   function Is_Type_Name (Name : String) return Boolean;
   function Get_Type (Name : String) return Kit_Type'Class;

   procedure Create_Standard_Types;

   procedure Iterate_User_Defined_Types
     (Process : not null access procedure (User_Type : Kit_Type'Class));

   procedure Iterate_All_Types
     (Process : not null access procedure (User_Type : Kit_Type'Class));

   procedure Update_Record_Type
     (Record_Count : Natural;
      Record_Name  : access
        function (Index : Positive) return String);

private

   type Kit_Type is
     abstract new Kit.Names.Root_Named_Object with
      record
         Size         : Natural := 0;
         User_Defined : Boolean := True;
      end record;

   function Storage_Array_Transfer
     (Item          : Kit_Type'Class;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset;
      Proc_Name     : String)
      return Aquarius.Drys.Statement'Class;

end Kit.Schema.Types;
