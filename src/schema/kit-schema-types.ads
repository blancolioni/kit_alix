with System.Storage_Elements;

with Syn;

with Kit.Names;

package Kit.Schema.Types is

   type Root_Kit_Type is
     abstract new Kit.Names.Root_Named_Object with private;

   function Return_Subtype (Item : Root_Kit_Type) return String is abstract;
   function Return_Handle_Subtype (Item : Root_Kit_Type'Class) return String;

   function Record_Subtype (Item : Root_Kit_Type) return String;
   function Unconstrained_Record_Subtype (Item : Root_Kit_Type) return String;
   function Argument_Subtype (Item : Root_Kit_Type) return String;
   function Convert_To_String (Item   : Root_Kit_Type;
                               Object_Name : String)
                               return Syn.Expression'Class;
   function Convert_From_String (Item   : Root_Kit_Type;
                                 Object_Name : String)
                                 return Syn.Expression'Class;
   function Has_Default_Value (Item : Root_Kit_Type)
                               return Boolean;
   function Default_Value (Item : Root_Kit_Type)
                           return Syn.Expression'Class
                           is abstract;

   function Haskell_Type_Name (Item : Root_Kit_Type) return String;
   function Internal_Database_Name (Item : Root_Kit_Type) return String
   is (Item.Standard_Name);

   function Is_Text (Item : Root_Kit_Type) return Boolean is (False);

   function Is_Bounded_String (Item : Root_Kit_Type) return Boolean;
   function Is_Fixed_String (Item : Root_Kit_Type) return Boolean;
   function Is_Table_Reference (Item : Root_Kit_Type) return Boolean;

   function Key_OK (Item : Root_Kit_Type) return Boolean
   is (True);

   function Size (Item : Root_Kit_Type) return Natural;

   function To_Storage_Array
     (Item   : Root_Kit_Type;
      Object : Syn.Expression'Class)
      return Syn.Expression'Class;

   function To_Storage_Array
     (Item        : Root_Kit_Type'Class;
      Object_Name : String)
      return Syn.Expression'Class
   is (Item.To_Storage_Array (Syn.Object (Object_Name)));

   function Storage_Array_Transfer
     (Item          : Root_Kit_Type;
      To_Storage    : Boolean;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset)
      return Syn.Statement'Class;

   procedure Set_Value
     (Value_Type  : Root_Kit_Type;
      Target_Name : String;
      Value_Name  : String;
      Sequence    : in out Syn.Statement_Sequencer'Class);

   function Return_Value
     (Value_Type  : Root_Kit_Type;
      Target_Name : String)
      return Syn.Expression'Class;

   function To_Declaration
     (From_Type : Root_Kit_Type)
      return Syn.Declaration'Class;

   function Reference_Database_Type
     (Of_Kit_Type : Root_Kit_Type)
      return Syn.Expression'Class;

   function Create_Database_Record
     (For_Type : Root_Kit_Type)
      return Syn.Statement'Class
      is abstract;

   function First_Value (Of_Type : Root_Kit_Type)
                         return Syn.Expression'Class;

   function Last_Value (Of_Type : Root_Kit_Type)
                        return Syn.Expression'Class;

   type Kit_Type is access all Root_Kit_Type'Class;

   function Standard_Integer        return Kit_Type;
   function Standard_Long_Integer   return Kit_Type;
   function Standard_Positive       return Kit_Type;
   function Standard_Natural        return Kit_Type;
   function Standard_Float          return Kit_Type;
   function Standard_Long_Float     return Kit_Type;
   function Standard_Boolean        return Kit_Type;
   function Standard_Record_Type    return Kit_Type;

   function Standard_String (Length : Positive) return Kit_Type;
   function Standard_Fixed_String (Length : Positive) return Kit_Type;
   function Standard_Text return Kit_Type;

   function Table_Reference_Type
     (Table_Name : String)
      return Kit_Type;

   function Is_Reference_To
     (Item       : Root_Kit_Type;
      Table_Name : String)
      return Boolean;

   function Referenced_Table_Name
     (Item : Root_Kit_Type'Class)
      return String
   with Pre => Item.Is_Table_Reference;

   procedure New_Type (New_Type : Kit_Type);

   procedure New_External_Type
     (Base_Type              : Kit_Type;
      External_Package_Name  : String;
      External_Type_Name     : String;
      To_Database_Function   : String;
      From_Database_Function : String;
      To_String_Function     : String;
      From_String_Function   : String);

   function Is_External_Type
     (Item : Root_Kit_Type'Class)
      return Boolean;

   function External_Type_Package_Name
     (Item : Root_Kit_Type'Class)
      return String
     with Pre => Is_External_Type (Item);

   function Local_Type
     (From_External_Type : Root_Kit_Type'Class)
      return Kit_Type
     with Pre => Is_External_Type (From_External_Type);

   function Is_Type_Name (Name : String) return Boolean;
   function Get_Type (Name : String) return Kit_Type;

   procedure Create_Standard_Types;

   procedure Iterate_User_Defined_Types
     (Process : not null access procedure (User_Type : Kit_Type));

   procedure Iterate_All_Types
     (Process : not null access procedure (User_Type : Kit_Type));

   procedure Update_Record_Type
     (Record_Count : Natural;
      Record_Name  : access
        function (Index : Positive) return String);

private

   type Root_Kit_Type is
     abstract new Kit.Names.Root_Named_Object with
      record
         Size         : Natural := 0;
         User_Defined : Boolean := True;
      end record;

   function Storage_Array_Transfer
     (Item          : Root_Kit_Type'Class;
      Object_Name   : String;
      Storage_Name  : String;
      Start, Finish : System.Storage_Elements.Storage_Offset;
      Proc_Name     : String)
      return Syn.Statement'Class;

   function Is_Bounded_String (Item : Root_Kit_Type) return Boolean
   is (False);

   function Is_Fixed_String (Item : Root_Kit_Type) return Boolean
   is (False);

   function Return_Handle_Subtype (Item : Root_Kit_Type'Class) return String
   is (if Item.Is_Table_Reference
       then Item.Ada_Name & "." & Item.Ada_Name & "_Class"
       else Item.Return_Subtype);

end Kit.Schema.Types;
