with System.Storage_Elements;

with Kit.Db.Kit_Type;

package Kit.Server.Storage is

   function Storage_To_String
     (Value : System.Storage_Elements.Storage_Array;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type)
      return String;

   procedure String_To_Storage
     (Value      : String;
      Value_Type : Kit.Db.Kit_Type.Kit_Type_Type;
      Storage    : out System.Storage_Elements.Storage_Array);

end Kit.Server.Storage;
