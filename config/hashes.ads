with Ada.Containers;

package {database}.{table}_Hashes is

   function Hash
     (Reference : {table}_Reference)
      return Ada.Containers.Hash_Type;

private
   
   function Hash
     (Reference : {table}_Reference)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Reference));
   
end {database}.{table}_Hashes;
