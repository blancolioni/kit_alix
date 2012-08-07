with Kit.Schema.Keys;
with Kit.Schema.Tables;

package body Kit.Generate.Marlowe_Keys_Package is

   ----------------------
   -- Generate_Package --
   ----------------------

   function Generate_Package
     (Db : in out Kit.Schema.Databases.Database_Type)
      return Aquarius.Drys.Declarations.Package_Type
   is
      Result : Aquarius.Drys.Declarations.Package_Type :=
                 Aquarius.Drys.Declarations. New_Package_Type
                   (Db.Ada_Name & ".Marlowe_Keys");

      procedure Generate_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type'Class);

      -------------------------
      -- Generate_Table_Keys --
      -------------------------

      procedure Generate_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type'Class)
      is
         procedure Generate_Key
           (Base : Kit.Schema.Tables.Table_Type'Class;
            Key  : Kit.Schema.Keys.Key_Type'Class);

         ------------------
         -- Generate_Key --
         ------------------

         procedure Generate_Key
           (Base : Kit.Schema.Tables.Table_Type'Class;
            Key  : Kit.Schema.Keys.Key_Type'Class)
         is
            pragma Unreferenced (Base);
            use Kit.Schema.Tables;
            Dec : constant Aquarius.Drys.Declaration'Class :=
                    Aquarius.Drys.Declarations.New_Object_Declaration
                      (Table.Key_Reference_Name (Key),
                       Aquarius.Drys.Named_Subtype
                         ("Marlowe.Btree_Handles.Btree_Reference"));
         begin
            Result.Append (Dec);
         end Generate_Key;

      begin
         Table.Scan_Keys (Generate_Key'Access);
      end Generate_Table_Keys;

   begin
      Result.With_Package ("Marlowe.Btree_Handles");
      Result.Append
        (Aquarius.Drys.Declarations.New_Object_Declaration
           ("Handle",
            Aquarius.Drys.Named_Subtype
              ("Marlowe.Btree_Handles.Btree_Handle")));

      Db.Iterate (Generate_Table_Keys'Access);

      return Result;
   end Generate_Package;

end Kit.Generate.Marlowe_Keys_Package;
