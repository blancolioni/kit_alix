with Ada.Strings.Fixed;

with Kit.Schema.Tables;

with Kit.Generate.Transformations;

with Kit.Paths;

package body Kit.Generate.Templates is

   ----------------------------
   -- Copy_Template_Packages --
   ----------------------------

   procedure Copy_Template_Packages
     (Database         : not null access
        Kit.Schema.Databases.Database_Type'Class;
      Target_Directory : String)
   is

      procedure General_Packages;

      procedure Extra_Packages
        (Table : Kit.Schema.Tables.Table_Type'Class);

      --------------------
      -- Extra_Packages --
      --------------------

      procedure Extra_Packages
        (Table : Kit.Schema.Tables.Table_Type'Class)
      is
         use Kit.Generate.Transformations;

         Standard_Sub : Substitutions;

         procedure Copy (Base_Name : String);

         ----------
         -- Copy --
         ----------

         procedure Copy (Base_Name : String) is
            Package_Name : constant String :=
                             Database.Ada_Name
                             & "."
                             & Table.Ada_Name
                             & "_"
                             & Base_Name;
            Source_Spec  : constant String := Base_Name & ".ads";
            Source_Body  : constant String := Base_Name & ".adb";
            Target_Spec  : constant String :=
                             Make_File_Name (Package_Name, "ads");
            Target_Body  : constant String :=
                             Make_File_Name (Package_Name, "adb");
         begin
            Copy_File (Kit.Paths.Config_Path & "/" & Source_Spec,
                       Target_Directory & "/" & Target_Spec,
                       Standard_Sub);
            Copy_File (Kit.Paths.Config_Path & "/" & Source_Body,
                       Target_Directory & "/" & Target_Body,
                       Standard_Sub);
         end Copy;

      begin

         Add_Substitution (Standard_Sub, "database", Database.Ada_Name);
         Add_Substitution (Standard_Sub, "table", Table.Ada_Name);
         Add_Substitution
           (Standard_Sub, "table_count",
            Ada.Strings.Fixed.Trim
              (Natural'Image (Database.Table_Count), Ada.Strings.Left));

         if Table.With_Vector_Package then
            Copy ("vectors");
         end if;
         if Table.With_Map_Package then
            Copy ("maps");
         end if;
      end Extra_Packages;

      procedure General_Packages is
         use Kit.Generate.Transformations;

         Standard_Sub : Substitutions;

         procedure Copy (Base_Name : String);

         ----------
         -- Copy --
         ----------

         procedure Copy (Base_Name : String) is
            Package_Name : constant String :=
                             Database.Ada_Name
                             & "."
                             & Base_Name;
            Source_Spec  : constant String := Base_Name & ".ads";
            Source_Body  : constant String := Base_Name & ".adb";
            Target_Spec  : constant String :=
                             Make_File_Name (Package_Name, "ads");
            Target_Body  : constant String :=
                             Make_File_Name (Package_Name, "adb");
         begin
            Copy_File (Kit.Paths.Config_Path & "/" & Source_Spec,
                       Target_Directory & "/" & Target_Spec,
                       Standard_Sub);
            Copy_File (Kit.Paths.Config_Path & "/" & Source_Body,
                       Target_Directory & "/" & Target_Body,
                       Standard_Sub);
         end Copy;

      begin

         Add_Substitution (Standard_Sub, "database", Database.Ada_Name);
         Add_Substitution
           (Standard_Sub, "table_count",
            Ada.Strings.Fixed.Trim
              (Natural'Image (Database.Table_Count), Ada.Strings.Left));

         Copy ("kit_deferred_keys");
      end General_Packages;

   begin
      General_Packages;
      Database.Iterate
        (Extra_Packages'Access);
   end Copy_Template_Packages;

end Kit.Generate.Templates;
