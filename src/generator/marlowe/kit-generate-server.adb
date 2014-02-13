with Kit.Paths;

with Kit.Generate.Transformations;

package body Kit.Generate.Server is

   --------------------------
   -- Copy_Server_Packages --
   --------------------------

   procedure Copy_Server_Packages
     (Database_Name    : String;
      Target_Directory : String)
   is
      use Kit.Generate.Transformations;

      Standard_Sub : Substitutions;

      procedure Copy (Package_Name : String);

      ----------
      -- Copy --
      ----------

      procedure Copy (Package_Name : String) is
         Source_Spec : constant String :=
                         Make_File_Name (Package_Name, "ads");
         Source_Body : constant String :=
                         Make_File_Name (Package_Name, "adb");
         Target_Spec : constant String :=
                         Make_File_Name (Database_Name & "." & Package_Name,
                                         "ads");
         Target_Body : constant String :=
                         Make_File_Name (Database_Name & "." & Package_Name,
                                         "adb");
      begin
         Copy_File (Kit.Paths.Config_Path & "/" & Source_Spec,
                    Target_Directory & "/" & Target_Spec,
                    Standard_Sub);
         Copy_File (Kit.Paths.Config_Path & "/" & Source_Body,
                    Target_Directory & "/" & Target_Body,
                    Standard_Sub);
      end Copy;

   begin

      Add_Substitution (Standard_Sub, "database", Database_Name);

      Copy ("Tables");
      Copy ("Tables.Scanner");
      Copy ("Tables.SK_Tables");
      Copy ("SK_Bindings");

   end Copy_Server_Packages;

end Kit.Generate.Server;
