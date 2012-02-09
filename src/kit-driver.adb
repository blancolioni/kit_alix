with Aquarius.Drys;
with Aquarius.Drys.File_Writer;
with Aquarius.Drys.Projects;

with Kit.Databases;
with Kit.Test_Database;
with Kit.Generate;

procedure Kit.Driver is
   Db : Kit.Databases.Database_Type :=
     Kit.Test_Database.Create_Test_Database;
   Project : constant Aquarius.Drys.Projects.Project :=
     Kit.Generate.Generate_Database (Db);
begin

   declare
      File : Aquarius.Drys.File_Writer.File_Writer;
   begin
      Aquarius.Drys.Projects.Write_Project (Project, File);
   end;

end Kit.Driver;

--     Table_Type_Definition.New_Literal ("T_No_Table");
--     Table_Type_Definition.New_Literal ("T_Named_Object");
--     Table_Type_Definition.New_Literal ("T_Unit");

--     declare
--        Table_Type : constant Type_Declaration :=
--          New_Full_Type_Declaration ("Table_Type",
--                                     Table_Type_Definition);
--     begin
--        Top_Package.Append (Table_Type);
--     end;

--     declare
--        Table_Type_Definition : Enumeration_Type_Definition;
--        It : Kit.Databases.Table_Cursor :=
--          Db.First;
--     begin
--        while Kit.Databases.Has_Element (It) loop

--     declare
--        Reference_Type : Type_Declaration :=
--          New_Private_Type_Declaration
--          (Table_Name & "_Reference",
--           New_Derived_Type ("Marlowe.Database_Index"));
--        Null_Reference : Object_Declaration :=
--          New_Deferred_Constant_Declaration
--          ("null_" & Table_Name & "_Reference",
--           Table_Name & "_Reference",
--           Literal (0));
--     begin
--        Harriet.Append (Reference_Type);
--        Harriet.Append (Null_Reference);
--     end;

--     declare
--        File : Aquarius.Drys.File_Writer.File_Writer;
--     begin
--        File.Create ("/home/fraser/harriet.ads");
--        Harriet.Write (File);
--        File.Close;
--     end;

--  end Kit.Driver;
