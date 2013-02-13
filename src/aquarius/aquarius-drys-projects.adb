package body Aquarius.Drys.Projects is

   -----------------
   -- Add_Package --
   -----------------

   procedure Add_Package (To_Project : in out Project;
                          Item       : in     Declarations.Package_Type'Class)
   is
   begin
      To_Project.Packages.Append (Item);
   end Add_Package;

   -------------------
   -- Write_Project --
   -------------------

   procedure Write_Project (Item   : in Project;
                            File   : in out Writer_Interface'Class)
   is
   begin
      for P of Item.Packages loop
         P.Write (File);
      end loop;
   end Write_Project;

end Aquarius.Drys.Projects;
