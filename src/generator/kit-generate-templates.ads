package Kit.Generate.Templates is

   procedure Copy_Template_Packages
     (Database : not null access Kit.Schema.Databases.Database_Type'Class;
      Target_Directory : String);

end Kit.Generate.Templates;
