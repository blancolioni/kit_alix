with Kit.Server.Export;

package Kit.Server.Kit_XML is

   function XML_Exporter
     (Target_Directory : String)
     return Kit.Server.Export.Root_Exporter'Class;

end Kit.Server.Kit_XML;
