with Aquarius.Drys;

with Kit.Tables;

package Kit.Generate.Fetch is

   procedure Fetch_From_Index
     (Table       : Kit.Tables.Table_Type'Class;
      Object_Name : String;
      Target      : in out Aquarius.Drys.Statement_Sequencer'Class);

end Kit.Generate.Fetch;
