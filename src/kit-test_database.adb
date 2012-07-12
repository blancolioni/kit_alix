with Kit.Schema.Fields;
with Kit.Schema.Tables;
with Kit.Schema.Types;

package body Kit.Test_Database is

   --------------------------
   -- Create_Test_Database --
   --------------------------

   function Create_Test_Database return Kit.Schema.Databases.Database_Type is
      Result : Kit.Schema.Databases.Database_Type;
      Object, Named_Object, Pop_Type, Ship, Unit : Kit.Schema.Tables.Table_Type;
   begin
      Result.Create_Database ("test_database");

      Object.Create ("object");

      declare
         Field : Kit.Schema.Fields.Field_Type;
      begin
         Field.Create_Field ("identity",
                             Kit.Schema.Types.Standard_Positive);
         Object.Append (Field, True, True);
      end;

      Result.Append (Object);

      Named_Object.Create ("named_object");
      Named_Object.Add_Base (Object);

      declare
         Field : Kit.Schema.Fields.Field_Type;
      begin
         Field.Create_Field ("name",
                                Kit.Schema.Types.Standard_String (32));
         Named_Object.Append (Field, True, True);
      end;

      Result.Append (Named_Object);

      Pop_Type.Create ("pop_type");
      Pop_Type.Add_Base (Named_Object);

      declare
         Field : Kit.Schema.Fields.Field_Type;
      begin
         Field.Create_Field ("size", Kit.Schema.Types.Standard_Natural);
         Pop_Type.Append (Field, False);
      end;

      Result.Append (Pop_Type);

      Ship.Create ("ship");
      Ship.Add_Base (Named_Object);

      declare
         Field : Kit.Schema.Fields.Field_Type;
      begin
         Field.Create_Field ("owner", Kit.Schema.Types.Standard_Natural);
         Ship.Append (Field, True);
      end;

      Result.Append (Ship);

      Unit.Create ("unit");
      Unit.Add_Base (Named_Object);

      declare
         Field : Kit.Schema.Fields.Field_Type;
      begin
         Field.Create_Field ("size", Kit.Schema.Types.Standard_Natural);
         Unit.Append (Field, False);
      end;

      Result.Append (Unit);

      return Result;

   end Create_Test_Database;

end Kit.Test_Database;
