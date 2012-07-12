with Kit.Databases;

package Kit.Server.Export is

   type Root_Exporter is abstract tagged limited private;

   procedure Start_Export (Exporter : in out Root_Exporter;
                           Name     : String)
   is abstract;

   procedure End_Export (Exporter : in out Root_Exporter)
   is abstract;

   procedure Start_Table (Exporter : in out Root_Exporter;
                          Name     : String)
   is abstract;

   procedure End_Table (Exporter : in out Root_Exporter)
   is abstract;

   procedure Base_Table (Exporter  : in out Root_Exporter;
                         Base_Name : in     String)
   is abstract;

   procedure Field (Exporter   : in out Root_Exporter;
                    Name       : in String;
                    Field_Type : in String)
   is abstract;

   procedure Field (Exporter   : in out Root_Exporter;
                    Name       : in String;
                    Base       : in String;
                    Field_Type : in String)
   is abstract;

   procedure Start_Key (Exporter    : in out Root_Exporter;
                        Name        : in String;
                        Unique      : in Boolean)
   is abstract;

   procedure End_Key (Exporter : in out Root_Exporter)
   is abstract;

   procedure Key_Field (Exporter : in out Root_Exporter;
                        Field_Name : in String)
   is abstract;

   procedure Start_Record (Exporter    : in out Root_Exporter)
   is abstract;

   procedure Record_Field (Exporter    : in out Root_Exporter;
                           Field_Name  : in     String;
                           Field_Value : in     String)
   is abstract;

   procedure End_Record (Exporter    : in out Root_Exporter)
   is abstract;

   procedure Export
     (Exporter : in out Root_Exporter'Class;
      Db       : Kit.Databases.Root_Database_Interface'Class);

private

   type Root_Exporter is abstract tagged limited null record;

end Kit.Server.Export;
