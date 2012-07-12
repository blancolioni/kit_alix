with Kit.Names;

package Kit.Compound_Keys is

   type Compound_Key_Type is
     new Kit.Names.Root_Named_Object with private;

   procedure Create_Compound_Key (Item       : in out Compound_Field_Type;
                                  Name       : in     String;
                                  Unique     : in     Boolean);

   procedure Add_Field (To    : in out Compound_Field_Type;
                        Index : in     Positive);

private

   Max_Key_Fields : constant := 12;

   type Compound_Key_Fields is array (1 .. Max_Key_Fields) of Positive;

   type Compound_Key_Type is
     new Kit.Names.Root_Named_Object with
      record
         Compound_Field_Type : access Kit.Schema.Types.Kit_Type'Class;
         Unique      : Boolean;
         Field_Count : Natural;
         Fields      : Compound_Key_Fields;
      end record;

end Kit.Compound_Keys;

