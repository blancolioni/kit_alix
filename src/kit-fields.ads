private with Ada.Containers.Vectors;

with Kit.Names;
with Kit.Types;

package Kit.Fields is

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with private;

   function Size (Item : Root_Field_Type) return Natural;

   type Field_Type is new Root_Field_Type with private;

   procedure Create_Field (Item       : in out Field_Type;
                           Name       : in     String;
                           Field_Type : in     Kit.Types.Kit_Type'Class);

   function Get_Field_Type (Item : Field_Type)
                           return Kit.Types.Kit_Type'Class;

   type Compound_Field_Type is new Root_Field_Type with private;

   procedure Create_Field (Item       : in out Compound_Field_Type;
                           Name       : in     String);

   procedure Add_Field (Item  : in out Compound_Field_Type;
                        Field : not null access Field_Type'Class);

   function Contains (Item  : Compound_Field_Type;
                      Field : Field_Type'Class)
                      return Boolean;

private

   type Root_Field_Type is
     new Kit.Names.Root_Named_Object with
      record
         Size    : Natural;
      end record;

   type Field_Type is new Root_Field_Type with
      record
         Field_Type : access Kit.Types.Kit_Type'Class;
      end record;

   type Field_Access is access all Root_Field_Type'Class;

   function Equal_Fields (Left, Right : Field_Access) return Boolean;

   package Field_Vectors is
     new Ada.Containers.Vectors (Positive, Field_Access, Equal_Fields);

   type Compound_Field_Type is new Root_Field_Type with
      record
         Fields : Field_Vectors.Vector;
      end record;

end Kit.Fields;
