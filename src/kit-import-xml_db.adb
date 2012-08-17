with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Kit.Schema.Fields;
with Kit.Schema.Tables;
with Kit.Schema.Types;

package body Kit.Import.XML_DB is

   type Reader_State is (Start,
                         Top_Level,
                         Table_Spec,
                         Column,
                         Table_Body,
                         Row,
                         Error);

   type XML_DB_Reader_Type is
     new XML.XML_Document with
      record
         Db : Kit.Schema.Databases.Database_Access;
         State : Reader_State := Start;
         Table      : Kit.Schema.Tables.Table_Type;
         Field_Name : Ada.Strings.Unbounded.Unbounded_String;
         Field_Type : Ada.Strings.Unbounded.Unbounded_String;
         Reference  : Ada.Strings.Unbounded.Unbounded_String;
         Default    : Ada.Strings.Unbounded.Unbounded_String;
         Key        : Boolean := False;
         Unique     : Boolean := False;
         Primary    : Boolean := False;
         Auto       : Boolean := False;
         Not_Null   : Boolean := False;
      end record;

   procedure On_Open_Tag
     (Document : in out XML_DB_Reader_Type;
      Tag_Name : String);

   procedure On_Close_Tag
     (Document : in out XML_DB_Reader_Type;
      Tag_Name : String);

   procedure On_Attribute
     (Document : in out XML_DB_Reader_Type;
      Attribute_Name  : String;
      Attribute_Value : String);

   function Get_Type
     (Db        : Kit.Schema.Databases.Database_Access;
      Type_Name : String;
      Reference : String)
      return Kit.Schema.Types.Kit_Type'Class;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Db        : Kit.Schema.Databases.Database_Access;
      Type_Name : String;
      Reference : String)
      return Kit.Schema.Types.Kit_Type'Class
   is
      pragma Unreferenced (Db);
   begin
      if Reference /= "" then
         declare
            Index : constant Natural :=
                      Ada.Strings.Fixed.Index (Reference,
                                               "(Type)");
            Name  : constant String :=
                      (if Index = 0
                       then Reference
                       else Reference (Reference'First .. Index - 1));
         begin
--              if not Db.Contains (Name) then
--                 declare
--                    Table : Kit.Schema.Tables.Table_Type;
--                 begin
--                    Table.Create (Name);
--                    Db.Append (Table);
--                 end;
--              end if;
            return Kit.Schema.Types.Table_Reference_Type (Name);
         end;
      end if;

      if Type_Name = "integer" or else Type_Name = "int" then
         return Kit.Schema.Types.Standard_Integer;
      elsif Type_Name = "text" or else Type_Name = "string" then
         return Kit.Schema.Types.Standard_String (32);
      elsif Type_Name = "bool" or else Type_Name = "boolean" then
         return Kit.Schema.Types.Standard_Boolean;
      elsif Type_Name = "variant" then
         return Kit.Schema.Types.Standard_String (16);
      elsif Type_Name = "float" then
         return Kit.Schema.Types.Standard_Float;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown type: " & Type_Name);
         raise Constraint_Error;
      end if;
   end Get_Type;

   ------------------
   -- On_Attribute --
   ------------------

   procedure On_Attribute
     (Document : in out XML_DB_Reader_Type;
      Attribute_Name  : String;
      Attribute_Value : String)
   is
      use Ada.Strings.Unbounded;
   begin
      case Document.State is
         when Start =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Attribute_Name & ": ignored");
         when Top_Level =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Document.Db.Name & "."
               & Attribute_Name
               & ": ignored");

         when Table_Spec =>
            if Attribute_Name = "name" then
               Document.Table.Create (Attribute_Value);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Table." &
                    Attribute_Name & ": ignored");
            end if;

         when Column =>

            if Attribute_Name = "name" then
               Document.Field_Name :=
                 To_Unbounded_String (Attribute_Value);
            elsif Attribute_Name = "type" then
               Document.Field_Type :=
                 To_Unbounded_String (Attribute_Value);
            elsif Attribute_Name = "primarykey" then
               Document.Key := Boolean'Value (Attribute_Value);
               Document.Unique := Document.Key;
               Document.Primary := Document.Key;
            elsif Attribute_Name = "unique" then
               Document.Key := Boolean'Value (Attribute_Value);
               Document.Unique := Document.Key;
            elsif Attribute_Name = "autoincrement" then
               Document.Auto := Boolean'Value (Attribute_Value);
            elsif Attribute_Name = "notnull" then
               Document.Not_Null := Boolean'Value (Attribute_Value);
            elsif Attribute_Name = "reference" then
               Document.Reference :=
                 To_Unbounded_String (Attribute_Value);
            elsif Attribute_Name = "default" then
               Document.Default :=
                 To_Unbounded_String (Attribute_Value);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Column." &
                    Attribute_Name & ": ignored");
            end if;

         when Table_Body =>
            null;

         when Row =>
            null;

         when Error =>
            null;
      end case;
   end On_Attribute;

   ------------------
   -- On_Close_Tag --
   ------------------

   procedure On_Close_Tag
     (Document : in out XML_DB_Reader_Type;
      Tag_Name : String)
   is
   begin
      case Document.State is
         when Start =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Tag_Name & ": ignored");
            Document.State := Error;
         when Top_Level =>
            Document.State := Start;

         when Table_Spec =>

            Document.Db.Append (Document.Table);

            Document.State := Top_Level;

         when Column =>

            declare
               use Ada.Strings.Unbounded;
               Field : Kit.Schema.Fields.Field_Type;
            begin
               Field.Create_Field
                 (To_String (Document.Field_Name),
                  Get_Type
                    (Document.Db, To_String (Document.Field_Type),
                     To_String (Document.Reference)));

               Document.Table.Append
                 (Item      => Field);
            end;

            Document.State := Table_Spec;

         when Table_Body =>
            Document.State := Top_Level;

         when Row =>
            Document.State := Table_Body;

         when Error =>
            null;
      end case;
   end On_Close_Tag;


   -----------------
   -- On_Open_Tag --
   -----------------

   procedure On_Open_Tag
     (Document : in out XML_DB_Reader_Type;
      Tag_Name : String)
   is
   begin
      case Document.State is
         when Start =>
            Document.State := Top_Level;
            if Document.Db.Name = "" then
               Document.Db.Create_Database (Tag_Name);
            elsif Document.Db.Name /= Tag_Name then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "warning: while reading database " & Document.Db.Name);
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "warning: found database definition for " & Tag_Name);
            end if;
         when Top_Level =>
            if Tag_Name = "Table" then
               Document.State := Table_Spec;
            else
               if Document.Db.Contains (Tag_Name) then
                  Document.State := Table_Body;
               else
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Tag_Name & ": unknown table");
                  Document.State := Error;
               end if;
            end if;
         when Table_Spec =>
            if Tag_Name = "Column" then
               Document.State := Column;
               Document.Field_Name :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Document.Field_Type :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Document.Reference :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Document.Default :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Document.Key := False;
               Document.Unique := False;
               Document.Primary := False;
               Document.Auto := False;
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Tag_Name & ": expected a column");
               Document.State := Error;
            end if;
         when Column =>
            null;
         when Table_Body =>
            if Tag_Name = "Row" then
               Document.State := Row;
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Tag_Name & ": expected a row");
               Document.State := Error;
            end if;
         when Row =>
            null;
         when Error =>
            null;
      end case;
   end On_Open_Tag;

   -------------------
   -- XML_DB_Reader --
   -------------------

   function XML_DB_Reader
     (Db : Kit.Schema.Databases.Database_Access)
      return XML.XML_Document'Class
   is
   begin
      return Result : XML_DB_Reader_Type do
         Result.Db := Db;
      end return;
   end XML_DB_Reader;

end Kit.Import.XML_DB;
