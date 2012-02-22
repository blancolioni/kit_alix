with Aquarius.Drys.Declarations;

package body Kit.Types.Enumerated is

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (To      : in out Enumerated_Type;
      Literal : String)
   is
   begin
      To.Literals.Append (Literal);
   end Add_Literal;

   --------------------
   -- Return_Subtype --
   --------------------

   function Return_Subtype
     (Item : Enumerated_Type)
      return String
   is
   begin
      return Item.Ada_Name;
   end Return_Subtype;

   --------------------
   -- To_Declaration --
   --------------------

   overriding
   function To_Declaration
     (Item : Enumerated_Type)
      return Aquarius.Drys.Declaration'Class
   is
      Definition : Aquarius.Drys.Enumeration_Type_Definition;
   begin
      for Literal of Item.Literals loop
         Definition.New_Literal (Literal);
      end loop;
      return Aquarius.Drys.Declarations.New_Full_Type_Declaration
        (Item.Ada_Name, Definition);
   end To_Declaration;

end Kit.Types.Enumerated;
