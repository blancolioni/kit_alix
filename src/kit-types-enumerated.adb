with Aquarius.Drys.Declarations;
with Aquarius.Drys.Expressions;

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
      if To.Literals.Last_Index = 2 then
         To.Size := 1;
      elsif To.Literals.Last_Index mod 256 = 0 then
         To.Size := To.Size + 1;
      end if;
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

   function To_Storage_Array
     (Item        : Enumerated_Type;
      Object_Name : String)
      return Aquarius.Drys.Expression'Class
   is
      use Aquarius.Drys, Aquarius.Drys.Expressions;
   begin
      return New_Function_Call_Expression
        ("Marlowe.Key_Storage.To_Storage_Array",
         New_Function_Call_Expression
           (Item.Ada_Name & "'Pos",
            Object_Name),
         Literal (Item.Size));
   end To_Storage_Array;

end Kit.Types.Enumerated;
