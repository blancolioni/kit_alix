with Ada.Text_IO;

with Test_Diamond_1.Intermediate;
with Test_Diamond_1.Concrete;
with Test_Diamond_1.Database;

procedure Test_Diamond_1.Driver is
begin
   Database.Create;
   for F in 1 .. 10 loop
      for G in 1 .. 10 loop
         for H in 1 .. 10 loop
            declare
               Item : Test_Diamond_1.Concrete.Concrete_Type :=
                 Test_Diamond_1.Concrete.Create;
            begin
               Item.Set_F (F'Img);
               Item.Set_G (G'Img);
               Item.Set_H (H'Img);
            end;
         end loop;
      end loop;
   end loop;

   Database.Close;

   Database.Open;

   declare
      use Intermediate;
      Item : Intermediate_Type := First_By_G;
   begin
      while Item.Has_Element loop
         Item.Set_F (Integer'Image (Integer'Value (Item.F) * 1000));
         Item.Next;
      end loop;
   end;

   Database.Close;

   Database.Open;

   declare
      use Concrete;
      Item : Concrete_Type := First_By_F;
   begin
      while Item.Has_Element loop
         Ada.Text_IO.Put_Line (Item.F & Item.G & Item.H);
         Item.Next;
      end loop;
   end;

   Database.Close;

end Test_Diamond_1.Driver;
