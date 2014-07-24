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

   for Item of Intermediate.Select_By_G loop
      Item.Set_F (Integer'Image (Integer'Value (Item.F) * 1000));
   end loop;

   Database.Close;

   Database.Open;

   for Item of Concrete.Select_By_F loop
      Ada.Text_IO.Put_Line (Item.F & Item.G & Item.H);
   end loop;

   Database.Close;

end Test_Diamond_1.Driver;
