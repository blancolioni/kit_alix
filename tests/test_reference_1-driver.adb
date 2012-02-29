with Test_Reference_1.Database;         use Test_Reference_1.Database;
with Test_Reference_1.Table_1;          use Test_Reference_1.Table_1;
with Test_Reference_1.Table_2;          use Test_Reference_1.Table_2;

procedure Test_Reference_1.Driver is
begin
   Create;

   for I in 1 .. 100 loop
      declare
         Item : Table_1_Type := Create;
      begin
         Item.Set_X (I'Img);
         Item.Set_Processed (False);
      end;
   end loop;

   Close;

   Open;

   declare
      Item_1 : Table_1_Type := First_By_X;
   begin
      while Item_1.Has_Element loop
         Item_1.Set_Processed (True);
         for I in 1 .. 100 loop
            if I'Img /= Item_1.X then
               declare
                  Item_2 : Table_1_Type := First_By_X (I'Img);
                  Link   : Table_2_Type := Create;
               begin
                  Link.Set_Table_1_1 (Item_1.Reference);
                  Link.Set_Table_1_2 (Item_2.Reference);
               end;
            end if;
         end loop;
         Item_1.Next;
      end loop;
   end;

   Close;

end Test_Reference_1.Driver;
