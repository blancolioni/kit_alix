with Harriet.Named_Object;

procedure Harriet.Driver is
begin

   for I in 1 .. 1000 loop
      declare
         Item : Harriet.Named_Object.Named_Object_Type :=
                  Harriet.Named_Object.Create;
      begin
         Item.Set_Name (I'Img);
      end;
   end loop;

end Harriet.Driver;
