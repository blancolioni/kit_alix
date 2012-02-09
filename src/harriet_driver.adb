with Ada.Text_IO;

with Harriet.Named_Object;

with Drys;

procedure Harriet_Driver is
begin
   for I in 1 .. 1000 loop
      declare
         New_Item : Harriet.Named_Object.Named_Object_Type :=
                      Harriet.Named_Object.Create;
      begin
         New_Item.Set_Name (I'Img);
      end;
   end loop;

   declare
      Item : Harriet.Named_Object.Named_Object_Type :=
               Harriet.Named_Object.First_By_Name;
   begin
      while Item.Has_Element loop
         Ada.Text_IO.Put_Line (Item.Name);
         Item.Next;
      end loop;
   end;

   declare
      Item : Harriet.Named_Object.Named_Object_Type :=
               Harriet.Named_Object.Get_By_Name (" 123");
   begin
     if Item.Has_Element then
         Ada.Text_IO.Put_Line (Item.Name);
      end if;
   end;

   declare
      Table_Name : constant String := "named_object";
      Harriet_Package : constant Drys.Package_Declaration :=
                          Drys.New_Package ("harriet");
      P : Drys.Package_Declaration :=
                          Harriet_Package.New_Child_Package
                            (Table_Name);
      Interface_Name  : constant String :=
                          Table_Name & "_interface";
      Type_Name       : constant String :=
                          Table_Name & "_type";
      Reference_Name  : constant String :=
                          Table_Name & "_reference";
      Implementation_Package : constant String :=
                                      Table_Name & "_impl";
      Implementation_Type    : constant String :=
                                 Implementation_Package & "." &
      Table_Name & "_implementation";

   begin
      P.Create_Interface (Interface_Name);
      P.Create_Class_Wide_Subtype (Interface_Name, Type_Name);
      P.Abstract_Function ("name",
                           Drys.Argument ("item", Interface_Name),
                           "String");

      P.Abstract_Procedure ("set_name",
        (Drys.Inout_Argument ("item", Interface_Name),
         Drys.Argument ("value", "string")));

      P.Create_Function
        ("create",
         Type_Name,
         Drys.Statement_Body
           (Drys.Return_Statement
              ("result", Implementation_Type,
               Drys.Sequence_Of_Statements
                 ((Drys.Assignment_Statement
                     ("Result." & Table_Name & "_Record.Ref",
                      Implementation_Package & ".Create"),
                   Drys.Assignment_Statement
                     ("Result.Created", "True"),
                   Drys.Assignment_Statement
                     ("Result.Dirty", "True"))))));

      P.Create_Function
        ("get",
         Drys.Argument ("reference", Reference_Name),
         Type_Name,
         Drys.statement_body
           (Drys.Return_Statement
              ("result", Implementation_Type,
               Drys.Procedure_Call_Statement
                 (Implementation_Package & ".read",
                  "reference",
                  "result." & Table_Name & "_record"))));
      P.Write ("/home/fraser");

   end;

end Harriet_Driver;
