with WL.String_Maps;

package body Kit.SQL.Requests is

   type Constraint_Status is
      record
         Have_Constraint : Boolean := False;
         Constraint      : Kit.SQL.Constraints.Field_Value_Type;
      end record;

   type Constraint_Status_Array is
     array (Kit.SQL.Constraints.Constraint_Class) of Constraint_Status;

   type Field_Constraints is
      record
         Table_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Field_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Field_Ref   : Kit.SQL.Database.Field_Reference :=
                         Kit.SQL.Database.No_Field;
         Field_Key   : Kit.SQL.Database.Key_Reference;
         Constraints : Constraint_Status_Array;
      end record;

   package Field_Constraint_Maps is
     new WL.String_Maps (Field_Constraints);

   ------------
   -- Create --
   ------------

   procedure Create
     (Request     : in out Request_Type'Class;
      Tables      : Kit.SQL.Tables.Lists.List;
      Constraints : Kit.SQL.Constraints.Constraint_List'Class)
   is

      Constraint_Map : Field_Constraint_Maps.Map;

      procedure Apply_Constraint
        (Constraint : Kit.SQL.Constraints.Constraint_Type'Class);

      ----------------------
      -- Apply_Constraint --
      ----------------------

      procedure Apply_Constraint
        (Constraint : Kit.SQL.Constraints.Constraint_Type'Class)
      is
         Field_Key : constant String :=
                       Constraint.Table_Name & "--" & Constraint.Field_Name;
      begin
         Request.Constraints.Add (Constraint);
         if not Constraint_Map.Contains (Field_Key) then
            declare
               Field : constant Kit.SQL.Database.Field_Reference :=
                         Kit.SQL.Database.Get_Field
                           (Request.Table, Constraint.Field_Name);
               New_Rec : constant Field_Constraints :=
                           Field_Constraints'
                             (Field_Ref => Field,
                              Field_Key =>
                                Kit.SQL.Database.Get_Default_Field_Key (Field),
                              others    => <>);
            begin
               Constraint_Map.Insert (Field_Key, New_Rec);
            end;
         end if;

         declare
            Field : Field_Constraints renames Constraint_Map (Field_Key);
         begin
            Field.Constraints (Constraint.Class) :=
              (True, Constraint.Value);
         end;
      end Apply_Constraint;

   begin
      Request.Table := Kit.SQL.Database.Get_Table
        (Tables.First_Element.Table_Name);
      Constraints.Iterate (Apply_Constraint'Access);

      declare
         use all type Kit.SQL.Constraints.Constraint_Class;
         Have_Equality_Key : Boolean := False;
--           Have_Min_Max_Key  : Boolean := False;
--           Have_Key          : Boolean := False;
         Key               : Kit.SQL.Database.Key_Reference :=
                               Kit.SQL.Database.Get_Default_Key
                                 (Request.Table);
         Min_Value : Kit.SQL.Constraints.Field_Value_Type :=
                       Kit.SQL.Constraints.No_Value;
         Max_Value : Kit.SQL.Constraints.Field_Value_Type :=
                       Kit.SQL.Constraints.No_Value;
         Min_Closed : constant Boolean := True;
         Max_Closed : constant Boolean := True;
      begin
         for Constraint of Constraint_Map loop
            if Kit.SQL.Database.Is_Field_Key
              (Constraint.Field_Key, Constraint.Field_Ref)
            then
               declare
                  Equality : constant Constraint_Status :=
                               Constraint.Constraints (Equal);
               begin
                  if Equality.Have_Constraint then
                     if not Have_Equality_Key then
                        Have_Equality_Key := True;
                        Key := Constraint.Field_Key;
                        Min_Value := Equality.Constraint;
                        Max_Value := Equality.Constraint;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         Request.Search_Key := Key;

         Request.Search_Start :=
           Storage_Array_Holders.To_Holder
             (Kit.SQL.Constraints.To_Storage
                (Min_Value,
                 Kit.SQL.Database.Get_Minimum (Key)));
         Request.Search_Finish :=
           Storage_Array_Holders.To_Holder
             (Kit.SQL.Constraints.To_Storage
                (Max_Value,
                 Kit.SQL.Database.Get_Maximum (Key)));
         Request.Search_Start_Closed := Min_Closed;
         Request.Search_Finish_Closed := Max_Closed;
      end;

   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Request   : Request_Type'Class;
      On_Record : not null access
        procedure (Table : Kit.SQL.Database.Table_Reference;
                   Record_Index : Marlowe.Database_Index))
   is
      procedure Check_Constraints
        (Record_Index : Marlowe.Database_Index);

      -----------------------
      -- Check_Constraints --
      -----------------------

      procedure Check_Constraints
        (Record_Index : Marlowe.Database_Index)
      is
      begin
         On_Record (Request.Table, Record_Index);
      end Check_Constraints;

   begin
      Kit.SQL.Database.Scan
        (Table      => Request.Table,
         Key        => Request.Search_Key,
         Min_Value  => Request.Search_Start.Element,
         Max_Value  => Request.Search_Finish.Element,
         Min_Closed => Request.Search_Start_Closed,
         Max_Closed => Request.Search_Finish_Closed,
         Callback   => Check_Constraints'Access);
   end Execute;

end Kit.SQL.Requests;
