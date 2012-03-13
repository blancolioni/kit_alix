with Kit;

package body Abydos.Expressions is

   type Value_Expression_Type is
     new Root_Expression_Type with
      record
         V : Abydos.Values.Value;
      end record;

   function Evaluate (Expr    : Value_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value;

   type Object_Expression_Type is
     new Root_Expression_Type with
      record
         Name : access String;
         Args : access Array_Of_Expressions;
      end record;

   function Evaluate (Expr    : Object_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value;

   type Field_Expression_Type is
     new Root_Expression_Type with
      record
         Rec   : Expression;
         Field : access String;
      end record;

   function Evaluate (Expr    : Field_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value;

   type Select_Expression_Type is
     new Root_Expression_Type with
      record
         Table, Field, Key, Key_Value, Condition : Expression;
      end record;

   function Evaluate (Expr    : Select_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (E : Expression;
      Env : Abydos.Environments.Environment)
      return Abydos.Values.Value
   is
   begin
      return E.Evaluate (Env);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr    : Value_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value
   is
      pragma Unreferenced (Env);
   begin
      return Expr.V;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr    : Object_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value
   is
      Args : Values.Array_Of_Values (Expr.Args'Range);
   begin
      for I in Args'Range loop
         Args (I) := Expr.Args (I).Evaluate (Env);
      end loop;
      return Environments.Apply (Env, Expr.Name.all, Args);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr    : Field_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value
   is
      V : constant Values.Value := Expr.Rec.Evaluate (Env);
   begin
      return Values.Index (V, Values.To_Value (Expr.Field.all));
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr    : Select_Expression_Type;
                      Env     : Abydos.Environments.Environment)
                     return Values.Value
   is
      use Abydos.Values;
      Table_Name : constant String := To_String (Expr.Table.Evaluate (Env));
      Field_Name : constant String := To_String (Expr.Field.Evaluate (Env));
      Key_Name   : constant String := To_String (Expr.Key.Evaluate (Env));
      Key_Value  : constant String :=
        To_String (Expr.Key_Value.Evaluate (Env));
      It : Kit.Root_Database_Record'Class :=
        Env.Scan_By_Key_Value (Table_Name, Key_Name, Key_Value);
      Result     : Value;
   begin
      while It.Has_Element loop
         declare
            Local_Env : Environments.Environment :=
              Environments.New_Environment (It);
         begin
            if To_Boolean (Expr.Condition.Evaluate (Local_Env)) then
               Append (Result, To_Value (It.Get (Field_Name)));
            end if;
            Local_Env.Close;
         end;
      end loop;
      return Result;
   end Evaluate;

   -------------------
   -- Function_Call --
   -------------------

   function Function_Call
     (Function_Name : String;
      Arguments     : Array_Of_Expressions)
      return Expression
   is
   begin
      return new Object_Expression_Type'
        (Root_Expression_Type with
           new String'(Function_Name),
         new Array_Of_Expressions'(Arguments));
   end Function_Call;

   ------------
   -- Object --
   ------------

   function Object (Name : String) return Expression is
   begin
      return new Object_Expression_Type'(Root_Expression_Type with
                                           new String'(Name), null);
   end Object;

   ------------------
   -- Select_Field --
   ------------------

   function Select_Field
     (Rec  : Expression;
      Field : String)
      return Expression
   is
   begin
      return new Field_Expression_Type'
        (Root_Expression_Type with Rec, new String'(Field));
   end Select_Field;

   ---------------
   -- Selection --
   ---------------

   function Selection
     (Table      : Expression;
      Field      : Expression;
      Key        : Expression;
      Key_Value  : Expression;
      Condition  : Expression)
      return Expression
   is
   begin
      return new Select_Expression_Type'
        (Root_Expression_Type with Table, Field, Key, Key_Value, Condition);
   end Selection;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (V : Values.Value)
      return Expression
   is
   begin
      return new Value_Expression_Type'(Root_Expression_Type with V);
   end To_Expression;

end Abydos.Expressions;
