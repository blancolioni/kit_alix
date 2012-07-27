with Ada.Strings.Fixed;

with System.Storage_Elements;

with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Marlowe;

with Kit.Schema.Fields;

package body Kit.Generate.Private_Interface is

   procedure Create_Compound_Key_To_Storage_Functions
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Database_Record
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Memory_Record
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);
   pragma Unreferenced (Create_Memory_Record);

   procedure Create_Read_Write_Procedures
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Key_Mutexes
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   ----------------------------------------------
   -- Create_Compound_Key_To_Storage_Functions --
   ----------------------------------------------

   procedure Create_Compound_Key_To_Storage_Functions
     (Db    : in     Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);

      procedure Create_Key_To_Storage (Key : Kit.Schema.Tables.Key_Cursor);
      function To_Storage_Expression
        (Key   : Kit.Schema.Tables.Key_Cursor;
         Index : Positive)
         return Aquarius.Drys.Expression'Class;

      ---------------------------
      -- Create_Key_To_Storage --
      ---------------------------

      procedure Create_Key_To_Storage (Key : Kit.Schema.Tables.Key_Cursor) is
         use Kit.Schema.Tables;
      begin
         if Is_Compound_Key (Key) then
            declare
               use Aquarius.Drys, Aquarius.Drys.Declarations;
               Fn : Subprogram_Declaration'Class :=
                      Aquarius.Drys.Declarations.New_Function
                        (Ada_Name (Key) & "_To_Storage",
                         "System.Storage_Elements.Storage_Array",
                         To_Storage_Expression
                           (Key, 1));
            begin
               for I in 1 .. Compound_Field_Count (Key) loop
                  Fn.Add_Formal_Argument
                    (Compound_Field (Key, I).Ada_Name,
                     Compound_Field (Key, I).Get_Field_Type.Argument_Subtype);
               end loop;
               Fn.Add_Local_Declaration
                 (Use_Type ("System.Storage_Elements.Storage_Array"));
               Top.Append (Fn);
            end;
         end if;
      end Create_Key_To_Storage;

      ---------------------------
      -- To_Storage_Expression --
      ---------------------------

      function To_Storage_Expression
        (Key   : Kit.Schema.Tables.Key_Cursor;
         Index : Positive)
         return Aquarius.Drys.Expression'Class
      is
         use Aquarius.Drys.Expressions;
         Field : constant Kit.Schema.Fields.Field_Type'Class :=
                   Kit.Schema.Tables.Compound_Field (Key, Index);
         This  : constant Aquarius.Drys.Expression'Class :=
                   Field.Get_Field_Type.To_Storage_Array
                     (Field.Ada_Name);
      begin
         if Index = Kit.Schema.Tables.Compound_Field_Count (Key) then
            return This;
         else
            return Operator ("&", This,
                             To_Storage_Expression (Key, Index + 1));
         end if;
      end To_Storage_Expression;

   begin
      Table.Scan_Keys (Create_Key_To_Storage'Access);
   end Create_Compound_Key_To_Storage_Functions;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   procedure Create_Database_Record
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      Have_String_With : Boolean := False;

      Record_Defn : Aquarius.Drys.Types.Record_Type_Definition;

      procedure Add_Component (Field : Kit.Schema.Fields.Field_Type'Class);
      procedure Add_Base_Index (Base : Kit.Schema.Tables.Table_Type'Class);

      --------------------
      -- Add_Base_Index --
      --------------------

      procedure Add_Base_Index (Base : Kit.Schema.Tables.Table_Type'Class) is
      begin
         Record_Defn.Add_Component ("T" & Base.Index_Image & "_Idx",
                                    "Marlowe.Database_Index");
      end Add_Base_Index;

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component (Field : Kit.Schema.Fields.Field_Type'Class) is
         use Kit.Schema.Tables;
      begin
         if Field.Get_Field_Type.Has_Default_Value then
            Aquarius.Drys.Types.Add_Component
              (Record_Defn, Field.Ada_Name,
               Aquarius.Drys.Named_Subtype
                 (Field.Get_Field_Type.Record_Subtype),
               Field.Get_Field_Type.Default_Value);
         else
            Aquarius.Drys.Types.Add_Component
              (Record_Defn, Field.Ada_Name,
               Aquarius.Drys.Named_Subtype
                 (Field.Get_Field_Type.Record_Subtype));
         end if;

         if not Have_String_With
           and then Field.Get_Field_Type.Is_String
         then
            Impl.With_Package ("Kit.Strings");
            Have_String_With := True;
         end if;

      end Add_Component;

   begin

      Record_Defn.Add_Component
        ("Magic",
         Aquarius.Drys.Named_Subtype ("Integer"),
         Aquarius.Drys.Object (Table.Ada_Name & "_Magic"));

      Record_Defn.Add_Component
        ("Deleted",
         Aquarius.Drys.Named_Subtype ("Boolean"),
         Aquarius.Drys.Object ("False"));

      Table.Iterate (Process     => Add_Base_Index'Access,
                     Inclusive   => False);

      Table.Scan_Fields (Add_Component'Access);

      Impl.Append
        (Aquarius.Drys.Declarations.New_Full_Type_Declaration
           (Identifier => Table.Ada_Name & "_Database_Record",
            Definition => Record_Defn));

   end Create_Database_Record;

   ------------------------
   -- Create_Key_Mutexes --
   ------------------------

   procedure Create_Key_Mutexes
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      procedure Add_Mutex (Base : Kit.Schema.Tables.Table_Type'Class;
                           Key  : Kit.Schema.Tables.Key_Cursor);

      ---------------
      -- Add_Mutex --
      ---------------

      procedure Add_Mutex (Base : Kit.Schema.Tables.Table_Type'Class;
                           Key  : Kit.Schema.Tables.Key_Cursor)
      is
         pragma Unreferenced (Base);
      begin
         Impl.Append
           (Aquarius.Drys.Declarations.New_Object_Declaration
              (Kit.Schema.Tables.Ada_Name (Key) & "_Key_Mutex",
               "Kit.Mutex.Mutex_Type"));
      end Add_Mutex;

   begin
      Table.Scan_Keys (Add_Mutex'Access);

      Impl.Append
        (Aquarius.Drys.Declarations.New_Object_Declaration
           ("File_Mutex",
            "Kit.Mutex.Mutex_Type"));

   end Create_Key_Mutexes;

   --------------------------
   -- Create_Memory_Record --
   --------------------------

   procedure Create_Memory_Record
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      Record_Defn : Aquarius.Drys.Types.Record_Type_Definition;

   begin

      Record_Defn.Add_Parent ("Kit.Cache.Cache_Entry_Record");
      Record_Defn.Add_Component
        ("Item",
         Table.Ada_Name & "_Database_Record",
         Is_Access => True);

      Impl.Append
        (Aquarius.Drys.Declarations.New_Full_Type_Declaration
           (Identifier => Table.Ada_Name & "_Memory_Record",
            Definition => Record_Defn));

      Impl.Add_Separator;
      Impl.Append
        (Aquarius.Drys.Declarations.New_Full_Type_Declaration
           (Identifier => "Cached_" & Table.Ada_Name,
            Definition =>
              Aquarius.Drys.New_Access_Type
                (Table.Ada_Name & "_Memory_Record'Class",
                 Access_All => True)));

   end Create_Memory_Record;

   ----------------------------------
   -- Create_Read_Write_Procedures --
   ----------------------------------

   procedure Create_Read_Write_Procedures
     (Table : in     Kit.Schema.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      function Call_Marlowe (Write : Boolean)
                             return Aquarius.Drys.Statement'Class;

      procedure Create_Transfer (Write : Boolean);

      ------------------
      -- Call_Marlowe --
      ------------------

      function Call_Marlowe (Write : Boolean)
                             return Aquarius.Drys.Statement'Class
      is
         use Aquarius.Drys;
         use Aquarius.Drys.Statements;
         Name : constant String := (if Write then "Write_Record"
                                    else "Get_Record");
         Stat : Procedure_Call_Statement :=
                  New_Procedure_Call_Statement
                    ("Marlowe.Btree_Handles." & Name);
      begin
         Stat.Add_Actual_Argument ("Marlowe_Keys.Handle");
         Stat.Add_Actual_Argument (Table.Ada_Name & "_Table_Index");
         Stat.Add_Actual_Argument ("Ref");
         Stat.Add_Actual_Argument ("Storage'Address");
         return Stat;
      end Call_Marlowe;

      ---------------------
      -- Create_Transfer --
      ---------------------

      procedure Create_Transfer (Write : Boolean) is

         use System.Storage_Elements;

         Block : Aquarius.Drys.Blocks.Block_Type;

         procedure Handle_Storage (Field : Kit.Schema.Fields.Field_Type'Class);
         procedure Handle_Base (Base : Kit.Schema.Tables.Table_Type'Class);

         -----------------
         -- Handle_Base --
         -----------------

         procedure Handle_Base
           (Base : Kit.Schema.Tables.Table_Type'Class)
         is
            use Ada.Strings, Ada.Strings.Fixed;
            use Aquarius.Drys, Aquarius.Drys.Statements;
            Start  : constant Storage_Offset := Table.Base_Start (Base);
            Finish : constant Storage_Offset :=
                       Start +
                       Marlowe.Database_Index'Size / System.Storage_Unit - 1;
            Rec    : constant String :=
                       "T" & Base.Index_Image & "_Idx";
            S         : constant String :=
                          Trim (Storage_Offset'Image (Start), Left);
            F         : constant String :=
                          Trim (Storage_Offset'Image (Finish), Left);
            Store     : constant String :=
                          "Storage" & " (" & S & " .. " & F & ")";
            Proc_Name : constant String :=
                          (if Write then "To_Storage" else "From_Storage");
         begin
            Block.Append
              (New_Procedure_Call_Statement
                 ("Marlowe.Key_Storage." & Proc_Name,
                  Object ("Item." & Rec),
                  Object (Store)));
         end Handle_Base;

         --------------------
         -- Handle_Storage --
         --------------------

         procedure Handle_Storage
           (Field : Kit.Schema.Fields.Field_Type'Class)
         is
            Start  : constant Storage_Offset := Table.Field_Start (Field);
            Finish : constant Storage_Offset :=
                       Table.Field_Start (Field) +
                       Storage_Offset (Field.Size) - 1;
            Rec    : constant String :=
                       "Item." & Field.Ada_Name;
         begin
            Block.Append
              (Field.Get_Field_Type.Storage_Array_Transfer
                 (Object_Name       => Rec,
                  To_Storage        => Write,
                  Storage_Name      => "Storage",
                  Start             => Start,
                  Finish            => Finish));
         end Handle_Storage;

      begin
         Block.Add_Declaration
           (Aquarius.Drys.Declarations.New_Object_Declaration
              ("Storage",
               "System.Storage_Elements.Storage_Array "
               & "(0 .."
               & System.Storage_Elements.Storage_Offset'Image
                 (Table.Length - 1)
               & ")"));

         if Write then
            Table.Scan_Fields (Handle_Storage'Access);
            Table.Iterate (Handle_Base'Access, Inclusive => False);
         end if;

         Block.Add_Statement (Call_Marlowe (Write));

         if not Write then
            Table.Scan_Fields (Handle_Storage'Access);
            Table.Iterate (Handle_Base'Access, Inclusive => False);
         end if;

         declare
            P     : Aquarius.Drys.Declarations.Subprogram_Declaration :=
                      Aquarius.Drys.Declarations.New_Procedure
                        (Name  => (if Write then "Write" else "Read"),
                         Block => Block);
            Mode  : constant Aquarius.Drys.Declarations.Argument_Mode :=
                      (if Write then Aquarius.Drys.Declarations.In_Argument
                       else Aquarius.Drys.Declarations.Out_Argument);
         begin
            P.Add_Formal_Argument ("Ref", "Marlowe.Database_Index");
            P.Add_Formal_Argument ("Item", Mode,
                                   Table.Implementation_Record_Type);

            Impl.Append (P);
         end;
      end Create_Transfer;

   begin

      for Write in Boolean loop
         Create_Transfer (Write);
      end loop;

   end Create_Read_Write_Procedures;

   --------------------------------
   -- Generate_Private_Interface --
   --------------------------------

   function Generate_Private_Interface
     (Db    : in out Kit.Schema.Databases.Database_Type;
      Table : in     Kit.Schema.Tables.Table_Type'Class;
      Top   : in     Aquarius.Drys.Declarations.Package_Type'Class)
      return Aquarius.Drys.Declarations.Package_Type'Class
   is
      Impl_Package : Aquarius.Drys.Declarations.Package_Type'Class :=
                       Top.New_Child_Package (Table.Ada_Name & "_Impl");
   begin
      Impl_Package.Set_Private;
      --  Impl_Package.With_Package ("Kit.Cache");
      Impl_Package.With_Package ("Kit.Mutex");
      Impl_Package.With_Package ("System.Storage_Elements");
      Impl_Package.With_Package ("Marlowe.Btree_Handles", Body_With => True);
      Impl_Package.With_Package ("Marlowe.Key_Storage",
                                 Body_With => True);
      Impl_Package.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                                Body_With => True);
      Impl_Package.Append
        (Aquarius.Drys.Declarations.New_Constant_Declaration
           (Table.Ada_Name & "_Magic",
            Aquarius.Drys.Literal (Table.Magic_Number)));
      Impl_Package.Add_Separator;

      Create_Database_Record (Table, Impl_Package);
      Impl_Package.Add_Separator;

      declare
         Block : Aquarius.Drys.Blocks.Block_Type;
      begin
         Block.Add_Declaration
           (Aquarius.Drys.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Offset"));
         Block.Add_Statement
           (Aquarius.Drys.Statements.New_Return_Statement
              (Aquarius.Drys.Expressions.Operator
                 ("/",
                  Aquarius.Drys.Object
                    (Table.Ada_Name & "_Database_Record'Size"),
                  Aquarius.Drys.Object ("System.Storage_Unit"))));

         Impl_Package.Append
           (Aquarius.Drys.Declarations.New_Function
              ("Disk_Storage_Units",
               "System.Storage_Elements.Storage_Count",
               Block));
      end;
--        Create_Memory_Record (Table, Impl_Package);
--        Impl_Package.Add_Separator;

      Create_Read_Write_Procedures (Table, Impl_Package);
      Create_Key_Mutexes (Table, Impl_Package);

      Create_Compound_Key_To_Storage_Functions
        (Db, Table, Impl_Package);

      return Impl_Package;
   end Generate_Private_Interface;

end Kit.Generate.Private_Interface;
