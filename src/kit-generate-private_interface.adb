with Aquarius.Drys.Blocks;
with Aquarius.Drys.Expressions;
with Aquarius.Drys.Statements;
with Aquarius.Drys.Types;

with Kit.Fields;

package body Kit.Generate.Private_Interface is

   procedure Create_Database_Record
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Memory_Record
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);
   pragma Unreferenced (Create_Memory_Record);

   procedure Create_Read_Write_Procedures
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   procedure Create_Key_Mutexes
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class);

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   procedure Create_Database_Record
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      Have_String_With : Boolean := False;

      Record_Defn : Aquarius.Drys.Types.Record_Type_Definition;

      procedure Add_Component (Field : Kit.Fields.Field_Type'Class);
      procedure Add_Base_Index (Base : Kit.Tables.Table_Type'Class);

      --------------------
      -- Add_Base_Index --
      --------------------

      procedure Add_Base_Index (Base : Kit.Tables.Table_Type'Class) is
      begin
         Record_Defn.Add_Component ("T" & Base.Index_Image & "_Idx",
                                    "Marlowe.Database_Index");
      end Add_Base_Index;

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component (Field : Kit.Fields.Field_Type'Class) is
         use Kit.Tables;
      begin
         Aquarius.Drys.Types.Add_Component
           (Record_Defn, Field.Ada_Name,
            Aquarius.Drys.Named_Subtype
              (Field.Get_Field_Type.Record_Subtype));

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

      Record_Defn.Add_Component
        ("Actual_Type",
         Aquarius.Drys.Named_Subtype ("Record_Type"));

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
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      procedure Add_Mutex (Base : Kit.Tables.Table_Type'Class;
                           Key  : Kit.Tables.Key_Cursor);

      ---------------
      -- Add_Mutex --
      ---------------

      procedure Add_Mutex (Base : Kit.Tables.Table_Type'Class;
                           Key  : Kit.Tables.Key_Cursor)
      is
         pragma Unreferenced (Base);
      begin
         Impl.Append
           (Aquarius.Drys.Declarations.New_Object_Declaration
              (Kit.Tables.Ada_Name (Key) & "_Key_Mutex",
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
     (Table : in     Kit.Tables.Table_Type'Class;
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
     (Table : in     Kit.Tables.Table_Type'Class;
      Impl  : in out Aquarius.Drys.Declarations.Package_Type'Class)
   is

      function Call_Marlowe (Write : Boolean)
                             return Aquarius.Drys.Statement'Class;

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
         Stat.Add_Actual_Argument ("Item'Address");
         return Stat;
      end Call_Marlowe;

   begin

      for Write in Boolean loop
         declare
            Block : Aquarius.Drys.Blocks.Block_Type;
         begin

            Block.Add_Statement (Call_Marlowe (Write));

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
         end;
      end loop;
   end Create_Read_Write_Procedures;

   --------------------------------
   -- Generate_Private_Interface --
   --------------------------------

   function Generate_Private_Interface
     (Db    : in out Kit.Databases.Database_Type;
      Table : in     Kit.Tables.Table_Type'Class;
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

      return Impl_Package;
   end Generate_Private_Interface;

end Kit.Generate.Private_Interface;
