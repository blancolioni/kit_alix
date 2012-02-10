with Aquarius.Drys.Declarations;
with Aquarius.Drys.Types;

with Kit.Tables;

with Kit.Generate.Database_Package;
with Kit.Generate.Get_From_Cache;
with Kit.Generate.Marlowe_Keys_Package;
with Kit.Generate.Public_Interface;
with Kit.Generate.Private_Interface;

package body Kit.Generate is

   procedure Create_Handle_Function
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   pragma Unreferenced (Create_Handle_Function);

   procedure Create_Table_Type
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   procedure Create_Key_Type
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   procedure Create_Reference_Types
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   procedure Create_Record_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   procedure Create_Search_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   procedure Create_Locking_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type);

   ----------------------------
   -- Create_Handle_Function --
   ----------------------------

   procedure Create_Handle_Function
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys.Declarations;

      Handle : constant Subprogram_Declaration :=
                 New_Function ("Handle",
                               "Kit.Access_Control.Access_Handle",
                               Aquarius.Drys.Object ("Local_Handle"));
      Local_Handle : constant Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Local_Handle",
                          "Kit.Access_Control.Access_Handle");
   begin
      Top.With_Package ("Kit.Access_Control");
      Top.Append_To_Body (Local_Handle);
      Top.Append (Handle);
      Top.Add_Separator;
   end Create_Handle_Function;

   ---------------------
   -- Create_Key_Type --
   ---------------------

   procedure Create_Key_Type
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      procedure Add_Table_Keys
        (Table : Kit.Tables.Table_Type'Class);

      --------------------
      -- Add_Table_Keys --
      --------------------

      procedure Add_Table_Keys
        (Table : Kit.Tables.Table_Type'Class)
      is

         Key_Type_Definition : Aquarius.Drys.Enumeration_Type_Definition;

         procedure Add_Key_Type_Literal
           (Base : Kit.Tables.Table_Type'Class;
            Item : Kit.Tables.Key_Cursor);

         --------------------------
         -- Add_Key_Type_Literal --
         --------------------------

         procedure Add_Key_Type_Literal
           (Base : Kit.Tables.Table_Type'Class;
            Item : Kit.Tables.Key_Cursor)
         is
            pragma Unreferenced (Base);
         begin
            Key_Type_Definition.New_Literal
              ("K_" & Table.Name & "_"
               & Kit.Tables.Name (Item));
         end Add_Key_Type_Literal;

      begin
         Key_Type_Definition.New_Literal ("K_None");
         Table.Scan_Keys (Add_Key_Type_Literal'Access);
         Top.Append
           (Aquarius.Drys.Declarations.New_Full_Type_Declaration
              (Table.Ada_Name & "_Key", Key_Type_Definition));
         Top.Append (Aquarius.Drys.Declarations.New_Separator);
      end Add_Table_Keys;

   begin
      Db.Iterate (Add_Table_Keys'Access);
   end Create_Key_Type;

   ------------------------------
   -- Create_Locking_Interface --
   ------------------------------

   procedure Create_Locking_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      pragma Unreferenced (Db);
      use Aquarius.Drys.Declarations;

      Lock_Context : Aquarius.Drys.Types.Record_Type_Definition;
      Lock_Record  : Type_Declaration;
      Lock_Access  : Type_Declaration;
      X_Lock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("X_Lock");
      S_Lock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("S_Lock");
      Unlock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("Unlock");
      Memory_Mutex : Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Memory_Mutex", "Kit.Mutex.Mutex_Type");
      Database_Mutex : Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Database_Mutex", "Kit.Mutex.Mutex_Type");

   begin
      Lock_Context.Set_Tagged;
      Lock_Context.Set_Abstract;
      Lock_Context.Add_Component ("S_Locked", "Boolean", "False");
      Lock_Context.Add_Component ("X_Locked", "Boolean", "False");
      Lock_Record :=
        Aquarius.Drys.Declarations.New_Full_Type_Declaration
          ("Lock_Context_Record", Definition => Lock_Context);
      Lock_Record.Set_Private_Spec;
      Top.Append (Lock_Record);
      Lock_Access :=
        Aquarius.Drys.Declarations.New_Full_Type_Declaration
          ("Lock_Context",
           Aquarius.Drys.New_Access_Type
             ("Lock_Context_Record'Class",
              Access_All => True));
      Lock_Access.Set_Private_Spec;
      Top.Append (Lock_Access);

      X_Lock.Set_Private_Spec;
      X_Lock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (X_Lock);

      S_Lock.Set_Private_Spec;
      S_Lock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (S_Lock);

      Unlock.Set_Private_Spec;
      Unlock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (Unlock);

      Memory_Mutex.Set_Private_Spec;
      Top.Append (Memory_Mutex);
      Database_Mutex.Set_Private_Spec;
      Top.Append (Database_Mutex);

   end Create_Locking_Interface;

   -----------------------------
   -- Create_Record_Interface --
   -----------------------------

   procedure Create_Record_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      use Aquarius.Drys, Aquarius.Drys.Declarations;
      pragma Unreferenced (Db);
      Record_Interface : Aquarius.Drys.Interface_Type_Definition;
   begin
      Record_Interface.Set_Limited;
      Top.Append (New_Full_Type_Declaration
                  ("Record_Interface", Record_Interface));
      Top.Add_Separator;

      Top.Append
        (New_Abstract_Procedure
           ("X_Lock",
            New_Inout_Argument
              ("Item", Named_Subtype ("Record_Interface"))));
      Top.Add_Separator;

   end Create_Record_Interface;

   ----------------------------
   -- Create_Reference_Types --
   ----------------------------

   procedure Create_Reference_Types
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is

      procedure Add_Reference_Type
        (Item : Kit.Tables.Table_Type'Class);

      ------------------------
      -- Add_Reference_Type --
      ------------------------

      procedure Add_Reference_Type
        (Item : Kit.Tables.Table_Type'Class)
      is
         use Aquarius.Drys, Aquarius.Drys.Declarations;
         Reference_Name : constant String :=
           Item.Name & "_Reference";
      begin
         Top.Append
           (New_Private_Type_Declaration
              (Reference_Name,
               New_Derived_Type ("Marlowe.Database_Index")));

         Top.Append
           (New_Deferred_Constant_Declaration
              ("Null_" & Item.Name & "_Reference",
               Reference_Name,
               Literal (0)));

         declare
            Table_Index : Declaration'Class :=
                            New_Constant_Declaration
                              (Item.Ada_Name & "_Table_Index",
                               Aquarius.Drys.Literal
                                 (Integer (Item.Reference_Index)));
         begin
            Table_Index.Set_Private_Spec;
            Top.Append (Table_Index);
         end;

         Top.Append (New_Separator);
      end Add_Reference_Type;

   begin
      Db.Iterate (Add_Reference_Type'Access);
   end Create_Reference_Types;

   -----------------------------
   -- Create_Search_Interface --
   -----------------------------

   procedure Create_Search_Interface
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      use Aquarius.Drys, Aquarius.Drys.Declarations;
      pragma Unreferenced (Db);
      Search_Interface : Aquarius.Drys.Interface_Type_Definition;
   begin
      Search_Interface.Set_Limited;
      Top.Append (New_Full_Type_Declaration
                  ("Search_Interface", Search_Interface));
      Top.Add_Separator;
      Top.Append
        (New_Abstract_Function
           ("Has_Element",
            New_Formal_Argument
              ("Item", Named_Subtype
                 ("Search_Interface")),
                     Named_Subtype ("Boolean")));
      Top.Add_Separator;

      Top.Append
        (New_Abstract_Procedure
           ("Next",
            New_Inout_Argument
              ("Item", Named_Subtype ("Search_Interface"))));
      Top.Add_Separator;

   end Create_Search_Interface;

   -----------------------
   -- Create_Table_Type --
   -----------------------

   procedure Create_Table_Type
     (Db  : Kit.Databases.Database_Type;
      Top : in out Aquarius.Drys.Declarations.Package_Type)
   is
      Table_Type_Definition : Aquarius.Drys.Enumeration_Type_Definition;

      procedure Add_Table_Type_Literal
        (Item : Kit.Tables.Table_Type'Class);

      ----------------------------
      -- Add_Table_Type_Literal --
      ----------------------------

      procedure Add_Table_Type_Literal
        (Item : Kit.Tables.Table_Type'Class)
      is
      begin
         Table_Type_Definition.New_Literal ("T_" & Item.Name);
      end Add_Table_Type_Literal;

   begin
      Table_Type_Definition.New_Literal ("T_None");
      Db.Iterate (Add_Table_Type_Literal'Access);
      Top.Append
        (Aquarius.Drys.Declarations.New_Full_Type_Declaration
           ("Table_Type", Table_Type_Definition));
      Top.Append (Aquarius.Drys.Declarations.New_Separator);

   end Create_Table_Type;

   -----------------------
   -- Generate_Database --
   -----------------------

   function Generate_Database
     (Db : in out Kit.Databases.Database_Type)
      return Aquarius.Drys.Projects.Project
   is
      Top_Package : Aquarius.Drys.Declarations.Package_Type :=
        Aquarius.Drys.Declarations.New_Package_Type (Db.Name);

      Project : Aquarius.Drys.Projects.Project;

      procedure Get_From_Cache
        (Table : Kit.Tables.Table_Type'Class);

      procedure Public_Interface
        (Table : Kit.Tables.Table_Type'Class);

      procedure Private_Interface
        (Table : Kit.Tables.Table_Type'Class);

      --------------------
      -- Get_From_Cache --
      --------------------

      procedure Get_From_Cache
        (Table : Kit.Tables.Table_Type'Class)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Get_From_Cache.Generate_Get_From_Cache
              (Db, Table, Top_Package));
      end Get_From_Cache;

      -----------------------
      -- Private_Interface --
      -----------------------

      procedure Private_Interface
        (Table : Kit.Tables.Table_Type'Class)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Private_Interface.Generate_Private_Interface
              (Db, Table, Top_Package));
      end Private_Interface;

      ----------------------
      -- Public_Interface --
      ----------------------

      procedure Public_Interface
        (Table : Kit.Tables.Table_Type'Class)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Public_Interface.Generate_Public_Interface
              (Db, Table, Top_Package));
      end Public_Interface;

      Db_Package  : constant Aquarius.Drys.Declarations.Package_Type :=
                      Database_Package.Generate_Database_Package (Db);

      Keys_Package : constant Aquarius.Drys.Declarations.Package_Type :=
                       Marlowe_Keys_Package.Generate_Package (Db);

   begin
      Top_Package.With_Package ("Marlowe", Private_With => True);
      Top_Package.With_Package ("Kit.Mutex", Private_With => True);

      --  Create_Handle_Function (Db, Top_Package);
      Create_Table_Type (Db, Top_Package);
      Create_Key_Type (Db, Top_Package);
      Create_Reference_Types (Db, Top_Package);
      Create_Record_Interface (Db, Top_Package);
      Create_Search_Interface (Db, Top_Package);
      Create_Locking_Interface (Db, Top_Package);
      Top_Package.Append
        (Aquarius.Drys.Declarations.New_Constant_Declaration
           ("Database_Magic_Number",
            Aquarius.Drys.Literal (1)));
      Project.Add_Package (Top_Package);
      Project.Add_Package (Db_Package);
      Project.Add_Package (Keys_Package);
      Db.Iterate (Get_From_Cache'Access);
      Db.Iterate (Public_Interface'Access);
      Db.Iterate (Private_Interface'Access);
      return Project;
   end Generate_Database;

end Kit.Generate;
