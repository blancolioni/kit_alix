with Ada.Characters.Handling;

with Glib;

with Gtk.Cell_Renderer_Text;
with Gtk.Handlers;
with Gtk.List_Store;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;

with Kit.Db.Kit_Key;
with Kit.Db.Kit_Record;
with Kit.Db.Kit_Record_Base;
with Kit.Db.Tables;

package body Kit.UI.Gtk_UI.Table_Book is

   package Column_Header_Callback is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record,
        Table_Display);

   procedure On_Column_Click
     (Tree_View : access
        Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Display   : Table_Display);

   procedure Show_Table
     (Display : Table_Display);

   -----------------------
   -- New_Table_Display --
   -----------------------

   function New_Table_Display
     (Table_Name : String)
      return Table_Display
   is
      Table : constant Kit.Db.Tables.Database_Table :=
                Kit.Db.Tables.Get_Table (Table_Name);
      View  : Gtk.Tree_View.Gtk_Tree_View;
      Store : Gtk.List_Store.Gtk_List_Store;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);

      procedure Add_Record (Rec : Kit.Db.Tables.Database_Record'Class);

      procedure Initialise_View_From_Record;

      function Format_Heading (Database_Name : String) return String;

      Result : constant Table_Display := new Root_Table_Display;


      ----------------
      -- Add_Record --
      ----------------

      procedure Add_Record (Rec : Kit.Db.Tables.Database_Record'Class) is
         --  pragma Unreferenced (Rec);
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         Store.Append (Iter);
         for I in 1 .. Table.Field_Count loop
            Store.Set (Iter   => Iter,
                       Column => Glib.Gint (I - 1),
                       Value  => Rec.Get (I));
         end loop;
      end Add_Record;

      --------------------
      -- Format_Heading --
      --------------------

      function Format_Heading (Database_Name : String) return String is
         use Ada.Characters.Handling;
         Result : String := Database_Name;
         First  : Boolean := True;
      begin
         for I in Result'Range loop
            if First then
               Result (I) := To_Upper (Result (I));
               First := False;
            elsif Result (I) = ' ' or else Result (I) = '_' then
               Result (I) := ' ';
               First := True;
            end if;
         end loop;
         return Result;
      end Format_Heading;

      ---------------------------------
      -- Initialise_View_From_Record --
      ---------------------------------

      procedure Initialise_View_From_Record is
         use type Glib.Gint;
         Column_Types : Glib.GType_Array (1 .. Glib.Guint (Table.Field_Count));
         Index : Glib.Gint := 0;
      begin
         Column_Types := (others => Glib.GType_String);
         Gtk.List_Store.Gtk_New
           (Store, Column_Types);

         View.Set_Model (Gtk.Tree_Model.Gtk_Tree_Model (Store));

         for I in 1 .. Table.Field_Count loop
            Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
            Gtk.Tree_View_Column.Gtk_New (Text_Column);
            Num := View.Append_Column (Text_Column);
            Text_Column.Pack_Start (Text_Render, True);
            Text_Column.Set_Sizing
              (Gtk.Tree_View_Column.Tree_View_Column_Autosize);

            Text_Column.Set_Title
              (Format_Heading (Table.Field_Name (I)));
            Text_Column.Add_Attribute (Text_Render, "text", Index);
            Column_Header_Callback.Connect
              (Text_Column,
               Gtk.Tree_View_Column.Signal_Clicked,
               Column_Header_Callback.To_Marshaller
                 (On_Column_Click'Access),
               Result);

            Index := Index + 1;
         end loop;

      end Initialise_View_From_Record;

   begin

      Result.Table_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Table_Name);

      Gtk.Tree_View.Gtk_New (View);
      Result.Widget := View;

      Initialise_View_From_Record;

      declare
         Kit_Record : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                        Kit.Db.Kit_Record.Get_By_Name (Table_Name);
         Kit_Key    : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                        Kit.Db.Kit_Key.Get_By_Kit_Record
                          (Kit_Record.Reference);
      begin
         if Kit_Key.Has_Element then
            Result.Key_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String (Kit_Key.Name);
            Table.Iterate (Kit_Key.Name, Add_Record'Access);
         end if;
      end;

      View.Set_Headers_Visible (True);
      View.Set_Headers_Clickable (True);

      return Result;

   end New_Table_Display;

   ---------------------
   -- On_Column_Click --
   ---------------------

   procedure On_Column_Click
     (Tree_View : access
        Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Display   : Table_Display)
   is

      function Deformat_Heading (Heading : String) return String;

      ----------------------
      -- Deformat_Heading --
      ----------------------

      function Deformat_Heading (Heading : String) return String is
         Result : String := Heading;
      begin
         for I in Result'Range loop
            if Result (I) = ' ' then
               Result (I) := '_';
            else
               Result (I) := Ada.Characters.Handling.To_Lower (Result (I));
            end if;
         end loop;
         return Result;
      end Deformat_Heading;

      Nice_Title : constant String := Tree_View.Get_Title;
      Standard_Title : constant String := Deformat_Heading (Nice_Title);

      Kit_Record : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                     Kit.Db.Kit_Record.Get_By_Name (Table_Name (Display.all));
      Kit_Key    : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                     Kit.Db.Kit_Key.Get_By_Record_Key
                       (Kit_Record.Reference, Standard_Title);
      Found      : Boolean := False;
   begin
      if Kit_Key.Has_Element then
         Display.Key_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Kit_Key.Name);
         Found := True;
      else
         for Base of
           Kit.Db.Kit_Record_Base.Select_By_Derived
             (Kit_Record.Reference)
         loop
            declare
               Base_Key : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                            Kit.Db.Kit_Key.Get_By_Record_Key
                              (Base.Base, Standard_Title);
            begin
               if Base_Key.Has_Element then
                  Display.Key_Name :=
                    Ada.Strings.Unbounded.To_Unbounded_String (Base_Key.Name);
                  Found := True;
                  --  bug in Kit requires that we continue this loop
               end if;
            end;
         end loop;

         if Found then
            Show_Table (Display);
         end if;
      end if;

   end On_Column_Click;

   ----------------
   -- Show_Table --
   ----------------

   procedure Show_Table
     (Display : Table_Display)
   is

      Store : constant Gtk.List_Store.Gtk_List_Store :=
                Gtk.List_Store.Gtk_List_Store
                  (Display.Widget.Get_Model);

      Table : constant Kit.Db.Tables.Database_Table :=
                Kit.Db.Tables.Get_Table (Table_Name (Display.all));

      procedure Add_Record (Rec : Kit.Db.Tables.Database_Record'Class);

      ----------------
      -- Add_Record --
      ----------------

      procedure Add_Record (Rec : Kit.Db.Tables.Database_Record'Class) is
         --  pragma Unreferenced (Rec);
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         Store.Append (Iter);
         for I in 1 .. Table.Field_Count loop
            Store.Set (Iter   => Iter,
                       Column => Glib.Gint (I - 1),
                       Value  => Rec.Get (I));
         end loop;
      end Add_Record;

   begin
      Store.Clear;

      Table.Iterate
        (Ada.Strings.Unbounded.To_String (Display.Key_Name),
         Add_Record'Access);

   end Show_Table;

   ----------------
   -- Table_Name --
   ----------------

   function Table_Name (Display : Root_Table_Display'Class)
                        return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Display.Table_Name);
   end Table_Name;

   ------------
   -- Widget --
   ------------

   function Widget (Display : Root_Table_Display'Class)
                    return Gtk.Widget.Gtk_Widget
   is
   begin
      return Gtk.Widget.Gtk_Widget (Display.Widget);
   end Widget;

end Kit.UI.Gtk_UI.Table_Book;
