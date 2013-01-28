with Ada.Characters.Handling;

with Glib;

with Gtk.Cell_Renderer_Text;
with Gtk.List_Store;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

with Kit.Db.Kit_Key;
with Kit.Db.Kit_Record;
with Kit.Db.Tables;

package body Kit.UI.Gtk_UI.Table_Book is

   -----------------------
   -- New_Table_Display --
   -----------------------

   function New_Table_Display
     (Table_Name : String)
      return Gtk.Widget.Gtk_Widget
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
            Index := Index + 1;
         end loop;

      end Initialise_View_From_Record;

   begin

      Gtk.Tree_View.Gtk_New (View);

      Initialise_View_From_Record;

      declare
         Kit_Record : constant Kit.Db.Kit_Record.Kit_Record_Type :=
                        Kit.Db.Kit_Record.Get_By_Name (Table_Name);
         Kit_Key    : constant Kit.Db.Kit_Key.Kit_Key_Type :=
                        Kit.Db.Kit_Key.Get_By_Kit_Record
                          (Kit_Record.Reference);
      begin
         if Kit_Key.Has_Element then
            Table.Iterate (Kit_Key.Name, Add_Record'Access);
         end if;
      end;

      return Gtk.Widget.Gtk_Widget (View);

   end New_Table_Display;

end Kit.UI.Gtk_UI.Table_Book;
