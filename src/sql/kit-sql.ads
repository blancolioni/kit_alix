private with Ada.Finalization;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Unbounded;
private with Ada.Text_IO;

package Kit.SQL is

   type Column_Count is new Natural;
   subtype Column_Index is Column_Count range 1 .. Column_Count'Last;

   type Row_Count is new Natural;

   subtype Row_Index is Row_Count range 1 .. Row_Count'Last;

   type File_Reference is private;

   type SQL_Element is abstract tagged private;

private

   type File_Reference is new Natural;

   type SQL_Element is abstract new Ada.Finalization.Controlled with
      record
         File   : File_Reference;
         Line   : Ada.Text_IO.Count;
         Column : Ada.Text_IO.Count;
      end record;

   overriding procedure Initialize
     (Element : in out SQL_Element);

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Same (Left, Right : String) return Boolean
   is (Ada.Strings.Fixed.Equal_Case_Insensitive (Left, Right));

   function Same (Left  : Ada.Strings.Unbounded.Unbounded_String;
                  Right : String)
                  return Boolean
   is (Same (-Left, Right));

end Kit.SQL;
