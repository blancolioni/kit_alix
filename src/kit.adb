with Ada.Unchecked_Deallocation;

package body Kit is

   -----------
   -- Close --
   -----------

   procedure Close (Rec : in out Database_Record) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Root_Database_Record'Class,
                                        Database_Record);
   begin
      Free (Rec);
   end Close;

end Kit;
