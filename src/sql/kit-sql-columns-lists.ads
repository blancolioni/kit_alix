with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Kit.SQL.Columns.Lists is
  new Ada.Containers.Indefinite_Doubly_Linked_Lists
    (Column_Element'Class);
