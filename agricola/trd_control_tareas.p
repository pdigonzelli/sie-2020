TRIGGER PROCEDURE FOR REPLICATION-DELETE OF control_tareas.
define buffer b_items_tareas for items_control_tareas.

if available control_tareas Then
  do:
     for each b_items_tareas of control_tareas:
        delete b_items_tareas.   
     end.
  end.
