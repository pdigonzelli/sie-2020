TRIGGER PROCEDURE FOR REPLICATION-DELETE OF semana_despacho.
define buffer b_items for items_semana_despacho.

if available semana_despacho Then
  do:
     FOR EACH b_items OF semana_despacho:
         DELETE b_items.
     END.
  end.
