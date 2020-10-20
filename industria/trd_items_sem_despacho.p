TRIGGER PROCEDURE FOR REPLICATION-DELETE OF items_semana_despacho.
define buffer b_calibres for calibres_semana_despacho.

if available items_semana_despacho Then
do:
     FOR EACH b_calibres OF items_semana_despacho:
         DELETE b_calibres.
     END.
end.
