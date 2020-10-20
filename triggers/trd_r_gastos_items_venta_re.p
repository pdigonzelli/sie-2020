TRIGGER PROCEDURE FOR REPLICATION-DELETE OF r_gastos_items_venta_re.
define buffer b_r_gastos for r_gastos_items_venta_re.
define buffer b_items for items_venta_re.
DEFINE VAR v_total AS DECIMAL.

if available r_gastos_items_venta_re Then
  do:
       FIND FIRST b_items OF r_gastos_items_venta_re NO-ERROR.
       IF AVAILABLE b_items THEN
       DO:
        v_total = 0.
        for each b_r_gastos of b_items:
          v_total = v_total + b_r_gastos.importe.
        end.
        ASSIGN b_items.gastos = v_total.
       END.
  end.
