TRIGGER PROCEDURE FOR REPLICATION-WRITE OF r_gastos_items_venta_re.
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

       IF r_gastos_items_venta_re.importe <> 0 THEN
        DO:
           FOR EACH b_r_gastos WHERE b_r_gastos.id_punto_venta = 
             r_gastos_items_venta_re.id_punto_venta AND 
             b_r_gastos.nromov = r_gastos_items_venta_re.nromov AND
             b_r_gastos.id_gasto = r_gastos_items_venta_re.id_gasto AND
             b_r_gastos.importe = 0:
                b_r_gastos.importe = r_gastos_items_venta_re.importe.
           END.
        END.
  end.
