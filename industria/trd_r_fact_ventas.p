TRIGGER PROCEDURE FOR REPLICATION-DELETE OF r_fact_ventas.
define buffer b_vtas for subd_vtas.
DEFINE BUFFER b_rec FOR subd_vtas.
DEFINE BUFFER b_tip FOR tipocomp.
DEF VAR v_vinculantes AS CHARACTER.

/* Reemplazar */
 v_vinculantes = "300,400,28,3,76,78,81,83,26".

if available r_fact_ventas Then
  do:
     FIND FIRST b_rec WHERE 
         b_rec.id_punto_venta = r_fact_ventas.id_punto_venta_ventas AND
         b_rec.nromov = r_fact_ventas.nromov_ventas NO-LOCK NO-ERROR.
     IF AVAILABLE b_rec THEN
     DO:
         IF CAN-DO(v_vinculantes, string(b_rec.id_tipocomp)) THEN
            DO:
               FIND FIRST b_vtas OF r_fact_ventas NO-ERROR.
               IF AVAILABLE b_vtas THEN
               DO:
                   ASSIGN b_vtas.saldo_origen = r_fact_ventas.saldo_anterior_origen
                          b_vtas.saldo_base = r_fact_ventas.saldo_anterior_base
                          b_vtas.saldo_local = r_fact_ventas.saldo_anterior_local.

               END.

               /******* Seteo el saldo del comprobante vinculante *****/
               FIND FIRST b_rec WHERE 
               b_rec.id_punto_venta = r_fact_ventas.id_punto_venta_ventas AND
               b_rec.nromov = r_fact_ventas.nromov_ventas NO-ERROR.
               ASSIGN b_rec.saldo_origen = b_rec.saldo_origen + r_fact_ventas.importe_origen.
                      b_rec.saldo_base = b_rec.saldo_origen * b_rec.cotizacion_base.
                      b_rec.saldo_local = b_rec.saldo_origen * (IF b_rec.id_moneda_origen <> 1 THEN b_rec.cotizacion ELSE 1).
               /*************************************************************************************************************/
            END.
     END.
  end.
