TRIGGER PROCEDURE FOR REPLICATION-WRITE OF r_fact_ventas.
define buffer b_vtas for subd_vtas.
DEFINE BUFFER b_rec FOR subd_vtas.
DEFINE BUFFER b_tipocomp FOR tipocomp.
define variable v_factor      as integer.
DEFINE VAR v_saldo LIKE subd_vtas.saldo_origen.
DEFINE BUFFER b_r_fact FOR r_fact_ventas.
DEFINE BUFFER b_tip FOR tipocomp.
DEF VAR v_vinculantes AS CHARACTER.

/* Reemplazar */
 v_vinculantes = "300,400,28,3,76,78,81,83,26,158,69,72".

if available r_fact_ventas Then
  do:
      FIND FIRST b_rec WHERE 
           b_rec.id_punto_venta = r_fact_ventas.id_punto_venta_ventas AND
           b_rec.nromov = r_fact_ventas.nromov_ventas NO-ERROR.
      IF AVAILABLE b_rec THEN
      DO:
          IF CAN-DO(v_vinculantes, string(b_rec.id_tipocomp)) THEN
          DO:
               FIND FIRST b_vtas OF r_fact_ventas NO-ERROR.
               IF AVAILABLE b_vtas THEN
               DO:

                 /* Actualizo saldo de los comprobantes vinculados */
                    ASSIGN b_vtas.saldo_origen =  r_fact_ventas.saldo_actual_origen
                           b_vtas.saldo_base =  r_fact_ventas.saldo_actual_base
                           b_vtas.saldo_local = r_fact_ventas.saldo_actual_local.

               END.


               /* Actualizo saldo de vinculantes ************************/
               FIND FIRST tipocomp OF b_rec NO-LOCK.
               if tipocomp.signo THEN v_factor = 1.
                                 ELSE v_factor = -1.
               v_saldo = b_rec.importe_origen * v_factor.

                /* Chequeo aplicacion con comprobantes vinculados */
                FOR EACH b_r_fact WHERE
                       b_r_fact.id_punto_venta_ventas = b_rec.id_punto_venta AND
                       b_r_fact.nromov_ventas = b_rec.nromov NO-LOCK,
                       FIRST b_vtas OF b_r_fact NO-LOCK,
                       FIRST b_tipocomp OF b_vtas NO-LOCK:
                       IF b_tipocomp.signo THEN
                            v_saldo = v_saldo + b_r_fact.importe_origen.
                         ELSE
                            v_saldo = v_saldo - b_r_fact.importe_origen.
                 END.
                 ASSIGN b_rec.saldo_origen = abs(truncate(v_saldo,2)).
                 ASSIGN b_rec.saldo_base = b_rec.saldo_origen * b_rec.cotizacion_base
                        b_rec.saldo_local = b_rec.saldo_origen * (IF b_rec.id_moneda_origen <> 1 THEN b_rec.cotizacion ELSE 1).
               
               /*********************************************************/

          END.
      END.
  end.
