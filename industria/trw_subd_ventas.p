TRIGGER PROCEDURE FOR REPLICATION-WRITE OF subd_vtas.
define buffer b_auxsub for aux_subd_ventas.
DEFINE BUFFER b_mov FOR movimientos_ventas.

/********** Variables para cotizacion *******/
    define var v_pesos-v as decimal decimals 10.
    define var v_pesos-c as decimal decimals 10.
    define var v_dolar-v as decimal decimals 10.
    define var v_dolar-c as decimal decimals 10.
/********************************************/


if available subd_vtas Then
  do:
      FIND FIRST b_auxsub OF subd_vtas NO-ERROR.
      IF NOT AVAILABLE b_auxsub THEN
      DO:
          CREATE b_auxsub.
          BUFFER-COPY subd_vtas TO b_auxsub.
      END.

      /* Observaciones para comprobantes en dolar Mercado Interno */
      IF subd_vtas.mercado = YES AND subd_vtas.id_moneda_origen = 2 and
          subd_vtas.observacion = "" AND subd_vtas.id_tipocomp <> 300 THEN
      DO:
          RUN p_carga_obs_dolar.p (INPUT ROWID(subd_vtas)).
      END.
      
      IF subd_vtas.mercado = NO THEN    /* Mercado Externo */
      DO:
          /* Completo datos de cotizaciones */
          run p_val_cotizacion.p (input subd_vtas.fecha,
                                  input subd_vtas.id_moneda_origen,
                                  output v_pesos-v,
                                  output v_pesos-c,
                                  output v_dolar-v,
                                  output v_dolar-c).

          IF subd_vtas.id_tipocomp <> 400 THEN
          DO:
              IF subd_vtas.cotizacion = 0 THEN
                 ASSIGN subd_vtas.cotizacion = v_pesos-c.
              
              ASSIGN 
               subd_vtas.id_moneda_local = 1
               subd_vtas.id_moneda_base = 2
               subd_vtas.importe_local = subd_vtas.importe_origen * cotizacion.

              IF subd_vtas.cotizacion_base = 0 THEN
               ASSIGN subd_vtas.importe_base  = subd_vtas.importe_origen * v_dolar-c
                      subd_vtas.cotizacion_base = v_dolar-c.

          END.

          /*********** Recibos *****************************************/
          IF subd_vtas.id_tipocomp = 400 THEN
          DO:
             /* Carga movimientos para recibo */
              RUN p_cargar_mov_ventas.p (INPUT ROWID(subd_vtas)).
              IF subd_vtas.cotizacion = 0 THEN
                    DO:
                     ASSIGN 
                      subd_vtas.id_moneda_local = 1
                      subd_vtas.id_moneda_base = 2 
                      subd_vtas.cotizacion = v_pesos-c
                      subd_vtas.cotizacion_base = v_dolar-c
                      subd_vtas.importe_local = subd_vtas.importe_origen * v_pesos-c
                      subd_vtas.importe_base  = subd_vtas.importe_origen * v_dolar-c.
                    END.
                ELSE
                    DO:
                      IF subd_vtas.id_moneda_origen = 2 THEN
                          ASSIGN subd_vtas.importe_base = subd_vtas.importe_origen.

                    END.
              IF subd_vtas.importe_origen > 0 THEN
              ASSIGN subd_vtas.cotizacion = subd_vtas.importe_local / subd_vtas.importe_origen.
          END.
      END.  /* Mercado Externo */
     ELSE
     DO:
         /* Comprobantes recibidos */
         FIND FIRST tipocomp OF subd_vtas NO-LOCK.
         IF tipocomp.recibido = YES THEN
         DO:
            FIND FIRST b_mov OF subd_vtas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b_mov THEN
            DO:
             /* Carga movimientos para comprobantes recibidos */
               RUN p_cargar_mov_ventas.p (INPUT ROWID(subd_vtas)).
            END.
         END.

         /* Carga de cotizaciones de Mercado Interno */
         if subd_vtas.id_moneda_origen <> 1 Then
                do:
                  run p_val_cotizacion.p (input subd_vtas.fecha,
                                          input subd_vtas.id_moneda_origen,
                                          output v_pesos-v,
                                          output v_pesos-c,
                                          output v_dolar-v,
                                          output v_dolar-c).
                end.                            
            Else
                do:
                    run p_val_cotizacion.p (input subd_vtas.fecha,
                                           input 2, /* Dolar */
                                           output v_pesos-v,
                                           output v_pesos-c,
                                           output v_dolar-v,
                                           output v_dolar-c).
                       v_dolar-c = 1 / v_pesos-c.
                       ASSIGN subd_vtas.importe_local = subd_vtas.importe_origen.
                end.

         ASSIGN 
              subd_vtas.id_moneda_local = 1
              subd_vtas.id_moneda_base = 2 
              subd_vtas.cotizacion_base = v_dolar-c
              subd_vtas.importe_base  = subd_vtas.importe_origen * v_dolar-c.
     END.
  end.
