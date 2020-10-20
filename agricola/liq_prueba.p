/******************************************************************/
/* SECTOR AGRICOLA                                                */ 
/* Genera archivo de liquidación de TARJA para incorporacion      */
/* en el Abacus                                                   */ 
/* Se toma como centro de costo la finca donde trabajo 15/01/2001 */
/***********************************************************************/
/* El 16/07/02 se agrega la cant de dias trabajados - cod ABACUS (941) */
/***********************************************************************/   

DEFINE VAR v_empresa LIKE liq_control_tareas.id_empresa.
DEFINE VAR v_sector LIKE liq_control_tareas.id_sector.
DEFINE VAR v_fecha_desde AS DATE.
DEFINE VAR v_fecha_hasta AS DATE.
DEFINE VAR v_carpeta AS CHARACTER.
DEFINE VAR  v-tipo AS CHARACTER.
  
DEF VAR v_centro AS INTEGER.
DEF VAR v_finca AS INTEGER.
DEF VAR v_cuenta AS INTEGER.

DEF VAR v_cuenta_jornal AS INTEGER.
DEF VAR v_cuenta_horas AS INTEGER.
DEF VAR v_cuenta_otros AS INTEGER.
DEF VAR v_jornal AS DECIMAL.

DEF VAR v_codigo AS INTEGER.
DEF VAR v_codigo-1 AS INTEGER.
DEF VAR v_codigo-2 AS INTEGER.

DEF VAR v_entero AS DECIMAL.
DEF VAR v_decimal AS DECIMAL.
DEFINE VAR v_cargo LIKE liq_legajos.id_cargo.
DEFINE VAR v_horas AS DECIMAL.
DEF VAR v_cantidad AS CHARACTER.
DEF VAR v_legajo LIKE liq_items_control_tareas.legajo.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_desde AS CHARACTER.
DEF VAR v_hasta AS CHARACTER.

DEFINE TEMP-TABLE t-personal
   FIELD legajo LIKE liq_items_control_tareas.legajo
   FIELD id_concepto LIKE r_tareas_unidades.id_concepto
   FIELD cantidad LIKE liq_items_control_tareas.cantidad FORMAT ">>>>9.99"
   FIELD id_centro_abacus LIKE liq_legajos.id_centro_costo
   FIELD id_parametro AS INTEGER
   FIELD fecha_desde AS DATE
   FIELD fecha_hasta AS DATE
   FIELD id_tipo_motivo AS INTEGER FORMAT "9"
   FIELD id_motivo  AS INTEGER  FORMAT "9"
   FIELD id_asiento AS INTEGER FORMAT ">"
   FIELD id_estructura1 AS INTEGER.

    

FOR EACH t-personal:
  DELETE t-personal. 
END.
   
v_empresa = 101.
v_sector = 5.
v_fecha_desde = DATE("16/07/16").
v_fecha_hasta = DATE("25/07/16").
v-tipo = "j".
v_legajo = 57217.

FOR EACH liq_control_tareas NO-LOCK WHERE id_empresa = v_empresa AND 
      liq_control_tareas.id_sector = v_sector AND
      liq_control_tareas.fecha >= v_fecha_desde AND liq_control_tareas.fecha <= v_fecha_hasta 
      ,EACH liq_items_control_tareas OF liq_control_tareas WHERE
        liq_items_control_tareas.id_tarea <> 0 AND  liq_items_control_tareas.legajo = v_legajo  NO-LOCK,
       FIRST liq_legajos WHERE liq_legajos.id_empresa_liq =
        liq_items_control_tareas.id_empresa AND liq_legajos.legajo = liq_items_control_tareas.legajo AND
       (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
       BY liq_items_control_tareas.id_empresa BY legajo BY liq_items_control_tareas.fecha:
      
        v_cuenta = v_cuenta + 1.

        FIND FIRST tareas WHERE tareas.id_tarea = liq_items_control_tareas.id_tarea NO-LOCK NO-ERROR.
        IF AVAILABLE tareas THEN
        DO:
          v_cargo = liq_legajos.id_cargo.        
          v_finca = 0.


          IF liq_legajos.toma_ccosto_legajo = NO THEN
          DO:
    
              FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_control_tareas.id_proveedor AND
              liq_ccostos.id_origen = liq_control_tareas.id_origen AND
              liq_ccostos.id_centro_costo > 100000000  NO-LOCK NO-ERROR.
              IF AVAILABLE liq_ccostos THEN
                v_finca = liq_ccostos.id_centro_costo_liq.
               ELSE
               DO:
                   FIND FIRST liq_centros_costos WHERE 
                       liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
                       liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
                   IF AVAILABLE liq_centros_costos THEN
                   DO:
                       FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
                           liq_ccostos.id_origen = liq_centros_costos.id_origen AND
                           liq_ccostos.id_centro_costo > 100000000 NO-LOCK NO-ERROR.
                       IF AVAILABLE liq_ccostos THEN
                           v_finca = liq_ccostos.id_centro_costo_liq.
                   END.
               END.
          END.
         ELSE v_finca = liq_legajos.id_ccostos_liq.


        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.   
    
       {liq-calcula-rhpro-agricola.i} 
 


      MESSAGE v_cuenta liq_control_tareas.fecha v_finca v_codigo v_codigo-1 v_codigo-2 VIEW-AS ALERT-BOX.


      IF v_codigo <> 9999 AND v_codigo <> 0 THEN
       DO:
       FIND FIRST t-personal WHERE t-personal.legajo = liq_items_control_tareas.legajo AND
       t-personal.id_concepto = v_codigo AND
       t-personal.id_centro_abacus = v_finca NO-LOCK NO-ERROR.
       IF NOT AVAILABLE t-personal THEN
          DO:
             CREATE t-personal.
             ASSIGN t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.
          END. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_jornal.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
       END.
    

  
     IF v_codigo-1 <> 9999 AND v_codigo-1 <> 0 THEN
     DO: 
       FIND FIRST t-personal WHERE t-personal.legajo = liq_items_control_tareas.legajo AND
       t-personal.id_concepto = v_codigo-1 AND
       t-personal.id_centro_abacus = v_finca NO-LOCK NO-ERROR.
       IF NOT AVAILABLE t-personal THEN
          DO:
             CREATE t-personal.
             ASSIGN t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.

          END. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_horas.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
    END.
     
    IF v_codigo-2 <> 9999 AND v_codigo-2 <> 0 THEN
     DO:
       FIND FIRST t-personal WHERE t-personal.legajo = liq_items_control_tareas.legajo AND
       t-personal.id_concepto = v_codigo-2 AND
       t-personal.id_centro_abacus = v_finca NO-LOCK NO-ERROR.
       IF NOT AVAILABLE t-personal THEN
          DO:
             CREATE t-personal.
             ASSIGN t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.
          END. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cantidad.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
     END. 

  END. 
END.   
      

/* Se excluyen del contado de dias las tareas con sector 0 */

FOR EACH liq_control_tareas NO-LOCK WHERE id_empresa = v_empresa AND 
      liq_control_tareas.id_sector = v_sector AND
      liq_control_tareas.fecha >= v_fecha_desde AND liq_control_tareas.fecha <= v_fecha_hasta 
      ,EACH liq_items_control_tareas OF liq_control_tareas WHERE
       liq_items_control_tareas.id_tarea <> 0 AND liq_items_control_tareas.legajo = v_legajo NO-LOCK,
       FIRST liq_legajos WHERE liq_legajos.id_empresa_liq =
           liq_items_control_tareas.id_empresa AND liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK,
       FIRST tareas WHERE tareas.id_tarea = liq_items_control_tareas.id_tarea AND 
       (tareas.id_sector <> 0) NO-LOCK 
       BREAK BY liq_items_control_tareas.id_empresa BY legajo BY liq_items_control_tareas.fecha:
      
       
                v_cargo = liq_legajos.id_cargo.        
                v_finca = 0.

                IF liq_legajos.toma_ccosto_legajo = NO THEN
                DO:
                        FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_control_tareas.id_proveedor AND
                        liq_ccostos.id_origen = liq_control_tareas.id_origen AND
                        liq_ccostos.id_centro_costo > 100000000  NO-LOCK NO-ERROR.
                        IF AVAILABLE liq_ccostos THEN
                          v_finca = liq_ccostos.id_centro_costo_liq.
                         ELSE
                         DO:
                             FIND FIRST liq_centros_costos WHERE 
                                 liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
                                 liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
                             IF AVAILABLE liq_centros_costos THEN
                             DO:
                                 FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
                                     liq_ccostos.id_origen = liq_centros_costos.id_origen AND
                                     liq_ccostos.id_centro_costo > 100000000 NO-LOCK NO-ERROR.
                                 IF AVAILABLE liq_ccostos THEN
                                     v_finca = liq_ccostos.id_centro_costo_liq.
                             END.
                         END.
                END.
                ELSE v_finca = liq_legajos.id_ccostos_liq.

              /************************************************************/ 
              /* Llevo a horas todas las tareas realizadas ***************/
            
              IF liq_items_control_tareas.cant_jornal <> 0 THEN
                 v_horas = v_horas + (liq_items_control_tareas.cant_jornal * 8).

              IF liq_items_control_tareas.cant_horas <> 0 THEN
                 v_horas = v_horas + liq_items_control_tareas.cant_horas.
                 
              IF liq_items_control_tareas.cantidad <> 0 THEN
                 DO:
                    CASE liq_items_control_tareas.id_unidad_liquidacion:
                        WHEN 1 OR WHEN 5 OR WHEN 10 OR WHEN 11 OR WHEN 14 THEN
                           v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
                        WHEN 2 OR WHEN 3 OR WHEN 7 OR WHEN 15 OR WHEN 17
                               OR WHEN 20 OR WHEN 21 OR WHEN 22 OR WHEN 23
                               OR WHEN 24 THEN
                           v_horas = v_horas + liq_items_control_tareas.cantidad.
                        OTHERWISE 
                           v_horas = v_horas + 8. 
                    END CASE.   
                 END.   
               /************************************************************/   

               IF LAST-OF(liq_items_control_tareas.fecha) THEN
                  DO:
                      IF v_horas > 8 THEN v_horas = 8.
                      v_codigo = 941.
                      FIND FIRST t-personal WHERE t-personal.legajo = liq_items_control_tareas.legajo AND
                      t-personal.id_concepto = v_codigo AND
                      t-personal.id_centro_abacus = v_finca NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE t-personal THEN
                         DO:
                            CREATE t-personal.
                            ASSIGN t-personal.legajo = liq_items_control_tareas.legajo
                                   t-personal.id_concepto = v_codigo 
                                   t-personal.id_centro_abacus = v_finca
                                   t-personal.id_tipo_motivo = 2
                                   t-personal.id_motivo = 1
                                   t-personal.id_asiento = 2
                                   t-personal.id_estructura1 = v_finca
                                   t-personal.fecha_desde = liq_items_control_tareas.fecha
                                   t-personal.fecha_hasta = liq_items_control_tareas.fecha.
                         END.
                      t-personal.cantidad = t-personal.cantidad + (v_horas / 8).
                      v_horas = 0.
                      IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
                          t-personal.fecha_hasta = liq_items_control_tareas.fecha.
                   END.
END.      

v_carpeta = "z:\temp\".
ASSIGN SESSION:NUMERIC-FORMAT = "EUROPEAN"
       SESSION:DATE-FORMAT = "dmy".

IF v-tipo = "" THEN
    OUTPUT to value (v_carpeta + "RHPRO"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(DAY(v_fecha_desde),"99") + "-" +
                     string(DAY(v_fecha_hasta),"99") + "-" + string(MONTH(v_fecha_hasta),"99") + "-" +
                     string(YEAR(v_fecha_hasta),"9999")
                      + ".csv").
 ELSE
     OUTPUT to value (v_carpeta + v-tipo + "-RHPRO"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(DAY(v_fecha_desde),"99") + "-" +
                      string(DAY(v_fecha_hasta),"99") + "-" + string(MONTH(v_fecha_hasta),"99") + "-" +
                      string(YEAR(v_fecha_hasta),"9999")
                       + ".csv").
PUT "Legajo;Concepto;Parametros;Monto;Fecha Desde;Fecha Hasta;Tipo de motivo;Motivo;Modelo Asiento;Estructura1;".
PUT SKIP.

  FOR EACH t-personal WHERE t-personal.id_concepto <> 912 BY t-personal.legajo BY t-personal.id_concepto :
    FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = t-personal.id_concepto NO-ERROR.
    IF AVAILABLE liq_conceptos THEN ASSIGN t-personal.id_parametro = liq_conceptos.id_parametro.

    v_cantidad = TRIM(STRING(t-personal.cantidad, ">>>>9.99")).

    IF v_empresa <> 101 THEN
                         DO:
                          v_inicio = INTEGER(SUBSTRING(STRING(v_empresa),1,1)).
                          v_legajo = (v_inicio * 10000000) + t-personal.legajo.
                         END.
                      ELSE v_legajo = t-personal.legajo.

    PUT v_legajo FORMAT ">>>>>>>>".
    PUT ";".
    PUT t-personal.id_concepto FORMAT "99999".
    PUT ";".
    PUT t-personal.id_parametro FORMAT ">>>9".
    PUT ";".
    PUT v_cantidad.
    PUT ";".
    PUT t-personal.fecha_desde.
    PUT ";".
    PUT t-personal.fecha_hasta.
    PUT ";".
    PUT t-personal.id_tipo_motivo FORMAT "9".
    PUT ";".
    PUT t-personal.id_motivo FORMAT "9".
    PUT ";".
    PUT t-personal.id_asiento FORMAT "9".
    PUT ";".
    PUT t-personal.id_estructura1 FORMAT ">>>>>>>>>>".
    PUT SKIP. 
        
   END.
OUTPUT close.

ASSIGN SESSION:NUMERIC-FORMAT = "AMERICAN".

MESSAGE "Archivo de TARJAS generado" VIEW-AS ALERT-BOX.
   
   
