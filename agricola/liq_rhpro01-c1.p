/******************************************************************/
/* SECTOR AGRICOLA                                                */ 
/* Genera archivo de liquidación de TARJA para incorporacion      */
/* en el RHPRO                                                   */ 
/* Se toma como centro de costo la finca donde trabajo 15/01/2001 */
/*************************************************************************/
/* El 16/07/02 se agrega la cant de dias trabajados - cod ABACUS (941)   */
/*************************************************************************/  
/* El 06/03/17 se agrega para el sector cosecha un CC unico y se mantiene*/
/* para los otros sectores la finca donde trabajo                        */
/*************************************************************************/  
 

define input parameter v_empresa like liq_tarjas.id_empresa.
define input parameter v_sector like liq_tarjas.id_sector.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
DEFINE INPUT PARAMETER v_carpeta AS CHARACTER.
DEFINE INPUT PARAMETER v-tipo AS CHARACTER.
  
def var v_centro as integer.
def var v_finca as integer.
def var v_cuenta as integer.


def var v_codigo as integer.
def var v_codigo-1 as integer.
def var v_codigo-2 as integer.

def var v_entero as decimal.
def var v_decimal as decimal.
define var v_cargo like liq_legajos.id_cargo.
define var v_horas as decimal.
DEF VAR v_cantidad AS CHARACTER.
DEF VAR v_legajo LIKE liq_items_tarjas.legajo.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_desde AS CHARACTER.
DEF VAR v_hasta AS CHARACTER.

define temp-table t-personal
   field legajo like liq_items_tarjas.legajo
   field id_concepto like liq_items_tarjas.id_codigo_abacus
   field cantidad like liq_items_tarjas.cantidad FORMAT ">>>>9.99"
   field id_centro_abacus like liq_legajos.id_centro_costo
   FIELD id_parametro AS INTEGER
   FIELD fecha_desde AS DATE
   FIELD fecha_hasta AS DATE
   FIELD id_tipo_motivo AS INTEGER FORMAT "9"
   FIELD id_motivo  AS INTEGER  FORMAT "9"
   FIELD id_asiento AS INTEGER FORMAT ">"
   FIELD id_estructura1 AS INTEGER.

    

for each t-personal:
  delete t-personal. 
end.
   
/************************************************************/

 for each liq_items_tarjas where 
      liq_items_tarjas.id_empresa = v_empresa AND 
      liq_items_tarjas.fecha >= v_fecha_desde and 
      liq_items_tarjas.fecha <= v_fecha_hasta and
      (IF v_sector <> 0 THEN liq_items_tarjas.id_sector = v_sector ELSE true) AND
      liq_items_tarjas.id_tipo_planilla <> 4 AND
      (liq_items_tarjas.cant_horas <> 0 OR liq_items_tarjas.cantidad <> 0) 
      no-lock,
     first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_tarjas.id_empresa and liq_legajos.legajo = liq_items_tarjas.legajo and
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
     BREAK by liq_items_tarjas.id_empresa by liq_items_tarjas.legajo
      by liq_items_tarjas.nombre by liq_items_tarjas.fecha :

      
      FIND FIRST liq_tarjas OF liq_items_tarjas NO-LOCK NO-ERROR.
      IF NOT AVAILABLE liq_tarjas THEN NEXT.

      find first liq_tareas where liq_tareas.id_tarea = liq_items_tarjas.id_tarea no-lock no-error.
      if NOT available liq_tareas THEN NEXT.

      v_finca = 0.

      IF liq_tarjas.id_sector <> 6 THEN /* COSECHA */
      DO:
          IF liq_legajos.toma_ccosto_legajo = NO THEN
          DO:

              find first liq_ccostos where liq_ccostos.id_proveedor = liq_tarjas.id_proveedor and
              liq_ccostos.id_origen = liq_tarjas.id_origen AND
              liq_ccostos.id_centro_costo > 100000000  no-lock no-error.
              if available liq_ccostos Then
                v_finca = liq_ccostos.id_centro_costo_liq.
               Else
               DO:
                   FIND FIRST liq_centros_costos WHERE 
                       liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
                       liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
                   IF AVAILABLE liq_centros_costos THEN
                   DO:
                       FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
                           liq_ccostos.id_origen = liq_centros_costos.id_origen and
                           liq_ccostos.id_centro_costo > 100000000 NO-LOCK NO-ERROR.
                       IF AVAILABLE liq_ccostos THEN
                           v_finca = liq_ccostos.id_centro_costo_liq.
                   END.
               END.
          END.
         ELSE v_finca = liq_legajos.id_ccostos_liq.
      END.
      ELSE v_finca = 1112990400.


      IF liq_items_tarjas.cant_hs_norm <> 0 THEN
      DO:
      find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
      t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus and
      t-personal.id_centro_abacus = v_finca no-lock no-error.
      if not available t-personal Then
         do:
            create t-personal.
            assign t-personal.legajo = liq_items_tarjas.legajo
                   t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus 
                   t-personal.id_centro_abacus = v_finca
                   t-personal.id_tipo_motivo = 2
                   t-personal.id_motivo = 1
                   t-personal.id_asiento = 2
                   t-personal.id_estructura1 = v_finca
                   t-personal.fecha_desde = liq_items_tarjas.fecha
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
         end. 
         t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.cant_hs_norm.
         IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
             t-personal.fecha_hasta = liq_items_tarjas.fecha.
      end.


    
        /* Agregado 22/09/16*/
        IF liq_items_tarjas.hs_acond_finca <> 0 THEN
        DO:
            find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
            t-personal.id_concepto = 1645 and
            t-personal.id_centro_abacus = v_finca no-lock no-error.
            if not available t-personal Then
               do:
                  create t-personal.
                  assign t-personal.legajo = liq_items_tarjas.legajo
                         t-personal.id_concepto = 1645 
                         t-personal.id_centro_abacus = v_finca
                         t-personal.id_tipo_motivo = 2
                         t-personal.id_motivo = 1
                         t-personal.id_asiento = 2
                         t-personal.id_estructura1 = v_finca
                         t-personal.fecha_desde = liq_items_tarjas.fecha
                         t-personal.fecha_hasta = liq_items_tarjas.fecha.
               end. 
               t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.hs_acond_finca.
               IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
        END.



        IF liq_items_tarjas.hs_plus_tareas_automatico <> 0 THEN
        DO:
            find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
            t-personal.id_concepto = 1430 and
            t-personal.id_centro_abacus = v_finca no-lock no-error.
            if not available t-personal Then
               do:
                  create t-personal.
                  assign t-personal.legajo = liq_items_tarjas.legajo
                         t-personal.id_concepto = 1430 
                         t-personal.id_centro_abacus = v_finca
                         t-personal.id_tipo_motivo = 2
                         t-personal.id_motivo = 1
                         t-personal.id_asiento = 2
                         t-personal.id_estructura1 = v_finca
                         t-personal.fecha_desde = liq_items_tarjas.fecha
                         t-personal.fecha_hasta = liq_items_tarjas.fecha.
               end. 
               t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.hs_plus_tareas_automatico.
               IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.

        END.


        IF liq_items_tarjas.hs_plus_tareas_trabajadas <> 0 THEN
        DO:
            find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
            t-personal.id_concepto = 1430 and
            t-personal.id_centro_abacus = v_finca no-lock no-error.
            if not available t-personal Then
               do:
                  create t-personal.
                  assign t-personal.legajo = liq_items_tarjas.legajo
                         t-personal.id_concepto = 1430 
                         t-personal.id_centro_abacus = v_finca
                         t-personal.id_tipo_motivo = 2
                         t-personal.id_motivo = 1
                         t-personal.id_asiento = 2
                         t-personal.id_estructura1 = v_finca
                         t-personal.fecha_desde = liq_items_tarjas.fecha
                         t-personal.fecha_hasta = liq_items_tarjas.fecha.
               end. 
               t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.hs_plus_tareas_trabajadas.
               IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
        END.


        IF liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0 THEN
        DO:
            find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
            t-personal.id_concepto = 1430 and
            t-personal.id_centro_abacus = v_finca no-lock no-error.
            if not available t-personal Then
               do:
                  create t-personal.
                  assign t-personal.legajo = liq_items_tarjas.legajo
                         t-personal.id_concepto = 1430 
                         t-personal.id_centro_abacus = v_finca
                         t-personal.id_tipo_motivo = 2
                         t-personal.id_motivo = 1
                         t-personal.id_asiento = 2
                         t-personal.id_estructura1 = v_finca
                         t-personal.fecha_desde = liq_items_tarjas.fecha
                         t-personal.fecha_hasta = liq_items_tarjas.fecha.
               end. 
               t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.hs_adicionales_tareas_trabajadas.
               IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
        END.


      IF liq_items_tarjas.cantidad <> 0 THEN
       DO:

            find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
            t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus_cantidad and
            t-personal.id_centro_abacus = v_finca no-lock no-error.
            if not available t-personal Then
               do:
                  create t-personal.
                  assign t-personal.legajo = liq_items_tarjas.legajo
                         t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus_cantidad 
                         t-personal.id_centro_abacus = v_finca
                         t-personal.id_tipo_motivo = 2
                         t-personal.id_motivo = 1
                         t-personal.id_asiento = 2
                         t-personal.id_estructura1 = v_finca
                         t-personal.fecha_desde = liq_items_tarjas.fecha
                         t-personal.fecha_hasta = liq_items_tarjas.fecha.
               end. 
               t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.cantidad.
               IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
        
       END.


       IF liq_items_tarjas.id_codigo_abacus_diferencial <> 0 THEN
        DO:

             find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
             t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus_diferencial and
             t-personal.id_centro_abacus = v_finca no-lock no-error.
             if not available t-personal Then
                do:
                   create t-personal.
                   assign t-personal.legajo = liq_items_tarjas.legajo
                          t-personal.id_concepto = liq_items_tarjas.id_codigo_abacus_diferencial 
                          t-personal.id_centro_abacus = v_finca
                          t-personal.id_tipo_motivo = 2
                          t-personal.id_motivo = 1
                          t-personal.id_asiento = 2
                          t-personal.id_estructura1 = v_finca
                          t-personal.fecha_desde = liq_items_tarjas.fecha
                          t-personal.fecha_hasta = liq_items_tarjas.fecha.
                end. 
                t-personal.cantidad = t-personal.cantidad + liq_items_tarjas.cant_horas.
                IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                    t-personal.fecha_hasta = liq_items_tarjas.fecha.

        END.
 END.




        /****************/
            

      

/* Se excluyen del contado de dias las liq_tareas con sector 0 */

for each liq_tarjas no-lock where liq_tarjas.id_empresa = v_empresa and 
      (IF v_sector <> 0 THEN liq_tarjas.id_sector = v_sector ELSE TRUE) and
      liq_tarjas.fecha >= v_fecha_desde and 
      liq_tarjas.fecha <= v_fecha_hasta 
      ,each liq_items_tarjas of liq_tarjas where
       liq_items_tarjas.id_tarea <> 0 no-lock,
       first liq_legajos where liq_legajos.id_empresa_liq =
           liq_items_tarjas.id_empresa and liq_legajos.legajo = liq_items_tarjas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK,
       first liq_tareas where liq_tareas.id_tarea = liq_items_tarjas.id_tarea no-lock 
       break by liq_items_tarjas.id_empresa by legajo by liq_items_tarjas.fecha:
      
       
            v_finca = 0.

            IF liq_tarjas.id_sector <> 6  THEN  /* COSECHA */
            DO:
                IF liq_legajos.toma_ccosto_legajo = NO THEN
                DO:
                        find first liq_ccostos where liq_ccostos.id_proveedor = liq_tarjas.id_proveedor and
                        liq_ccostos.id_origen = liq_tarjas.id_origen AND
                        liq_ccostos.id_centro_costo > 100000000  no-lock no-error.
                        if available liq_ccostos Then
                          v_finca = liq_ccostos.id_centro_costo_liq.
                         Else
                         DO:
                             FIND FIRST liq_centros_costos WHERE 
                                 liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
                                 liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
                             IF AVAILABLE liq_centros_costos THEN
                             DO:
                                 FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
                                     liq_ccostos.id_origen = liq_centros_costos.id_origen and
                                     liq_ccostos.id_centro_costo > 100000000 NO-LOCK NO-ERROR.
                                 IF AVAILABLE liq_ccostos THEN
                                     v_finca = liq_ccostos.id_centro_costo_liq.
                             END.
                         END.
                END.
                ELSE v_finca = liq_legajos.id_ccostos_liq.
            END.
            ELSE v_finca = 1112990400.

              /************************************************************/ 
              /* Llevo a horas todas las liq_tareas realizadas ***************/
            
              if liq_items_tarjas.cant_horas <> 0 Then
                 v_horas = v_horas + liq_items_tarjas.cant_horas.
                 
              if liq_items_tarjas.cantidad <> 0 Then
                 do:
                    case liq_items_tarjas.id_codigo_abacus_cantidad:
                        WHEN 1610 OR WHEN 1620 OR WHEN 2630 THEN
                           v_horas = v_horas + 8. 
                    end case.   
                 end.   
               /************************************************************/   

               if last-of(liq_items_tarjas.fecha) Then
                  do:
                      if v_horas > 8 Then v_horas = 8.
                      v_codigo = 941.
                      find first t-personal where t-personal.legajo = liq_items_tarjas.legajo and
                      t-personal.id_concepto = v_codigo and
                      t-personal.id_centro_abacus = v_finca no-lock no-error.
                      if not available t-personal Then
                         do:
                            create t-personal.
                            assign t-personal.legajo = liq_items_tarjas.legajo
                                   t-personal.id_concepto = v_codigo 
                                   t-personal.id_centro_abacus = v_finca
                                   t-personal.id_tipo_motivo = 2
                                   t-personal.id_motivo = 1
                                   t-personal.id_asiento = 2
                                   t-personal.id_estructura1 = v_finca
                                   t-personal.fecha_desde = liq_items_tarjas.fecha
                                   t-personal.fecha_hasta = liq_items_tarjas.fecha.
                         end.
                      t-personal.cantidad = t-personal.cantidad + (v_horas / 8).
                      v_horas = 0.
                      IF liq_items_tarjas.fecha > t-personal.fecha_hasta THEN
                          t-personal.fecha_hasta = liq_items_tarjas.fecha.
                   end.
end.      


ASSIGN SESSION:NUMERIC-FORMAT = "EUROPEAN"
       SESSION:DATE-FORMAT = "dmy".

IF v-tipo = "" THEN
    output to value (v_carpeta + "RHPRO"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                     string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                     string(year(v_fecha_hasta),"9999")
                      + ".csv").
 ELSE
     output to value (v_carpeta + v-tipo + "-RHPRO"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                      string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                      string(year(v_fecha_hasta),"9999")
                       + ".csv").
PUT "Legajo;Concepto;Parametros;Monto;Fecha Desde;Fecha Hasta;Tipo de motivo;Motivo;Modelo Asiento;Estructura1;".
PUT SKIP.

  for each t-personal WHERE t-personal.id_concepto <> 912 by t-personal.legajo by t-personal.id_concepto :
    FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = t-personal.id_concepto NO-ERROR.
    IF AVAILABLE liq_conceptos THEN ASSIGN t-personal.id_parametro = liq_conceptos.id_parametro.

    v_cantidad = trim(STRING(t-personal.cantidad, ">>>>9.99")).

    IF v_empresa <> 101 THEN
                         DO:
                          v_inicio = integer(SUBSTRING(STRING(v_empresa),1,1)).
                          v_legajo = (v_inicio * 10000000) + t-personal.legajo.
                         END.
                      ELSE v_legajo = t-personal.legajo.

    PUT v_legajo format ">>>>>>>>".
    PUT ";".
    PUT t-personal.id_concepto format "99999".
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
        
   end.
output close.

ASSIGN SESSION:NUMERIC-FORMAT = "AMERICAN".

message "Archivo de TARJAS generado" view-as alert-box.
   
   
