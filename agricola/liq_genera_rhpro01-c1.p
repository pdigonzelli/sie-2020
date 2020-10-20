/******************************************************************/
/* SECTOR AGRICOLA                                                */ 
/* Genera archivo de liquidación de TARJA para incorporacion      */
/* en el Abacus                                                   */ 
/* Se toma como centro de costo la finca donde trabajo 15/01/2001 */
/***********************************************************************/
/* El 16/07/02 se agrega la cant de dias trabajados - cod ABACUS (941) */
/***********************************************************************/   

define input parameter v_empresa like liq_control_tareas.id_empresa.
define input parameter v_sector like liq_control_tareas.id_sector.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
DEFINE INPUT PARAMETER v_carpeta AS CHARACTER.
DEFINE INPUT PARAMETER v-tipo AS CHARACTER.
  
def var v_centro as integer.
def var v_finca as integer.
def var v_cuenta as integer.

def var v_cuenta_jornal as integer.
def var v_cuenta_horas as integer.
def var v_cuenta_otros as integer.
def var v_jornal as decimal.

def var v_codigo as integer.
def var v_codigo-1 as integer.
def var v_codigo-2 as integer.

def var v_entero as decimal.
def var v_decimal as decimal.
define var v_cargo like liq_legajos.id_cargo.
define var v_horas as decimal.
DEF VAR v_cantidad AS CHARACTER.
DEF VAR v_legajo LIKE liq_items_control_tareas.legajo.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_desde AS CHARACTER.
DEF VAR v_hasta AS CHARACTER.

define temp-table t-personal
   field legajo like liq_items_control_tareas.legajo
   field id_concepto like r_tareas_unidades.id_concepto
   field cantidad like liq_items_control_tareas.cantidad FORMAT ">>>>9.99"
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
   

for each liq_control_tareas no-lock where id_empresa = v_empresa and 
      liq_control_tareas.id_sector = v_sector and
      liq_control_tareas.fecha >= v_fecha_desde and liq_control_tareas.fecha <= v_fecha_hasta 
      ,each liq_items_control_tareas of liq_control_tareas 
       WHERE liq_items_control_tareas.id_tarea <> 0 no-lock,
       first liq_legajos where liq_legajos.id_empresa_liq =
        liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
       (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
       by liq_items_control_tareas.id_empresa by legajo by liq_items_control_tareas.fecha:
      
        find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
          v_cargo = liq_legajos.id_cargo.        
          v_finca = 0.


          IF liq_legajos.toma_ccosto_legajo = NO THEN
          DO:
    
              find first liq_ccostos where liq_ccostos.id_proveedor = liq_control_tareas.id_proveedor and
              liq_ccostos.id_origen = liq_control_tareas.id_origen AND
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

        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.   
    
       {liq-calcula-rhpro-agricola.i} 
 
      if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_jornal.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
       end.
    

  
     if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-1 and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.

          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_horas.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
    end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-2 and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_finca
                    t-personal.id_tipo_motivo = 2
                    t-personal.id_motivo = 1
                    t-personal.id_asiento = 2
                    t-personal.id_estructura1 = v_finca
                    t-personal.fecha_desde = liq_items_control_tareas.fecha
                    t-personal.fecha_hasta = liq_items_control_tareas.fecha.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cantidad.
          IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
              t-personal.fecha_hasta = liq_items_control_tareas.fecha.
     end.

  end.
end.   
      

/* Se excluyen del contado de dias las tareas con sector 0 */

for each liq_control_tareas no-lock where id_empresa = v_empresa and 
      liq_control_tareas.id_sector = v_sector and
      liq_control_tareas.fecha >= v_fecha_desde and liq_control_tareas.fecha <= v_fecha_hasta 
      ,each liq_items_control_tareas of liq_control_tareas where
       liq_items_control_tareas.id_tarea <> 0 no-lock,
       first liq_legajos where liq_legajos.id_empresa_liq =
           liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK,
       first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea and 
       (tareas.id_sector <> 0) no-lock 
       break by liq_items_control_tareas.id_empresa by legajo by liq_items_control_tareas.fecha:
      
       
                v_cargo = liq_legajos.id_cargo.        
                v_finca = 0.

                IF liq_legajos.toma_ccosto_legajo = NO THEN
                DO:
                        find first liq_ccostos where liq_ccostos.id_proveedor = liq_control_tareas.id_proveedor and
                        liq_ccostos.id_origen = liq_control_tareas.id_origen AND
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

              /************************************************************/ 
              /* Llevo a horas todas las tareas realizadas ***************/
            
              if liq_items_control_tareas.cant_jornal <> 0 Then
                 v_horas = v_horas + (liq_items_control_tareas.cant_jornal * 8).

              if liq_items_control_tareas.cant_horas <> 0 Then
                 v_horas = v_horas + liq_items_control_tareas.cant_horas.
                 
              if liq_items_control_tareas.cantidad <> 0 Then
                 do:
                    case liq_items_control_tareas.id_unidad_liquidacion:
                        when 1 or when 5 or when 10 or when 11 or when 14 Then
                           v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
                        when 2 or when 3 or when 7 or when 15 or when 17
                               or when 20 or when 21 or when 22 or when 23
                               or when 24 Then
                           v_horas = v_horas + liq_items_control_tareas.cantidad.
                        otherwise 
                           v_horas = v_horas + 8. 
                    end case.   
                 end.   
               /************************************************************/   

               if last-of(liq_items_control_tareas.fecha) Then
                  do:
                      if v_horas > 8 Then v_horas = 8.
                      v_codigo = 941.
                      find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
                      t-personal.id_concepto = v_codigo and
                      t-personal.id_centro_abacus = v_finca no-lock no-error.
                      if not available t-personal Then
                         do:
                            create t-personal.
                            assign t-personal.legajo = liq_items_control_tareas.legajo
                                   t-personal.id_concepto = v_codigo 
                                   t-personal.id_centro_abacus = v_finca
                                   t-personal.id_tipo_motivo = 2
                                   t-personal.id_motivo = 1
                                   t-personal.id_asiento = 2
                                   t-personal.id_estructura1 = v_finca
                                   t-personal.fecha_desde = liq_items_control_tareas.fecha
                                   t-personal.fecha_hasta = liq_items_control_tareas.fecha.
                         end.
                      t-personal.cantidad = t-personal.cantidad + (v_horas / 8).
                      v_horas = 0.
                      IF liq_items_control_tareas.fecha > t-personal.fecha_hasta THEN
                          t-personal.fecha_hasta = liq_items_control_tareas.fecha.
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
   
   
