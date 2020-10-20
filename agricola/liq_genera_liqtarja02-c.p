/********************************************************************/
/* SECTOR COSECHA                                                   */
/* Genera archivo de liquidación de TARJA para incorporacion        */
/* en el Abacus                                                     */
/* Se toma como centro de costo el que figura en el archivo maestro */ 
/********************************************************************/

define input parameter v_empresa like liq_control_tareas.id_empresa.
define input parameter v_sector like liq_control_tareas.id_sector.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
DEFINE INPUT PARAMETER v_carpeta AS CHARACTER.
DEF INPUT PARAMETER v-tipo AS CHARACTER.
  
def var v_centro as integer.
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

define temp-table t-personal
   field legajo like liq_items_control_tareas.legajo
   field id_concepto like r_tareas_unidades.id_concepto
   field cantidad like liq_items_control_tareas.cantidad
   field id_centro_abacus like liq_legajos.id_centro_costo.

    
{s_varsis.i}
   
for each t-personal:
  delete t-personal. 
end.
   

for each liq_control_tareas no-lock where liq_control_tareas.id_empresa = v_empresa and 
      liq_control_tareas.id_sector = v_sector and 
      liq_control_tareas.fecha >= v_fecha_desde and liq_control_tareas.fecha <= v_fecha_hasta 
      ,each liq_items_control_tareas of liq_control_tareas no-lock,
      first liq_legajos where liq_legajos.id_empresa_liq =
       liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
       by liq_items_control_tareas.id_empresa by legajo by liq_items_control_tareas.fecha:
      
        find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
      
        v_cargo = liq_legajos.id_cargo.
  
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.   
    
    {liq-calcula-cosecha.i} 
        

    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = liq_legajos.id_centro_costo.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = liq_legajos.id_centro_costo.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = liq_legajos.id_centro_costo.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cantidad.
     end.

  end.
end.   
      

/* Se excluyen del contado de dias las tareas con sector 0 */

v_horas = 0.
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

            IF liq_items_control_tareas.legajo = 50654 THEN  MESSAGE v_horas liq_items_control_tareas.fecha VIEW-AS ALERT-BOX.
        
        
           if last-of(liq_items_control_tareas.fecha) Then
            do:
                v_codigo = 941.
                find first t-personal where t-personal.legajo = liq_items_control_tareas.legajo and
                t-personal.id_concepto = v_codigo no-lock no-error.
                if not available t-personal Then
                   do:
                      create t-personal.
                      assign t-personal.legajo = liq_items_control_tareas.legajo
                             t-personal.id_concepto = v_codigo 
                             t-personal.id_centro_abacus = liq_legajos.id_centro_costo.
                   end.
                if v_horas > 8 Then v_horas = 8.   
                t-personal.cantidad = t-personal.cantidad + (v_horas / 8) .
                v_horas = 0.
             end.
end.

IF v-tipo = "" THEN
   output to value (v_carpeta + "E"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                     string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                     string(year(v_fecha_hasta),"9999")
                      + ".txt").
ELSE
    output to value (v_carpeta + v-tipo + "-E"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                         string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                         string(year(v_fecha_hasta),"9999")
                          + ".txt").


  for each t-personal by t-personal.legajo by t-personal.id_concepto:
    v_entero = truncate(t-personal.cantidad,0).
    v_decimal = t-personal.cantidad - truncate(t-personal.cantidad,0). 
    
    put t-personal.legajo format "999999"  
        t-personal.id_centro_abacus format "999999"
        t-personal.id_concepto format "999"
        v_entero format "9999999" 
        substring(string(v_decimal,"9.99"),3,2) skip .
   end.
output close.

message "Archivo de TARJAS generado" view-as alert-box.
   
   
