/******************************************************************/
/* SECTOR AGRICOLA                                                */ 
/* Genera archivo de liquidación de TARJA para incorporacion      */
/* en el Abacus                                                   */ 
/* Se toma como centro de costo la finca donde trabajo 15/01/2001 */
/***********************************************************************/
/* El 16/07/02 se agrega la cant de dias trabajados - cod ABACUS (941) */
/***********************************************************************/   

define input parameter v_sector like control_tareas.id_sector.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
  
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
define var v_cargo like personal_finca.id_cargo.


define temp-table t-personal
   field legajo like items_control_tareas.legajo
   field id_concepto like r_tareas_unidades.id_concepto
   field cantidad like items_control_tareas.cantidad
   field id_centro_abacus like personal_finca.id_centro_abacus.

    
   
for each t-personal:
  delete t-personal. 
end.
   

for each control_tareas no-lock where id_empresa = 1 and 
      control_tareas.id_sector = v_sector and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
      ,each items_control_tareas of control_tareas no-lock 
       by items_control_tareas.id_empresa by legajo by items_control_tareas.fecha:
      
       find first personal_finca where personal_finca.id_empresa_cosechera =
       items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
       if available personal_finca Then
        do:
        find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
          v_cargo = personal_finca.id_cargo.        

          find first centros_costos_abacus where centros_costos_abacus.id_proveedor = control_tareas.id_proveedor and
          centros_costos_abacus.id_origen = control_tareas.id_origen and
          centros_costos_abacus.tipo_liquidacion = personal_finca.tipo_liquidacion no-lock no-error.
          if available centros_costos_abacus Then
            v_finca = centros_costos_abacus.id_centro_abacus.
           Else
            v_finca = personal_finca.id_centro_abacus.
            

        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.   
    
       {calcula-agricola.i} 
 
      if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_finca.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    

  
     if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-1 and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_finca.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-2 and
       t-personal.id_centro_abacus = v_finca no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_finca.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
     end.

   end.      
  end.
end.   
      

/* Se excluyen del contado de dias las tareas 114,115,117 y 118 */

for each control_tareas no-lock where id_empresa = 1 and 
      control_tareas.id_sector = v_sector and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
      ,each items_control_tareas of control_tareas where
      (items_control_tareas.id_tarea <> 114 and 
       items_control_tareas.id_tarea <> 115 and
       items_control_tareas.id_tarea <> 117 and
       items_control_tareas.id_tarea <> 118 and
       items_control_tareas.id_tarea <> 0) no-lock 
       break by items_control_tareas.id_empresa by legajo by items_control_tareas.fecha:
      
       find first personal_finca where personal_finca.id_empresa_cosechera =
       items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
       if available personal_finca Then
        do:
            find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
            if available tareas Then
            do:
                v_cargo = personal_finca.id_cargo.        
      
                find first centros_costos_abacus where centros_costos_abacus.id_proveedor = control_tareas.id_proveedor and
                centros_costos_abacus.id_origen = control_tareas.id_origen and
                centros_costos_abacus.tipo_liquidacion = personal_finca.tipo_liquidacion no-lock no-error.
                if available centros_costos_abacus Then
                  v_finca = centros_costos_abacus.id_centro_abacus.
                 Else
                  v_finca = personal_finca.id_centro_abacus.
                  
               if last-of(items_control_tareas.fecha) Then
                  do:
                      v_codigo = 941.
                      find first t-personal where t-personal.legajo = items_control_tareas.legajo and
                      t-personal.id_concepto = v_codigo and
                      t-personal.id_centro_abacus = v_finca no-lock no-error.
                      if not available t-personal Then
                         do:
                            create t-personal.
                            assign t-personal.legajo = items_control_tareas.legajo
                                   t-personal.id_concepto = v_codigo 
                                   t-personal.id_centro_abacus = v_finca.
                         end.
                      t-personal.cantidad = t-personal.cantidad + 1 .
                   end.
             end.      
       end.          
end.      



output to value ("c:\temp\TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                     string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                     string(year(v_fecha_hasta),"9999")
                      + ".txt").

  for each t-personal by t-personal.legajo by t-personal.id_concepto :
    v_entero = truncate(t-personal.cantidad,0).
    v_decimal = t-personal.cantidad - truncate(t-personal.cantidad,0). 

    put t-personal.legajo format "99999"  
        t-personal.id_centro_abacus format "999999"
        t-personal.id_concepto format "999"
        v_entero format "9999999" 
        substring(string(v_decimal,"9.99"),3,2) skip .
   end.
output close.

message "Archivo de TARJAS generado" view-as alert-box.
   
   
