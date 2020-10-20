/********************************************************************/
/* SECTOR COSECHA                                                   */
/* Genera archivo de liquidación de TARJA para incorporacion        */
/* en el Abacus                                                     */
/* Se toma como centro de costo el que figura en el archivo maestro */ 
/********************************************************************/

define input parameter v_empresa like control_tareas.id_empresa.
define input parameter v_sector like control_tareas.id_sector.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
  
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
define var v_cargo like personal_finca.id_cargo.
define var v_horas as decimal.

define temp-table t-personal
   field legajo like items_control_tareas.legajo
   field id_concepto like r_tareas_unidades.id_concepto
   field cantidad like items_control_tareas.cantidad
   field id_centro_abacus like personal_finca.id_centro_abacus.

    
   
for each t-personal:
  delete t-personal. 
end.
   

for each control_tareas no-lock where control_tareas.id_empresa = v_empresa and 
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
  
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.   
    
    {calcula-cosecha.i} 
        

    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = personal_finca.id_centro_abacus.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = personal_finca.id_centro_abacus.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = personal_finca.id_centro_abacus.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
     end.

   end.      
  end.
end.   
      

/* Se excluyen del contado de dias las tareas con sector 0 */

v_horas = 0.
for each control_tareas no-lock where id_empresa = v_empresa and 
      control_tareas.id_sector = v_sector and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
      ,each items_control_tareas of control_tareas where
       items_control_tareas.id_tarea <> 0 no-lock,
       first tareas where tareas.id_tarea = items_control_tareas.id_tarea and 
       (tareas.id_sector <> 0) no-lock 
       break by items_control_tareas.id_empresa by legajo by items_control_tareas.fecha:
      
       
       find first personal_finca where personal_finca.id_empresa_cosechera =
       items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
       if available personal_finca Then
        do:
        
              /************************************************************/ 
              /* Llevo a horas todas las tareas realizadas ***************/
            
              if items_control_tareas.cant_jornal <> 0 Then
                 v_horas = v_horas + (items_control_tareas.cant_jornal * 8).

              if items_control_tareas.cant_horas <> 0 Then
                 v_horas = v_horas + items_control_tareas.cant_horas.
                 
              if items_control_tareas.cantidad <> 0 Then
                 do:
                    case items_control_tareas.id_unidad_liquidacion:
                        when 1 or when 5 or when 10 or when 11 or when 14 Then
                           v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
                        when 2 or when 3 or when 7 or when 15 or when 17
                               or when 20 or when 21 or when 22 or when 23
                               or when 24 Then
                           v_horas = v_horas + items_control_tareas.cant_horas.
                        otherwise 
                           v_horas = v_horas + 8. 
                    end case.   
                 end.   
               /************************************************************/   


        
        
           if last-of(items_control_tareas.fecha) Then
            do:
                v_codigo = 941.
                find first t-personal where t-personal.legajo = items_control_tareas.legajo and
                t-personal.id_concepto = v_codigo no-lock no-error.
                if not available t-personal Then
                   do:
                      create t-personal.
                      assign t-personal.legajo = items_control_tareas.legajo
                             t-personal.id_concepto = v_codigo 
                             t-personal.id_centro_abacus = personal_finca.id_centro_abacus.
                   end.
                if v_horas > 8 Then v_horas = 8.   
                t-personal.cantidad = t-personal.cantidad + (v_horas / 8) .
                v_horas = 0.
             end.
      end.
end.


output to value ("c:\temp\" + "E"+ string(v_empresa,"999999") + "-TP-" + string(v_sector,"99") + "-" + string(day(v_fecha_desde),"99") + "-" +
                     string(day(v_fecha_hasta),"99") + "-" + string(month(v_fecha_hasta),"99") + "-" +
                     string(year(v_fecha_hasta),"9999")
                      + ".txt").

  for each t-personal by t-personal.legajo by t-personal.id_concepto:
    v_entero = truncate(t-personal.cantidad,0).
    v_decimal = t-personal.cantidad - truncate(t-personal.cantidad,0). 
    
    if t-personal.id_concepto = 941 Then
      do:
      find first personal_finca where personal_finca.legajo = t-personal.legajo no-lock no-error.
      if available personal_finca Then
         do:
            if personal_finca.id_cargo = 45 Then next.
         end.
      end.
    put t-personal.legajo format "99999"  
        t-personal.id_centro_abacus format "999999"
        t-personal.id_concepto format "999"
        v_entero format "9999999" 
        substring(string(v_decimal,"9.99"),3,2) skip .
   end.
output close.

message "Archivo de TARJAS generado" view-as alert-box.
   
   
