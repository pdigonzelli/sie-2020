define input parameter p_sector like liq_control_tareas.id_sector.
define input parameter p_fecha_desde like liq_control_tareas.fecha.
define input parameter p_fecha_hasta like liq_control_tareas.fecha.
define input parameter p_archivo as character.
 
define temp-table rb_resumen_3eros like liq_items_control_tareas
    field jornal_peon like liq_items_control_tareas.cant_jornal
    field jornal_trac like liq_items_control_tareas.cant_jornal
    field jornal_ayud like liq_items_control_tareas.cant_jornal
    field horas_peon like liq_items_control_tareas.cant_horas
    field horas_trac like liq_items_control_tareas.cant_horas
    field horas_trac_cont like liq_items_control_tareas.cant_horas
    field horas_tractoelev like liq_items_control_tareas.cantidad
    field destajo like liq_items_control_tareas.cantidad
    field plus_x_bins like liq_items_control_tareas.cantidad
    field viatico_peon like liq_items_control_tareas.cantidad
    field viatico_ayud like liq_items_control_tareas.cantidad
    field lic like liq_items_control_tareas.cantidad
    field lluvia like liq_items_control_tareas.cantidad.

define var v_cargo as integer.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   



for each liq_control_tareas where id_sector = p_sector and 
    liq_control_tareas.fecha >= p_fecha_desde and
    liq_control_tareas.fecha <= p_fecha_hasta and
    liq_control_tareas.id_tipo_planilla <> 4 no-lock:
    for each liq_items_control_tareas of liq_control_tareas where id_tarea <> 0 no-lock:
      
        find first liq_legajos where liq_legajos.id_empresa_liq =
              liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo no-lock no-error.
              if available liq_legajos Then
                  v_cargo = liq_legajos.id_cargo.
                Else
                  v_cargo = 0. 
    
       find first rb_resumen_3eros where rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa and
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo and
                  /*rb_resumen_3eros.nombre = liq_items_control_tareas.nombre and*/
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil and
                  rb_resumen_3eros.id_tarea = liq_items_control_tareas.id_tarea and
                  rb_resumen_3eros.id_proveedor = liq_items_control_tareas.id_proveedor and
                  rb_resumen_3eros.id_origen = liq_items_control_tareas.id_origen no-lock no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa 
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = liq_items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil 
                  rb_resumen_3eros.id_tarea = liq_items_control_tareas.id_tarea 
                  rb_resumen_3eros.id_proveedor = liq_items_control_tareas.id_proveedor 
                  rb_resumen_3eros.id_origen = liq_items_control_tareas.id_origen.
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and v_cargo <> 32 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + liq_items_control_tareas.cant_jornal.            

         /* Js M 18 */
          IF liq_items_control_tareas.id_unidad_liquidacion = 14 AND cant_jornal = 0 THEN
              rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + liq_items_control_tareas.cantidad.            


         /* Jornal Trac */
         if (v_cargo = 42 or liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + liq_items_control_tareas.cant_jornal.            

         /* Jornal Ayudante Capataz */
         if (v_cargo = 32 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + liq_items_control_tareas.cant_jornal.            

          if liq_items_control_tareas.id_unidad_liquidacion = 11 Then
             rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + liq_items_control_tareas.cantidad.          


         /* Horas Peon */
         if (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           rb_resumen_3eros.horas_peon = rb_resumen_3eros.horas_peon + liq_items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) and 
            liq_items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_3eros.horas_trac = rb_resumen_3eros.horas_trac + liq_items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (liq_items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_3eros.horas_trac_cont = rb_resumen_3eros.horas_trac_cont + liq_items_control_tareas.cant_horas.            

         /* Destajo */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 4 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + liq_items_control_tareas.cantidad.            

         /* Hs.Tractoelev */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 17 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.horas_tractoelev = rb_resumen_3eros.horas_tractoelev + liq_items_control_tareas.cantidad.            

         /* Plus x Bins */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 16 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.plus_x_bins = rb_resumen_3eros.plus_x_bins + liq_items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 8 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_peon = rb_resumen_3eros.viatico_peon + liq_items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 9 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_ayud = rb_resumen_3eros.viatico_ayud + liq_items_control_tareas.cantidad.            

    
         /* Licencias */
        
         if (liq_items_control_tareas.id_tarea >= 107 and liq_items_control_tareas.id_tarea <= 116) or liq_items_control_tareas.id_tarea = 118 Then
           rb_resumen_3eros.lic = rb_resumen_3eros.lic + liq_items_control_tareas.cantidad.            
         
         /* LLuvia */  
         if liq_items_control_tareas.id_tarea = 117 Then
           rb_resumen_3eros.lluvia = rb_resumen_3eros.lluvia + liq_items_control_tareas.cantidad.            

           
    end.
end.

if p_archivo = "" Then p_archivo = "Z:\temp\repcose01.txt".

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Sector;". 
  put "Nombre Sector;".
  put "Legajo;". 
  put "Nombre;". 
  put "DNI/CUIL;". 
  put "Cod.;". 
  put "Tarea;". 
  put "Desc.Tarea ;".
  put "FINCA;". 
  put "JS.PEON;". 
  put "JS.TRAC;". 
  put "JS.AYUD.CAP.;". 
  put "HS.PEON;". 
  put "HS.TRAC;". 
  put "TRAC.CONT;". 
  put "HS.TRACTOELV.;". 
  put "PLUS x BINS;". 
  put "VIATICO PEON;". 
  put "VIATICO AYUD;".
  put "LIC.;".
  put "LLUVIA;".
  put skip.
  for each rb_resumen_3eros:
      find first liq_empresas where liq_empresas.id_empresa_liq = rb_resumen_3eros.id_empresa no-lock no-error.
     find first liq_sectores where liq_sectores.id_empresa_liq = rb_resumen_3eros.id_empresa AND
     liq_sectores.id_sector = rb_resumen_3eros.id_sector no-lock no-error.

     find first tareas of rb_resumen_3eros no-lock no-error.
     find first origenes of rb_resumen_3eros no-lock no-error.
     if available tareas Then
               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                 if available liq_empresas Then liq_empresas.descripcion Else " "
                 rb_resumen_3eros.id_sector
                 if available liq_sectores Then liq_sectores.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.id_tarea
                tareas.abreviatura 
                tareas.descripcion 
                if available origenes Then origenes.descripcion Else " "
                rb_resumen_3eros.jornal_peon
                rb_resumen_3eros.jornal_trac
                rb_resumen_3eros.jornal_ayud
                rb_resumen_3eros.horas_peon
                rb_resumen_3eros.horas_trac
                rb_resumen_3eros.horas_trac_cont
                rb_resumen_3eros.horas_tractoelev
                rb_resumen_3eros.plus_x_bins
                rb_resumen_3eros.viatico_peon
                rb_resumen_3eros.viatico_ayud
                rb_resumen_3eros.lic
                rb_resumen_3eros.lluvia.
  end.
output close.

message "Archivo generado " p_archivo view-as alert-box.

run p_texto_a_excel.p (input "TEXT;" + p_archivo).
