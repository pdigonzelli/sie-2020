define input parameter p_sector like control_tareas.id_sector.
define input parameter p_fecha_desde like control_tareas.fecha.
define input parameter p_fecha_hasta like control_tareas.fecha.
DEFINE INPUT PARAMETER p_carpeta AS CHARACTER.

define var p_archivo as character.
 
define temp-table rb_resumen_3eros like items_control_tareas
    field jornal_peon like items_control_tareas.cant_jornal
    field jornal_trac like items_control_tareas.cant_jornal
    field horas_peon like items_control_tareas.cant_horas
    field horas_trac like items_control_tareas.cant_horas
    field horas_trac_cont like items_control_tareas.cant_horas
    field horas_pulv like items_control_tareas.cantidad
    field horas_pulv_trac like items_control_tareas.cantidad
    field destajo like items_control_tareas.cantidad
    field cant_hectareas like items_control_tareas.cantidad.

    
define var v_cargo as integer.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   

p_archivo = p_carpeta + "maquinarias.txt".

for each control_tareas where id_sector = p_sector and 
    control_tareas.fecha >= p_fecha_desde and
    control_tareas.fecha <= p_fecha_hasta no-lock:
    for each items_control_tareas of control_tareas where id_tarea <> 0 and
       (items_control_tareas.nro_maquina <> 0 or items_control_tareas.nro_tractor <> 0) no-lock:
      find first personal_finca where personal_finca.id_empresa_cosechera =
          items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
          if available personal_finca Then
              v_cargo = personal_finca.id_cargo.
            Else
              v_cargo = 0. 
    
       find first rb_resumen_3eros where rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa and
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = items_control_tareas.legajo and
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil and
                  rb_resumen_3eros.id_tarea = items_control_tareas.id_tarea and
                  rb_resumen_3eros.id_proveedor = items_control_tareas.id_proveedor and
                  rb_resumen_3eros.id_origen = items_control_tareas.id_origen and
                  rb_resumen_3eros.id_lote = items_control_tareas.id_lote no-lock no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa 
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil 
                  rb_resumen_3eros.id_tarea = items_control_tareas.id_tarea 
                  rb_resumen_3eros.id_proveedor = items_control_tareas.id_proveedor 
                  rb_resumen_3eros.id_origen = items_control_tareas.id_origen
                  rb_resumen_3eros.id_lote = items_control_tareas.id_lote
                  rb_resumen_3eros.nro_maquina = items_control_tareas.nro_maquina
                  rb_resumen_3eros.nro_tractor = items_control_tareas.nro_tractor.
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + items_control_tareas.cant_jornal.            

         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + items_control_tareas.cant_jornal.            

         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_3eros.horas_peon = rb_resumen_3eros.horas_peon + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_3eros.horas_trac = rb_resumen_3eros.horas_trac + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_3eros.horas_trac_cont = rb_resumen_3eros.horas_trac_cont + items_control_tareas.cant_horas.            

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           rb_resumen_3eros.horas_pulv = rb_resumen_3eros.horas_pulv + items_control_tareas.cantidad.            

         /* Horas Pulv Trac*/ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0)) Then
           rb_resumen_3eros.horas_pulv_trac = rb_resumen_3eros.horas_pulv_trac + items_control_tareas.cantidad.            
           
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + items_control_tareas.cantidad.            
         
         /* Hectareas */  
         if items_control_tareas.id_unidad_liquidacion = 13 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.cant_hectareas = rb_resumen_3eros.cant_hectareas + items_control_tareas.cantidad.            


    end.
end.

if p_archivo = "" Then p_archivo = p_carpeta + "reporte01.txt".

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
  put "Nro Maquina;".
  put "Nro Tractor;".
  put "FINCA;". 
  put "Lote;".
  put "Nombre Lote;".
  put "JS.PEON;". 
  put "JS.TRAC;". 
  put "HS.PEON;". 
  put "HS.TRAC;". 
  put "TRAC.CONT;". 
  put "HS.PULV;". 
  put "HS.PULV.TRAC;". 
  put "DESTAJO;". 
  put "HECTAREAS;". 
  put skip.
  for each rb_resumen_3eros:
     find first proveedores where proveedores.id_proveedor = rb_resumen_3eros.id_empresa no-lock no-error.
     find first sectores_agricolas where sectores_agricolas.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
     find first tareas of rb_resumen_3eros no-lock no-error.
     find first origenes of rb_resumen_3eros no-lock no-error.
     find first lotes_plantacion where lotes_plantacion.id_proveedor = rb_resumen_3eros.id_proveedor and
     lotes_plantacion.id_origen = rb_resumen_3eros.id_origen and
     lotes_plantacion.id_lote = rb_resumen_3eros.id_lote
      no-lock no-error.
     if available tareas Then
               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                if available proveedores Then proveedores.nombre Else " "
                rb_resumen_3eros.id_sector
                if available sectores_agricolas Then sectores_agricolas.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.id_tarea
                tareas.abreviatura 
                tareas.descripcion
                rb_resumen_3eros.nro_maquina
                rb_resumen_3eros.nro_tractor
                if available origenes Then origenes.descripcion Else " "
                rb_resumen_3eros.id_lote
                if available lotes_plantacion Then lotes_plantacion.descripcion Else " "
                rb_resumen_3eros.jornal_peon
                rb_resumen_3eros.jornal_trac
                rb_resumen_3eros.horas_peon
                rb_resumen_3eros.horas_trac
                rb_resumen_3eros.horas_trac_cont
                rb_resumen_3eros.horas_pulv
                rb_resumen_3eros.horas_pulv_trac
                rb_resumen_3eros.destajo
                rb_resumen_3eros.cant_hectareas.
  end.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
