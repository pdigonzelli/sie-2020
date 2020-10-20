define input parameter p_sector like control_tareas.id_sector.
define input parameter p_fecha_desde like control_tareas.fecha.
define input parameter p_fecha_hasta like control_tareas.fecha.
define input parameter p_archivo as character.
define var v_horas as decimal.
define var v_dias as integer.

 
define temp-table rb_resumen_3eros like items_control_tareas
    field jornal_peon like items_control_tareas.cant_jornal
    field jornal_trac like items_control_tareas.cant_jornal
    field jornal_ayud_capataz like items_control_tareas.cant_jornal
    field horas_peon like items_control_tareas.cant_horas
    field horas_trac like items_control_tareas.cant_horas
    field horas_trac_cont like items_control_tareas.cant_horas
    field horas_pulv like items_control_tareas.cantidad
    field horas_pulv_trac like items_control_tareas.cantidad
    field lic like items_control_tareas.cantidad
    field lluvia like items_control_tareas.cantidad
    field destajo like items_control_tareas.cantidad
    field viaje like items_control_tareas.cantidad
    field viaje_trafic like items_control_tareas.cantidad
    field cant_hectareas like items_control_tareas.cantidad
    field fecha_grab like items_control_tareas.c_fecha
    field hora_grab like items_control_tareas.c_hora
    field usuario_grab like items_control_tareas.c_usuario.

define var v_cargo as integer.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   



FOR each items_control_tareas where 
    items_control_tareas.id_sector = p_sector and 
    items_control_tareas.fecha >= p_fecha_desde and
    items_control_tareas.fecha <= p_fecha_hasta AND
    items_control_tareas.id_tipo_planilla <> 4 AND
    items_control_tareas.id_tarea <> 0  NO-LOCK
    break by items_control_tareas.id_empresa by items_control_tareas.legajo
     by items_control_tareas.nombre by items_control_tareas.dni_cuil by items_control_tareas.fecha:

      find first personal_finca where personal_finca.id_empresa_cosechera =
          items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
          if available personal_finca Then
              v_cargo = personal_finca.id_cargo.
            Else
              v_cargo = 0. 
    
       find first rb_resumen_3eros where 
                  rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa and
                  rb_resumen_3eros.id_sucursal = items_control_tareas.id_sucursal and
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = items_control_tareas.legajo and
                  rb_resumen_3eros.nombre = items_control_tareas.nombre and
                  rb_resumen_3eros.fecha = items_control_tareas.fecha no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign 
                  rb_resumen_3eros.fecha = items_control_tareas.fecha
                  rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa
                  rb_resumen_3eros.id_sucursal = items_control_tareas.id_sucursal
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil
                  rb_resumen_3eros.fecha_grab = items_control_tareas.c_fecha 
                  rb_resumen_3eros.hora_grab = items_control_tareas.c_hora 
                  rb_resumen_3eros.usuario_grab = items_control_tareas.c_usuario. 


                  
                   
              v_dias = 0.
              v_horas = 0.    
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + items_control_tareas.cant_jornal.            
             v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
           end.

          /* Js M 18 */
          IF items_control_tareas.id_unidad_liquidacion = 14 AND cant_jornal = 0 THEN
          DO:
              rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + items_control_tareas.cantidad.            
              v_horas = v_horas + (items_control_tareas.cantidad * 8).

          END.



          /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           do:
              rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + items_control_tareas.cant_jornal.            
              v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
           end.


         /* Jornal Ayud Capataz */
         if items_control_tareas.id_unidad_liquidacion = 11  Then
           do:
             rb_resumen_3eros.jornal_ayud_capataz = rb_resumen_3eros.jornal_ayud_capataz + items_control_tareas.cantidad.            
             v_horas = v_horas + (items_control_tareas.cantidad * 8).
           end.


         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.horas_peon = rb_resumen_3eros.horas_peon + items_control_tareas.cant_horas.            
             v_horas = v_horas + items_control_tareas.cant_horas.
           end.

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           do: 
             rb_resumen_3eros.horas_trac = rb_resumen_3eros.horas_trac + items_control_tareas.cant_horas.            
             v_horas = v_horas + items_control_tareas.cant_horas.
           end.

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           do:
             rb_resumen_3eros.horas_trac_cont = rb_resumen_3eros.horas_trac_cont + items_control_tareas.cant_horas.            
             v_horas = v_horas + items_control_tareas.cant_horas.
           end.

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv = rb_resumen_3eros.horas_pulv + items_control_tareas.cantidad.            
              v_horas = v_horas + items_control_tareas.cant_horas.
           end.

         /* Horas Pulv Trac*/ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv_trac = rb_resumen_3eros.horas_pulv_trac + items_control_tareas.cantidad.            
              v_horas = v_horas + items_control_tareas.cantidad.
           end.
           
         /* Licencias */
         
         find first tareas of items_control_tareas no-lock no-error.
         if available tareas Then
            do:
             if  tareas.id_grupo_tarea = 13 and items_control_tareas.id_tarea <> 117 Then
                do:
                  rb_resumen_3eros.lic = rb_resumen_3eros.lic + items_control_tareas.cantidad.            
                  if tareas.decreto Then
                      v_horas = 8.
                end.  
            end.
            
   
         /* LLuvia */  
         if items_control_tareas.id_tarea = 117 Then
           do:
           rb_resumen_3eros.lluvia = rb_resumen_3eros.lluvia + items_control_tareas.cantidad.            
           end.
         
           
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:         
             rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + items_control_tareas.cantidad.            
             v_horas = v_horas + 8.
           end.  
         
         
         /* Viaje Colectivo */  
         if items_control_tareas.id_unidad_liquidacion = 12 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.viaje = rb_resumen_3eros.viaje + items_control_tareas.cantidad.            
             v_horas = v_horas + (items_control_tareas.cantidad * 8).
           end.
         
           /* Viaje Trafic */  
           if items_control_tareas.id_unidad_liquidacion = 43 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
             do:
               rb_resumen_3eros.viaje_trafic = rb_resumen_3eros.viaje_trafic + items_control_tareas.cantidad.            
               v_horas = v_horas + (items_control_tareas.cantidad * 8).
             end.
         
             /* Hectareas */  
         if items_control_tareas.id_unidad_liquidacion = 13 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.cant_hectareas = rb_resumen_3eros.cant_hectareas + items_control_tareas.cantidad.            
             v_horas = v_horas + (items_control_tareas.cantidad * 8).
           
    end.
END.

if p_archivo = "" Then p_archivo = "z:\temp\repfecha01.txt".

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  put "Sucursal;".
  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Sector;". 
  put "Nombre Sector;".
  put "Legajo;". 
  put "Nombre;". 
  put "DNI/CUIL;". 
  put "Fecha;".
  put "JS.PEON;". 
  put "JS.TRAC;". 
  put "JS.AYUD CAP;". 
  put "HS.PEON;". 
  put "HS.TRAC;". 
  put "TRAC.CONT;". 
  put "HS.PULV;". 
  put "HS.PULV.TRAC;". 
  put "LIC;". 
  put "LLUVIA;".
  put "DESTAJO;". 
  put "VIAJE COL.;".
  put "VIAJE TRAFIC;".
  put "HECTAREAS.;".
  put "Fecha grab;".
  put "Hora grab;".
  put "Usuario grab;".
  
  put skip.
  for each rb_resumen_3eros:
     find first proveedores where proveedores.id_proveedor = rb_resumen_3eros.id_empresa no-lock no-error.
     find first sectores_agricolas where sectores_agricolas.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
     find first sucursales where sucursales.id_sucursal = rb_resumen_3eros.id_sucursal no-lock no-error.
               export delimiter ";" 
                if available sucursales Then sucursales.nombre Else " "                 
                rb_resumen_3eros.id_empresa
                if available proveedores Then proveedores.nombre Else " "
                rb_resumen_3eros.id_sector
                if available sectores_agricolas Then sectores_agricolas.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.fecha
                rb_resumen_3eros.jornal_peon
                rb_resumen_3eros.jornal_trac
                rb_resumen_3eros.jornal_ayud_capataz
                rb_resumen_3eros.horas_peon
                rb_resumen_3eros.horas_trac
                rb_resumen_3eros.horas_trac_cont
                rb_resumen_3eros.horas_pulv
                rb_resumen_3eros.horas_pulv_trac
                rb_resumen_3eros.lic
                rb_resumen_3eros.lluvia
                rb_resumen_3eros.destajo
                rb_resumen_3eros.viaje
                rb_resumen_3eros.viaje_trafic
                rb_resumen_3eros.fecha_grab
                rb_resumen_3eros.hora_grab
                rb_resumen_3eros.usuario_grab.
  end.
output close.

message "Archivo gral generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
