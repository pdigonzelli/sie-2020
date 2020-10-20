define input parameter p_sector like control_tareas.id_sector.
define input parameter p_fecha_desde like control_tareas.fecha.
define input parameter p_fecha_hasta like control_tareas.fecha.
define input parameter p_archivo as character.
define var v_horas as decimal.
define var v_dias as integer.

 
define temp-table rb_resumen_3eros like items_control_tareas
    field jornal_peon like items_control_tareas.cant_jornal_norm
    field jornal_trac like items_control_tareas.cant_jornal_norm
    field jornal_ayud_capataz like items_control_tareas.cant_jornal_norm
    field horas_peon-1 like items_control_tareas.cant_hs_norm
    field horas_peon-2 like items_control_tareas.cant_hs_comp
    field horas_peon-3 like items_control_tareas.cant_hs_extras
    field horas_trac-1 like items_control_tareas.cant_hs_norm
    field horas_trac-2 like items_control_tareas.cant_hs_comp
    field horas_trac-3 like items_control_tareas.cant_hs_extras
    field horas_trac_cont-1 like items_control_tareas.cant_hs_norm
    field horas_trac_cont-2 like items_control_tareas.cant_hs_comp
    field horas_trac_cont-3 like items_control_tareas.cant_hs_extras
    field horas_pulv like items_control_tareas.cantidad
    field horas_pulv_trac like items_control_tareas.cantidad
    field lic like items_control_tareas.cantidad
    field lluvia like items_control_tareas.cantidad
    field destajo like items_control_tareas.cantidad
    field viaje like items_control_tareas.cantidad
    field viaje_trafic like items_control_tareas.cantidad
    field cant_hectareas like items_control_tareas.cantidad
    FIELD cant_tareasc LIKE items_control_tareas.cantidad
    FIELD cant_aplic LIKE items_control_tareas.cantidad
    field cant_dias as integer
    field cant_total_horas as decimal
    field dias_cal as decimal.

define var v_cargo as integer.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   



for each control_tareas where id_sector = p_sector and 
    control_tareas.fecha >= p_fecha_desde and
    control_tareas.fecha <= p_fecha_hasta and
    control_tareas.id_tipo_planilla <> 4 no-lock,
    each items_control_tareas of control_tareas where id_tarea <> 0 no-lock
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
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = items_control_tareas.legajo and
                  rb_resumen_3eros.nombre = items_control_tareas.nombre and
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil  no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa 
                  rb_resumen_3eros.id_sector = items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil. 
              v_dias = 0.
              v_horas = 0.    
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + items_control_tareas.cant_jornal_norm.            
             v_horas = v_horas + (items_control_tareas.cant_jornal_norm * 8).
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
              rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + items_control_tareas.cant_jornal_norm.            
              v_horas = v_horas + (items_control_tareas.cant_jornal_norm * 8).
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
             rb_resumen_3eros.horas_peon-1 = rb_resumen_3eros.horas_peon-1 + items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_peon-2 = rb_resumen_3eros.horas_peon-2 + items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_peon-3 = rb_resumen_3eros.horas_peon-3 + items_control_tareas.cant_hs_extras.            
             v_horas = v_horas + (items_control_tareas.cant_hs_norm + items_control_tareas.cant_hs_compensa + items_control_tareas.cant_hs_extras).
           end.

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           do: 
             rb_resumen_3eros.horas_trac-1 = rb_resumen_3eros.horas_trac-1 + items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_trac-2 = rb_resumen_3eros.horas_trac-2 + items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_trac-3 = rb_resumen_3eros.horas_trac-3 + items_control_tareas.cant_hs_extras.            
             v_horas = v_horas + (items_control_tareas.cant_hs_norm + items_control_tareas.cant_hs_compensa + items_control_tareas.cant_hs_extras).
           end.

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           do:
             rb_resumen_3eros.horas_trac_cont-1 = rb_resumen_3eros.horas_trac_cont-1 + items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_trac_cont-2 = rb_resumen_3eros.horas_trac_cont-2 + items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_trac_cont-3 = rb_resumen_3eros.horas_trac_cont-3 + items_control_tareas.cant_hs_extras.            

             v_horas = v_horas + (items_control_tareas.cant_hs_norm + items_control_tareas.cant_hs_compensa + items_control_tareas.cant_hs_extras).
           end.

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv = rb_resumen_3eros.horas_pulv + items_control_tareas.cantidad.            
              v_horas = v_horas + items_control_tareas.cantidad.
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

         if last-of(items_control_tareas.fecha) Then
            do:
              /* Comentado el 01/03/2004  
                 debido a que se puede trabajar 1/2 jornal y luego
                 registrar lluvia por ejemplo Juarez de Tipa de 
                 la 1a quincena de enero 
                 
                 if  (items_control_tareas.id_tarea <> 114 and 
                    items_control_tareas.id_tarea <> 115 and
                    items_control_tareas.id_tarea <> 117 and
                    items_control_tareas.id_tarea <> 118) Then
                    do:*/
                        
                        if v_horas > 0 Then
                        do:
                            v_dias = v_dias + 1.
                            if v_horas > 8 Then  v_horas = 8.
                            
                            if items_control_tareas.id_tarea <> 116 Then
                            rb_resumen_3eros.cant_total_horas = rb_resumen_3eros.cant_total_horas + v_horas.
                        end.
                        
                /*    end.*/
                v_horas = 0.    
            end.
           
           rb_resumen_3eros.cant_dias = v_dias.
           
           if last-of(items_control_tareas.dni_cuil) Then
              do:
               rb_resumen_3eros.dias_cal = round(rb_resumen_3eros.cant_total_horas / 8, 2).
              end. 
    end.

if p_archivo = "" Then p_archivo = "z:\temp\repgral01.txt".

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
  put "JS.PEON;". 
  put "JS.TRAC;". 
  put "JS.AYUD CAP;". 
  put "HS.PEON-NORM;". 
  put "HS.PEON-COMP;". 
  put "HS.PEON-EXTRAS;". 
  put "HS.TRAC-NORM;". 
  put "HS.TRAC-COMP;". 
  put "HS.TRAC-EXTRAS;". 
  put "HS.TRAC.CONT-NORM;". 
  put "HS.TRAC.CONT-COMP;". 
  put "HS.TRAC.CONT-EXTRAS;". 
  put "HS.PULV;". 
  put "HS.PULV.TRAC;". 
  put "LIC;". 
  put "LLUVIA;".
  put "DESTAJO;". 
  put "VIAJE COL.;".
  put "VIAJE TRAFIC.;".
  put "HECTAREAS.;".
  put "DIAS TRAB.;".
  put "DIAS CAL.;".
  put skip.
  for each rb_resumen_3eros:
     find first proveedores where proveedores.id_proveedor = rb_resumen_3eros.id_empresa no-lock no-error.
     find first sectores_agricolas where sectores_agricolas.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                if available proveedores Then proveedores.nombre Else " "
                rb_resumen_3eros.id_sector
                if available sectores_agricolas Then sectores_agricolas.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.jornal_peon
                rb_resumen_3eros.jornal_trac
                rb_resumen_3eros.jornal_ayud_capataz
                rb_resumen_3eros.horas_peon-1
                rb_resumen_3eros.horas_peon-2
                rb_resumen_3eros.horas_peon-3
                rb_resumen_3eros.horas_trac-1
                rb_resumen_3eros.horas_trac-2
                rb_resumen_3eros.horas_trac-3
                rb_resumen_3eros.horas_trac_cont-1
                rb_resumen_3eros.horas_trac_cont-2
                rb_resumen_3eros.horas_trac_cont-3
                rb_resumen_3eros.horas_pulv
                rb_resumen_3eros.horas_pulv_trac
                rb_resumen_3eros.lic
                rb_resumen_3eros.lluvia
                rb_resumen_3eros.destajo
                rb_resumen_3eros.viaje
                rb_resumen_3eros.viaje_trafic
                rb_resumen_3eros.cant_hectareas
                rb_resumen_3eros.cant_dias
                rb_resumen_3eros.dias_cal.
  end.
output close.

message "Archivo gral generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
