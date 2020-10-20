define input parameter p_sector like liq_control_tareas.id_sector.
define input parameter p_fecha_desde like liq_control_tareas.fecha.
define input parameter p_fecha_hasta like liq_control_tareas.fecha.
define input parameter p_archivo as character.
define var v_horas as decimal.
define var v_dias as integer.

 
define temp-table rb_resumen_3eros like liq_items_control_tareas
    FIELD tipo_liquidacion LIKE liq_legajos.tipo_liquidacion
    field jornal_peon like liq_items_control_tareas.cant_jornal_norm
    field jornal_trac like liq_items_control_tareas.cant_jornal_norm
    field jornal_ayud_capataz like liq_items_control_tareas.cant_jornal_norm
    field horas_peon-1 like liq_items_control_tareas.cant_hs_norm
    field horas_peon-2 like liq_items_control_tareas.cant_hs_comp
    field horas_peon-3 like liq_items_control_tareas.cant_hs_extras
    field horas_trac-1 like liq_items_control_tareas.cant_hs_norm
    field horas_trac-2 like liq_items_control_tareas.cant_hs_comp
    field horas_trac-3 like liq_items_control_tareas.cant_hs_extras
    field horas_trac_cont-1 like liq_items_control_tareas.cant_hs_norm
    field horas_trac_cont-2 like liq_items_control_tareas.cant_hs_comp
    field horas_trac_cont-3 like liq_items_control_tareas.cant_hs_extras
    field horas_pulv like liq_items_control_tareas.cantidad
    field horas_pulv_trac like liq_items_control_tareas.cantidad
    field horas_taller like liq_items_control_tareas.cantidad
    field lic like liq_items_control_tareas.cantidad
    field lluvia like liq_items_control_tareas.cantidad
    field destajo like liq_items_control_tareas.cantidad
    field viaje like liq_items_control_tareas.cantidad
    field viaje_trafic like liq_items_control_tareas.cantidad
    field cant_hectareas like liq_items_control_tareas.cantidad
    FIELD cant_tareasc LIKE liq_items_control_tareas.cantidad
    FIELD cant_aplic LIKE liq_items_control_tareas.cantidad
    field cant_dias as integer
    field cant_total_horas as decimal
    field dias_cal as decimal
    FIELD sab_trab AS INTEGER.

define var v_cargo as integer.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   



for each liq_control_tareas where id_sector = p_sector and 
    liq_control_tareas.fecha >= p_fecha_desde and
    liq_control_tareas.fecha <= p_fecha_hasta and
    liq_control_tareas.id_tipo_planilla <> 4 no-lock,
    each liq_items_control_tareas of liq_control_tareas where id_tarea <> 0 no-lock
    break by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo
     by liq_items_control_tareas.nombre by liq_items_control_tareas.dni_cuil by liq_items_control_tareas.fecha:
      
    find first liq_legajos where liq_legajos.id_empresa_liq =
          liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo no-lock no-error.
          if available liq_legajos Then
              v_cargo = liq_legajos.id_cargo.
            Else
              v_cargo = 0. 
    
       find first rb_resumen_3eros where 
                  rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa and
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo and
                  rb_resumen_3eros.nombre = liq_items_control_tareas.nombre and
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil  no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa 
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = liq_items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil
                  rb_resumen_3eros.tipo_liquidacion = liq_legajos.tipo_liquidacion. 
              v_dias = 0.
              v_horas = 0.    
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + liq_items_control_tareas.cant_jornal_norm.            
             v_horas = v_horas + (liq_items_control_tareas.cant_jornal_norm * 8).
           end.


         /* Js M 18 */
          IF liq_items_control_tareas.id_unidad_liquidacion = 14 AND cant_jornal = 0 THEN
          DO:
              rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).

          END.


         /* Jornal Trac */
         if (v_cargo = 42 or liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) Then
           do:
              rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + liq_items_control_tareas.cant_jornal_norm.            
              v_horas = v_horas + (liq_items_control_tareas.cant_jornal_norm * 8).
           end.


         /* Jornal Ayud Capataz */
         if liq_items_control_tareas.id_unidad_liquidacion = 11  Then
           do:
             rb_resumen_3eros.jornal_ayud_capataz = rb_resumen_3eros.jornal_ayud_capataz + liq_items_control_tareas.cantidad.            
             v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
           end.


         /* Horas Peon */
         if (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.horas_peon-1 = rb_resumen_3eros.horas_peon-1 + liq_items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_peon-2 = rb_resumen_3eros.horas_peon-2 + liq_items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_peon-3 = rb_resumen_3eros.horas_peon-3 + liq_items_control_tareas.cant_hs_extras.            
             v_horas = v_horas + (liq_items_control_tareas.cant_hs_norm + liq_items_control_tareas.cant_hs_compensa + liq_items_control_tareas.cant_hs_extras).
           end.

         /* Horas Trac */
         if (v_cargo = 42 or (liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) and 
            liq_items_control_tareas.nro_tractor <> 99999) Then
           do: 
             rb_resumen_3eros.horas_trac-1 = rb_resumen_3eros.horas_trac-1 + liq_items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_trac-2 = rb_resumen_3eros.horas_trac-2 + liq_items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_trac-3 = rb_resumen_3eros.horas_trac-3 + liq_items_control_tareas.cant_hs_extras.            
             v_horas = v_horas + (liq_items_control_tareas.cant_hs_norm + liq_items_control_tareas.cant_hs_compensa + liq_items_control_tareas.cant_hs_extras).
           end.

         /* Trac Cont*/
         if (liq_items_control_tareas.nro_tractor = 99999) Then
           do:
             rb_resumen_3eros.horas_trac_cont-1 = rb_resumen_3eros.horas_trac_cont-1 + liq_items_control_tareas.cant_hs_norm.            
             rb_resumen_3eros.horas_trac_cont-2 = rb_resumen_3eros.horas_trac_cont-2 + liq_items_control_tareas.cant_hs_compensa.            
             rb_resumen_3eros.horas_trac_cont-3 = rb_resumen_3eros.horas_trac_cont-3 + liq_items_control_tareas.cant_hs_extras.            

             v_horas = v_horas + (liq_items_control_tareas.cant_hs_norm + liq_items_control_tareas.cant_hs_compensa + liq_items_control_tareas.cant_hs_extras).
           end.

         /* Horas Pulverizacion */ 
          if (liq_items_control_tareas.id_unidad_liquidacion = 7 and 
             (liq_items_control_tareas.id_tarea < 107 or  liq_items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv = rb_resumen_3eros.horas_pulv + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + liq_items_control_tareas.cantidad.
           end.

         /* Horas Pulv Trac*/ 
          if (liq_items_control_tareas.id_unidad_liquidacion = 7 and 
             (liq_items_control_tareas.id_tarea < 107 or  liq_items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv_trac = rb_resumen_3eros.horas_pulv_trac + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + liq_items_control_tareas.cantidad.
           end.
           
         /* Horas Taller*/ 
          if (liq_items_control_tareas.id_unidad_liquidacion = 46 and 
             (liq_items_control_tareas.id_tarea < 107 or  liq_items_control_tareas.id_tarea  > 118)) Then
           do:  
              rb_resumen_3eros.horas_taller = rb_resumen_3eros.horas_taller + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + liq_items_control_tareas.cantidad.
           end.
         
         /* Licencias */
         
         find first tareas of liq_items_control_tareas no-lock no-error.
         if available tareas Then
            do:
             if  tareas.id_grupo_tarea = 13 and liq_items_control_tareas.id_tarea <> 117 Then
                do:
                  rb_resumen_3eros.lic = rb_resumen_3eros.lic + liq_items_control_tareas.cantidad.            
                  if tareas.decreto Then
                      v_horas = 8.
                end.  
            end.
            
   
         /* LLuvia */  
         if liq_items_control_tareas.id_tarea = 117 Then
           do:
           rb_resumen_3eros.lluvia = rb_resumen_3eros.lluvia + liq_items_control_tareas.cantidad.            
           end.
         
           
         /* Destajo */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 4 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:         
             rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + liq_items_control_tareas.cantidad.            
             v_horas = v_horas + 8.
           end.  
         
         
         /* Viaje Colectivo */  
         if liq_items_control_tareas.id_unidad_liquidacion = 12 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.viaje = rb_resumen_3eros.viaje + liq_items_control_tareas.cantidad.            
             v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
           end.
         
           /* Viaje Trafic */  
           if liq_items_control_tareas.id_unidad_liquidacion = 43 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
             do:
               rb_resumen_3eros.viaje_trafic = rb_resumen_3eros.viaje_trafic + liq_items_control_tareas.cantidad.            
               v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
             end.
         
         /* Hectareas */  
         if liq_items_control_tareas.id_unidad_liquidacion = 13 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.cant_hectareas = rb_resumen_3eros.cant_hectareas + liq_items_control_tareas.cantidad.            
             v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
           end.

         if last-of(liq_items_control_tareas.fecha) Then
            do:
              /* Comentado el 01/03/2004  
                 debido a que se puede trabajar 1/2 jornal y luego
                 registrar lluvia por ejemplo Juarez de Tipa de 
                 la 1a quincena de enero 
                 
                 if  (items_control_tareas.id_tarea <> 114 and 
                    liq_items_control_tareas.id_tarea <> 115 and
                    liq_items_control_tareas.id_tarea <> 117 and
                    liq_items_control_tareas.id_tarea <> 118) Then
                    do:*/
                        
                        if v_horas > 0 Then
                        do:
                            v_dias = v_dias + 1.
                            if v_horas > 8 Then  v_horas = 8.
                            
                            if liq_items_control_tareas.id_tarea <> 116 Then
                            DO:
                                rb_resumen_3eros.cant_total_horas = rb_resumen_3eros.cant_total_horas + v_horas.
    
                                IF weekday(liq_items_control_tareas.fecha) = 7 THEN
                                rb_resumen_3eros.sab_trab = rb_resumen_3eros.sab_trab + 1.
                            END.
                        end.
                        
                /*    end.*/
                v_horas = 0.    
            end.
           
           rb_resumen_3eros.cant_dias = v_dias.
           
           if last-of(liq_items_control_tareas.dni_cuil) Then
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
  PUT "Tipo Liq;".
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
  put "HS.TALLER;". 
  put "LIC;". 
  put "LLUVIA;".
  put "DESTAJO;". 
  put "VIAJE COL.;".
  put "VIAJE TRAFIC.;".
  put "HECTAREAS.;".
  put "DIAS TRAB.;".
  put "DIAS CAL.;".
  PUT "SAB TRAB.;".
  put skip.
  for each rb_resumen_3eros:
     find first liq_empresas where liq_empresas.id_empresa_liq = rb_resumen_3eros.id_empresa no-lock no-error.
     find first liq_sectores where liq_sectores.id_empresa_liq = rb_resumen_3eros.id_empresa AND
         liq_sectores.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                if available liq_empresas Then liq_empresas.descripcion Else " "
                rb_resumen_3eros.id_sector
                if available liq_sectores Then liq_sectores.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.tipo_liquidacion
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
                rb_resumen_3eros.horas_taller
                rb_resumen_3eros.lic
                rb_resumen_3eros.lluvia
                rb_resumen_3eros.destajo
                rb_resumen_3eros.viaje
                rb_resumen_3eros.viaje_trafic
                rb_resumen_3eros.cant_hectareas
                rb_resumen_3eros.cant_dias
                rb_resumen_3eros.dias_cal
                rb_resumen_3eros.sab_trab.
  end.
output close.

message "Archivo gral generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
