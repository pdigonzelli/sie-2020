DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_sector AS INTEGER.
DEF INPUT PARAMETER p_fecha_desde AS DATE.
DEF INPUT PARAMETER p_fecha_hasta AS DATE.
define input parameter v_feriado as date.

define var p_archivo as character.

define var v_horas as decimal.
define var v_dias as integer.
define var v_dias-1 as integer.
define var v_dias-2 as integer.
define var v_dias_feriados as integer.
define var v_dias_enfermedad as integer.
define var v_dias_vacaciones as integer.
define var v_dias_otraslic as integer.
define var v_total as decimal.

define var v_dias_feriados-1 as integer.
define var v_dias_enfermedad-1 as integer.
define var v_dias_vacaciones-1 as integer.
define var v_dias_otraslic-1 as integer.
define var v_total-1 as decimal.

define var v_dias_feriados-2 as integer.
define var v_dias_enfermedad-2 as integer.
define var v_dias_vacaciones-2 as integer.
define var v_dias_otraslic-2 as integer.
define var v_total-2 as decimal.


define var v_feriado_cond-a as integer.
define var v_feriado_cond-b as integer.

define var v_feriado_cond-a-1 as integer.
define var v_feriado_cond-b-1 as integer.
define var v_feriado_cond-a-2 as integer.
define var v_feriado_cond-b-2 as integer.

 
define temp-table rb_resumen_3eros like liq_items_control_tareas
    field jornal_peon like liq_items_control_tareas.cant_jornal
    field jornal_trac like liq_items_control_tareas.cant_jornal
    field jornal_ayud_capataz like liq_items_control_tareas.cant_jornal
    field horas_peon like liq_items_control_tareas.cant_horas
    field horas_trac like liq_items_control_tareas.cant_horas
    field horas_trac_cont like liq_items_control_tareas.cant_horas
    field horas_tractoelev like liq_items_control_tareas.cantidad
    field plus_x_bins like liq_items_control_tareas.cantidad
    field viatico_peon like liq_items_control_tareas.cantidad
    field viatico_ayud like liq_items_control_tareas.cantidad
    field horas_pulv like liq_items_control_tareas.cantidad
    field horas_pulv_trac like liq_items_control_tareas.cantidad
    field lic like liq_items_control_tareas.cantidad
    field lluvia like liq_items_control_tareas.cantidad
    field destajo like liq_items_control_tareas.cantidad
    field viaje like liq_items_control_tareas.cantidad
    field cant_hectareas like liq_items_control_tareas.cantidad
    field cant_dias as integer
    field cant_dias_feriado as integer
    field cant_feriado_cond-a as integer
    field cant_feriado_cond-b as integer
    field cant_dias_enfermedad as integer
    field cant_dias_vacaciones as integer
    field cant_dias_otraslic as integer
    field cant_dias_feriado-1 as integer
    field cant_feriado_cond-a-1 as integer
    field cant_feriado_cond-b-1 as integer
    field cant_dias_enfermedad-1 as integer
    field cant_dias_vacaciones-1 as integer
    field cant_dias_otraslic-1 as integer
    field cant_dias_feriado-2 as integer
    field cant_feriado_cond-a-2 as integer
    field cant_feriado_cond-b-2 as integer
    field cant_dias_enfermedad-2 as integer
    field cant_dias_vacaciones-2 as integer
    field cant_dias_otraslic-2 as integer
    field cant_total_horas as decimal
    field cant_total_horas-1 as decimal
    field cant_total_horas-2 as decimal
    field dias_cal as decimal
    field dias_cal-1 as decimal
    field dias_cal-2 as decimal.

define var v_cargo as integer.
define var v_estado as logical.
DEF BUFFER b_ult FOR liq_control_tareas.
DEF VAR v-nro AS INTEGER.


DEF TEMP-TABLE t-lluvia
    FIELD fecha AS DATE.

FOR EACH t-lluvia:
    DELETE t-lluvia.
END.

FOR EACH liq_items_control_tareas WHERE liq_items_control_tareas.fecha >= p_fecha_desde AND
    liq_items_control_tareas.fecha <= p_fecha_hasta AND
    id_tarea = 117 NO-LOCK, FIRST liq_control_tareas OF liq_items_control_tareas NO-LOCK
     BREAK BY liq_items_control_tareas.fecha:
    IF LAST-OF(liq_items_control_tareas.fecha) THEN
    DO:
        CREATE t-lluvia.
        ASSIGN t-lluvia.fecha = liq_items_control_tareas.fecha.
    END.
END.




for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   


p_archivo = "z:\temp\jorpers.txt".


for each liq_control_tareas where 
    liq_control_tareas.id_empresa = p_empresa and
    liq_control_tareas.id_sector = p_sector and
    liq_control_tareas.fecha >= p_fecha_desde and
    liq_control_tareas.fecha <= p_fecha_hasta no-lock,
    each liq_items_control_tareas of liq_control_tareas where id_tarea <> 0 and
         liq_items_control_tareas.id_tarea <> 113 and /* Feriado */
         liq_items_control_tareas.id_tarea <> 654 AND
         liq_items_control_tareas.id_tarea <> 655 AND
         liq_items_control_tareas.dni_cuil <> "" no-lock
    break by liq_items_control_tareas.id_empresa by 
    liq_items_control_tareas.dni_cuil by liq_items_control_tareas.fecha:
      
    
       find first rb_resumen_3eros where 
                  rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa and
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo  no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa 
                    rb_resumen_3eros.id_sucursal = liq_items_control_tareas.id_sucursal
                    rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector 
                    rb_resumen_3eros.legajo = liq_items_control_tareas.legajo 
                    rb_resumen_3eros.nombre = liq_items_control_tareas.nombre. 
                    rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil. 
              v_dias = 0.
              v_dias_feriados = 0.
              v_feriado_cond-a = 0.
              v_feriado_cond-b = 0.

                 v_dias_feriados-1 = 0.
                 v_feriado_cond-a-1 = 0.
                 v_feriado_cond-b-1 = 0.
                 v_dias_feriados-2 = 0.
                 v_feriado_cond-a-2 = 0.
                 v_feriado_cond-b-2 = 0.
                 v_dias_enfermedad-1 = 0.
                 v_dias_vacaciones-1 = 0.
                 v_dias_otraslic-1 = 0.
                 v_dias_enfermedad-2 = 0.
                 v_dias_vacaciones-2 = 0.
                 v_dias_otraslic-2 = 0.


              v_horas = 0.    
          end.
        
         /* Jornal Peon */
         if (v_cargo <> 42 and v_cargo <> 32 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           do:
           rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + liq_items_control_tareas.cant_jornal.            
           v_horas = v_horas + (liq_items_control_tareas.cant_jornal * 8).
           end.


         /* Jornal Trac */
         if (v_cargo = 42 or liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) Then
           do:
              rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + liq_items_control_tareas.cant_jornal.            
              v_horas = v_horas + (liq_items_control_tareas.cant_jornal * 8).
           end.


         /* Jornal Ayud Capataz */

         if (v_cargo = 32 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           do:
           rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + liq_items_control_tareas.cant_jornal.            
           v_horas = v_horas + (liq_items_control_tareas.cant_jornal * 8).
           end.


          if liq_items_control_tareas.id_unidad_liquidacion = 11 Then
           do:
             rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + liq_items_control_tareas.cantidad.          
             v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
           end.  


         /* Horas Peon */
         if (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0) Then
           do:
             rb_resumen_3eros.horas_peon = rb_resumen_3eros.horas_peon + liq_items_control_tareas.cant_horas.            
             v_horas = v_horas + liq_items_control_tareas.cant_horas.
           end.

         /* Horas Trac */
         if (v_cargo = 42 or (liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) and 
            liq_items_control_tareas.nro_tractor <> 99999) Then
           do: 
             rb_resumen_3eros.horas_trac = rb_resumen_3eros.horas_trac + liq_items_control_tareas.cant_horas.            
             v_horas = v_horas + liq_items_control_tareas.cant_horas.
           end.

         /* Trac Cont*/
         if (liq_items_control_tareas.nro_tractor = 99999) Then
           do:
             rb_resumen_3eros.horas_trac_cont = rb_resumen_3eros.horas_trac_cont + liq_items_control_tareas.cant_horas.            
             v_horas = v_horas + liq_items_control_tareas.cant_horas.
           end.

         /* Horas Pulverizacion */ 
          if (liq_items_control_tareas.id_unidad_liquidacion = 7 and 
             (liq_items_control_tareas.id_tarea < 107 or  liq_items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and liq_items_control_tareas.nro_tractor = 0 and liq_items_control_tareas.nro_maquina = 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv = rb_resumen_3eros.horas_pulv + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + liq_items_control_tareas.cant_horas.
           end.

         /* Horas Pulv Trac*/ 
          if (liq_items_control_tareas.id_unidad_liquidacion = 7 and 
             (liq_items_control_tareas.id_tarea < 107 or  liq_items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0)) Then
           do:  
              rb_resumen_3eros.horas_pulv_trac = rb_resumen_3eros.horas_pulv_trac + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + liq_items_control_tareas.cantidad.
           end.
          
          
         /* Destajo */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 4 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:
              rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + liq_items_control_tareas.cantidad.            
              v_horas = v_horas + 8.
           end.

         /* Hs.Tractoelev */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 17 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:
           rb_resumen_3eros.horas_tractoelev = rb_resumen_3eros.horas_tractoelev + liq_items_control_tareas.cantidad.            
           v_horas = v_horas + liq_items_control_tareas.cantidad.
           end.

         /* Plus x Bins */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 16 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.plus_x_bins = rb_resumen_3eros.plus_x_bins + liq_items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 8 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_peon = rb_resumen_3eros.viatico_peon + liq_items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if liq_items_control_tareas.id_unidad_liquidacion = 9 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_ayud = rb_resumen_3eros.viatico_ayud + liq_items_control_tareas.cantidad.            
          
   
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
         
         /* Hectareas */  
         if liq_items_control_tareas.id_unidad_liquidacion = 13 and (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.cant_hectareas = rb_resumen_3eros.cant_hectareas + liq_items_control_tareas.cantidad.            
             v_horas = v_horas + (liq_items_control_tareas.cantidad * 8).
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


        
         if last-of(liq_items_control_tareas.fecha) Then
            do:
                 if v_horas > 0 Then
                    do:
                      if liq_items_control_tareas.fecha = v_feriado 
                         and liq_items_control_tareas.id_tarea <> 107 
                         and liq_items_control_tareas.id_tarea <> 116  Then
                              do:
                                v_dias_feriados = v_dias_feriados + 1.
                                if day(liq_items_control_tareas.fecha) <= 15 Then
                                     v_dias_feriados-1 = v_dias_feriados-1 + 1.
                                    Else 
                                     v_dias_feriados-2 = v_dias_feriados-2 + 1.
                              end.  
                              Else 
                              do:
                                case liq_items_control_tareas.id_tarea:
                                 when 107 Then /* Enfermedades */
                                  do:
                                   v_dias_enfermedad = v_dias_enfermedad + 1.
                                   if day(liq_items_control_tareas.fecha) <= 15 Then
                                      v_dias_enfermedad-1 = v_dias_enfermedad-1 + 1.
                                     Else
                                      v_dias_enfermedad-2 = v_dias_enfermedad-2 + 1.
                                  end. 
                                 when 116 Then /* Vacaciones */
                                  do:
                                   v_dias_vacaciones = v_dias_vacaciones + 1.
                                   if day(liq_items_control_tareas.fecha) <= 15 Then
                                      v_dias_vacaciones-1 = v_dias_vacaciones-1 + 1.
                                     Else
                                      v_dias_vacaciones-2 = v_dias_vacaciones-2 + 1.
                                  end. 
                                 when 108 or when 109 or when 110 /* Otras lic*/ 
                                          or when 111 or when 112
                                          or when 184 or when 185 Then
                                  do:
                                   v_dias_otraslic = v_dias_otraslic + 1.
                                   if day(liq_items_control_tareas.fecha) <= 15 Then
                                      v_dias_otraslic-1 = v_dias_otraslic-1 + 1.
                                          Else
                                      v_dias_otraslic-2 = v_dias_otraslic-2 + 1.
                                  end.           
                                 otherwise
                                 do:
                                   v_dias = v_dias + 1.
                                       
                                   if v_horas > 8 Then  v_horas = 8.
                                    rb_resumen_3eros.cant_total_horas = rb_resumen_3eros.cant_total_horas + v_horas.
                                   
                                   if day(liq_items_control_tareas.fecha) <= 15 Then
                                          rb_resumen_3eros.cant_total_horas-1 = rb_resumen_3eros.cant_total_horas-1 + v_horas.
                                      Else
                                          rb_resumen_3eros.cant_total_horas-2 = rb_resumen_3eros.cant_total_horas-2 + v_horas.
 
                                 end.  
                                end. 
                              end.  
                                
                    end.
                v_horas = 0.    
            end.
           
           if last-of(liq_items_control_tareas.dni_cuil) Then
              do:
                 if v_dias_feriados = 0 Then
                    do: 
                        run ver-condicion-periodo-anterior  (input liq_control_tareas.id_empresa,
                                                             input liq_items_control_tareas.dni_cuil,
                                                             output v_estado).
                         if v_estado Then
                           do:
                             v_feriado_cond-a = v_feriado_cond-a + 1.                                  
                             if day(v_feriado) <= 15 Then
                                v_feriado_cond-a-1 = v_feriado_cond-a-1 + 1.                                  
                               Else
                                v_feriado_cond-a-2 = v_feriado_cond-a-2 + 1.                                  
                                
       
                           end. 
                    end.
                            
                 if v_dias_feriados = 0 and v_feriado_cond-a = 0 Then
                    do:
                        run ver-condicion-periodo-siguiente (input liq_control_tareas.id_empresa,
                                                             input liq_items_control_tareas.dni_cuil,
                                                             output v_estado).
                        if v_estado Then
                          do:
                           v_feriado_cond-b = v_feriado_cond-b + 1.                                  
                           if day(v_feriado) <= 15 Then
                              v_feriado_cond-b-1 = v_feriado_cond-b-1 + 1.                                  
                             Else 
                              v_feriado_cond-b-2 = v_feriado_cond-b-2 + 1.                                  
                          end. 
                    end.                                    
                            
               /**********************************************/
                 
                 
                 rb_resumen_3eros.cant_dias = v_dias.
                 rb_resumen_3eros.cant_dias_feriado = v_dias_feriados.
                 rb_resumen_3eros.cant_feriado_cond-a = v_feriado_cond-a.
                 rb_resumen_3eros.cant_feriado_cond-b = v_feriado_cond-b.
                 rb_resumen_3eros.cant_dias_feriado-1 = v_dias_feriados-1.
                 rb_resumen_3eros.cant_feriado_cond-a-1 = v_feriado_cond-a-1.
                 rb_resumen_3eros.cant_feriado_cond-b-1 = v_feriado_cond-b-1.
                 rb_resumen_3eros.cant_dias_feriado-2 = v_dias_feriados-2.
                 rb_resumen_3eros.cant_feriado_cond-a-2 = v_feriado_cond-a-2.
                 rb_resumen_3eros.cant_feriado_cond-b-2 = v_feriado_cond-b-2.
                 rb_resumen_3eros.dias_cal = round(rb_resumen_3eros.cant_total_horas / 8, 2).
                 rb_resumen_3eros.dias_cal-1 = round(rb_resumen_3eros.cant_total_horas-1 / 8, 2).
                 rb_resumen_3eros.dias_cal-2 = round(rb_resumen_3eros.cant_total_horas-2 / 8, 2).
                 rb_resumen_3eros.cant_dias_enfermedad = v_dias_enfermedad.
                 rb_resumen_3eros.cant_dias_vacaciones = v_dias_vacaciones.
                 rb_resumen_3eros.cant_dias_otraslic = v_dias_otraslic.
                 rb_resumen_3eros.cant_dias_enfermedad-1 = v_dias_enfermedad-1.
                 rb_resumen_3eros.cant_dias_vacaciones-1 = v_dias_vacaciones-1.
                 rb_resumen_3eros.cant_dias_otraslic-1 = v_dias_otraslic-1.
                 rb_resumen_3eros.cant_dias_enfermedad-2 = v_dias_enfermedad-2.
                 rb_resumen_3eros.cant_dias_vacaciones-2 = v_dias_vacaciones-2.
                 rb_resumen_3eros.cant_dias_otraslic-2 = v_dias_otraslic-2.
                 
                 v_dias = 0.
                 v_dias_feriados = 0.
                 v_feriado_cond-a = 0.
                 v_feriado_cond-b = 0.
                 v_dias_enfermedad = 0.
                 v_dias_vacaciones = 0.
                 v_dias_otraslic = 0.
                 v_horas = 0.    

                 v_dias_feriados-1 = 0.
                 v_feriado_cond-a-1 = 0.
                 v_feriado_cond-b-1 = 0.
                 v_dias_feriados-2 = 0.
                 v_feriado_cond-a-2 = 0.
                 v_feriado_cond-b-2 = 0.
                 v_dias_enfermedad-1 = 0.
                 v_dias_vacaciones-1 = 0.
                 v_dias_otraslic-1 = 0.
                 v_dias_enfermedad-2 = 0.
                 v_dias_vacaciones-2 = 0.
                 v_dias_otraslic-2 = 0.
              end. 
    end.


/* Genero feriados seg£n corresponda */
DEF BUFFER b-control FOR liq_control_tareas.
DEF BUFFER b-items FOR liq_items_control_tareas.
DEF BUFFER b-new FOR liq_items_control_tareas.


FOR EACH rb_resumen_3eros NO-LOCK,
    FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = rb_resumen_3eros.id_empresa AND
    liq_legajos.legajo = rb_resumen_3eros.legajo NO-LOCK,
    liq_centros_costos OF liq_legajos NO-LOCK:


    FIND FIRST b-control WHERE
          b-control.id_empresa = rb_resumen_3eros.id_empresa AND
          b-control.id_sucursal = rb_resumen_3eros.id_sucursal AND
          b-control.id_sector = rb_resumen_3eros.id_sector AND
          b-control.fecha = v_feriado AND
          b-control.id_tipo_planilla = 6 AND
          b-control.id_proveedor = liq_centros_costos.id_proveedor AND
          b-control.id_origen = liq_centros_costos.id_origen NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b-control THEN
     DO:
         find last b_ult use-index suc_sector_tipo_nro where  
         b_ult.id_sucursal = rb_resumen_3eros.id_sucursal and 
         b_ult.id_sector = rb_resumen_3eros.id_sector AND
         b_ult.id_tipo_planilla = 6 no-lock no-error.
         IF AVAILABLE b_ult THEN
              v-nro = b_ult.nro_planilla + 1.
            ELSE 
              v-nro = 1.
         CREATE b-control.
         ASSIGN 
         b-control.id_empresa = rb_resumen_3eros.id_empresa 
         b-control.id_sector = rb_resumen_3eros.id_sector
         b-control.id_sucursal = rb_resumen_3eros.id_sucursal
         b-control.fecha = v_feriado 
         b-control.id_tipo_planilla = 6 
         b-control.nro_planilla = v-nro
         b-control.id_proveedor = liq_centros_costos.id_proveedor 
         b-control.id_origen = liq_centros_costos.id_origen.
     END.


  IF rb_resumen_3eros.cant_dias_feriado <> 0 THEN
    DO:
      FIND FIRST b-items OF b-control WHERE
          b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
      IF NOT AVAILABLE b-items THEN
      DO:
          CREATE b-new.
          BUFFER-COPY b-control TO b-new.
          ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                 b-new.nombre = rb_resumen_3eros.nombre
                 b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                 b-new.id_tarea = 113
                 b-new.id_unidad_liquidacion = 6
                 b-new.cantidad = 1.
          ASSIGN b-new.c_fecha = TODAY
                 b-new.c_hora = STRING(TIME,"HH:MM:SS")
                 b-new.c_usuario = "auto". 
      END.
    END.

    IF rb_resumen_3eros.cant_feriado_cond-a <> 0 THEN
    DO:
        FIND FIRST b-items OF b-control WHERE
            b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-items THEN
        DO:
            CREATE b-new.
            BUFFER-COPY b-control TO b-new.
            ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                   b-new.nombre = rb_resumen_3eros.nombre
                   b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                   b-new.id_tarea = 654
                   b-new.id_unidad_liquidacion = 6
                   b-new.cantidad = 1.
            ASSIGN b-new.c_fecha = TODAY
                   b-new.c_hora = STRING(TIME,"HH:MM:SS")
                   b-new.c_usuario = "auto". 
        END.
    END.

    IF rb_resumen_3eros.cant_feriado_cond-b <> 0 THEN
    DO:
        FIND FIRST b-items OF b-control WHERE
            b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-items THEN
        DO:
            CREATE b-new.
            BUFFER-COPY b-control TO b-new.
            ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                   b-new.nombre = rb_resumen_3eros.nombre
                   b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                   b-new.id_tarea = 655
                   b-new.id_unidad_liquidacion = 6
                   b-new.cantidad = 1.
            ASSIGN b-new.c_fecha = TODAY
                   b-new.c_hora = STRING(TIME,"HH:MM:SS")
                   b-new.c_usuario = "auto". 
        END.
    END.
END.
MESSAGE "Planilla de Feriados generada" VIEW-AS ALERT-BOX.



procedure ver-condicion-periodo-anterior:
define input parameter v_empresa as integer.
define input parameter v_dnicuil like liq_items_control_tareas.dni_cuil.
define output parameter p_estado as logical initial no.

define buffer b_control for liq_control_tareas.
define buffer b_items for liq_items_control_tareas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.

DEF VAR v_cant_dias AS INTEGER.


/* Federico Diaz 12/04/2011 se consideran dias habiles no corridos */
/* Jose Lopez 09/05/2012 se excluyen los d¡as de lluvia resgitrados */
/* Jose Lopez 25/05/2016 se incluyen los dias de lluvia registrados */

v_fecha-1 = v_feriado.
v_cant_dias = 0.

repeat:
    if v_cant_dias = 10 Then leave.
    v_fecha-1 = v_fecha-1 - 1.
    
    FIND FIRST feriados_agricola WHERE feriados_agricola.fecha = v_fecha-1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE feriados_agricola THEN
      DO:
        if weekday(v_fecha-1) <> 1 Then /* Distinto de domingo */
        DO:
            /*****
            FIND FIRST t-lluvia WHERE t-lluvia.fecha = v_fecha-1 NO-LOCK NO-ERROR. /* No lluvia */
            IF NOT AVAILABLE t-lluvia  THEN   ***/
              v_cant_dias = v_cant_dias + 1.
        END.
      END.
end.

v_fecha-2 = v_feriado.

v_cuenta_dias = 0.
for each b_control where 
    b_control.id_empresa = v_empresa and
    b_control.id_sector = p_sector and
    b_control.fecha >= v_fecha-1 and
    b_control.fecha < v_fecha-2 no-lock,
    each b_items of b_control where
    b_items.dni_cuil = v_dnicuil and 
    b_items.id_tarea <> 0 and
    b_items.id_tarea <> 113 /* Feriado */ and
    b_items.id_tarea <> 654 AND
    b_items.id_tarea <> 655 AND
    (b_items.cant_jornal <> 0 or b_items.cantidad <> 0 or
     b_items.cant_horas <> 0),
    first tareas of b_items where tareas.id_grupo_tarea <> 13 no-lock 
    break by b_items.fecha:

      if last-of(b_items.fecha) Then
          v_cuenta_dias = v_cuenta_dias + 1.
           
END.
    if v_cuenta_dias >= 6 Then p_estado = yes.
end procedure. 



procedure ver-condicion-periodo-siguiente:
define input parameter v_empresa as integer.
define input parameter v_dnicuil like liq_items_control_finca.dni_cuil.
define output parameter p_estado as logical initial no.

define buffer b_control for liq_control_tareas.
define buffer b_items for liq_items_control_tareas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.
define var v_vispera as logical initial no.
DEFINE VAR v_cant_dias AS INTEGER.

v_fecha-1 = v_feriado - 1.
v_fecha-2 = v_feriado.

v_cant_dias = 0.
repeat:
    if v_cant_dias = 5 Then leave.
    v_fecha-2 = v_fecha-2 + 1.
    
    FIND FIRST feriados_agricola WHERE feriados_agricola.fecha = v_fecha-2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE feriados_agricola THEN
      DO:
        if weekday(v_fecha-2) <> 1 Then /* Distinto de domingo */
        DO:
            /****
            FIND FIRST t-lluvia WHERE t-lluvia.fecha = v_fecha-2 NO-LOCK NO-ERROR. /* No lluvia */
            IF NOT AVAILABLE t-lluvia  THEN  ***/
              v_cant_dias = v_cant_dias + 1.
        END.
      END.
end.


     v_cuenta_dias = 0.
     v_vispera = no. 
    for each b_control where 
            b_control.id_empresa = v_empresa and
            b_control.id_sector = p_sector and
            b_control.fecha >= v_fecha-1 and
            b_control.fecha < v_fecha-2 no-lock,
            each b_items of b_control where id_tarea <> 0 and
            b_items.id_tarea <> 113 and /* Feriado */
            b_items.id_tarea <> 654 AND
            b_items.id_tarea <> 655 AND
            b_items.dni_cuil = v_dnicuil and
            (b_items.cant_jornal <> 0 or b_items.cantidad <> 0 or
             b_items.cant_horas <> 0) no-lock,
             first tareas of b_items where tareas.id_grupo_tarea <> 13 no-lock break by  b_items.fecha:
                     
             if last-of(b_items.fecha) Then
                do:
                   if b_items.fecha = v_fecha-1 Then               
                      v_vispera = yes.
                   v_cuenta_dias = v_cuenta_dias + 1.
                   if v_cuenta_dias = 2 Then
                      leave.
                end.  
    end.
    if v_vispera = YES AND v_cuenta_dias = 2 Then p_estado = yes.
end procedure. 



