define input parameter p_empresa like control_tareas.id_empresa.
define input parameter p_sector as integer.
define input parameter p_fecha_desde like control_tareas.fecha.
define input parameter p_fecha_hasta like control_tareas.fecha.
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

 
define temp-table rb_resumen_3eros like items_control_tareas
    field jornal_peon like items_control_tareas.cant_jornal
    field jornal_trac like items_control_tareas.cant_jornal
    field jornal_ayud_capataz like items_control_tareas.cant_jornal
    field horas_peon like items_control_tareas.cant_horas
    field horas_trac like items_control_tareas.cant_horas
    field horas_trac_cont like items_control_tareas.cant_horas
    field horas_tractoelev like items_control_tareas.cantidad
    field plus_x_bins like items_control_tareas.cantidad
    field viatico_peon like items_control_tareas.cantidad
    field viatico_ayud like items_control_tareas.cantidad
    field horas_pulv like items_control_tareas.cantidad
    field horas_pulv_trac like items_control_tareas.cantidad
    field lic like items_control_tareas.cantidad
    field lluvia like items_control_tareas.cantidad
    field destajo like items_control_tareas.cantidad
    field viaje like items_control_tareas.cantidad
    field cant_hectareas like items_control_tareas.cantidad
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



for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   

p_archivo = "c:\temp\jorpers.txt".


for each control_tareas where 
    control_tareas.id_empresa = p_empresa and
    control_tareas.id_sector = p_sector and
    control_tareas.fecha >= p_fecha_desde and
    control_tareas.fecha <= p_fecha_hasta no-lock,
    each items_control_tareas of control_tareas where id_tarea <> 0 and
         items_control_tareas.id_tarea <> 113 and /* Feriado */
         items_control_tareas.dni_cuil <> "" no-lock
    break by items_control_tareas.id_empresa by 
    items_control_tareas.dni_cuil by items_control_tareas.fecha:
      
    
       find first rb_resumen_3eros where 
                  rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa and
                  rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil  no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = items_control_tareas.id_empresa 
                    rb_resumen_3eros.id_sector = items_control_tareas.id_sector 
                    rb_resumen_3eros.legajo = items_control_tareas.legajo 
                    rb_resumen_3eros.nombre = items_control_tareas.nombre. 
                    rb_resumen_3eros.dni_cuil = items_control_tareas.dni_cuil. 
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
         if (v_cargo <> 42 and v_cargo <> 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           do:
           rb_resumen_3eros.jornal_peon = rb_resumen_3eros.jornal_peon + items_control_tareas.cant_jornal.            
           v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
           end.


         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           do:
              rb_resumen_3eros.jornal_trac = rb_resumen_3eros.jornal_trac + items_control_tareas.cant_jornal.            
              v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
           end.


         /* Jornal Ayud Capataz */

         if (v_cargo = 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           do:
           rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + items_control_tareas.cant_jornal.            
           v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
           end.


          if items_control_tareas.id_unidad_liquidacion = 11 Then
           do:
             rb_resumen_3eros.jornal_ayud = rb_resumen_3eros.jornal_ayud + items_control_tareas.cantidad.          
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
          
          
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:
              rb_resumen_3eros.destajo = rb_resumen_3eros.destajo + items_control_tareas.cantidad.            
              v_horas = v_horas + 8.
           end.

         /* Hs.Tractoelev */
         
         if items_control_tareas.id_unidad_liquidacion = 17 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:
           rb_resumen_3eros.horas_tractoelev = rb_resumen_3eros.horas_tractoelev + items_control_tareas.cantidad.            
           v_horas = v_horas + items_control_tareas.cantidad.
           end.

         /* Plus x Bins */
         
         if items_control_tareas.id_unidad_liquidacion = 16 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.plus_x_bins = rb_resumen_3eros.plus_x_bins + items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if items_control_tareas.id_unidad_liquidacion = 8 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_peon = rb_resumen_3eros.viatico_peon + items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if items_control_tareas.id_unidad_liquidacion = 9 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_3eros.viatico_ayud = rb_resumen_3eros.viatico_ayud + items_control_tareas.cantidad.            
          
   
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
         
         /* Hectareas */  
         if items_control_tareas.id_unidad_liquidacion = 13 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           do:
             rb_resumen_3eros.cant_hectareas = rb_resumen_3eros.cant_hectareas + items_control_tareas.cantidad.            
             v_horas = v_horas + (items_control_tareas.cantidad * 8).
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


        
         if last-of(items_control_tareas.fecha) Then
            do:
                 if v_horas > 0 Then
                    do:
                      if items_control_tareas.fecha = v_feriado 
                         and items_control_tareas.id_tarea <> 107 
                         and items_control_tareas.id_tarea <> 116  Then
                              do:
                                v_dias_feriados = v_dias_feriados + 1.
                                if day(items_control_tareas.fecha) <= 15 Then
                                     v_dias_feriados-1 = v_dias_feriados-1 + 1.
                                    Else 
                                     v_dias_feriados-2 = v_dias_feriados-2 + 1.
                              end.  
                              Else 
                              do:
                                case items_control_tareas.id_tarea:
                                 when 107 Then /* Enfermedades */
                                  do:
                                   v_dias_enfermedad = v_dias_enfermedad + 1.
                                   if day(items_control_tareas.fecha) <= 15 Then
                                      v_dias_enfermedad-1 = v_dias_enfermedad-1 + 1.
                                     Else
                                      v_dias_enfermedad-2 = v_dias_enfermedad-2 + 1.
                                  end. 
                                 when 116 Then /* Vacaciones */
                                  do:
                                   v_dias_vacaciones = v_dias_vacaciones + 1.
                                   if day(items_control_tareas.fecha) <= 15 Then
                                      v_dias_vacaciones-1 = v_dias_vacaciones-1 + 1.
                                     Else
                                      v_dias_vacaciones-2 = v_dias_vacaciones-2 + 1.
                                  end. 
                                 when 108 or when 109 or when 110 /* Otras lic*/ 
                                          or when 111 or when 112
                                          or when 184 or when 185 Then
                                  do:
                                   v_dias_otraslic = v_dias_otraslic + 1.
                                   if day(items_control_tareas.fecha) <= 15 Then
                                      v_dias_otraslic-1 = v_dias_otraslic-1 + 1.
                                          Else
                                      v_dias_otraslic-2 = v_dias_otraslic-2 + 1.
                                  end.           
                                 otherwise
                                 do:
                                   v_dias = v_dias + 1.
                                       
                                   if v_horas > 8 Then  v_horas = 8.
                                    rb_resumen_3eros.cant_total_horas = rb_resumen_3eros.cant_total_horas + v_horas.
                                   
                                   if day(items_control_tareas.fecha) <= 15 Then
                                          rb_resumen_3eros.cant_total_horas-1 = rb_resumen_3eros.cant_total_horas-1 + v_horas.
                                      Else
                                          rb_resumen_3eros.cant_total_horas-2 = rb_resumen_3eros.cant_total_horas-2 + v_horas.
 
                                 end.  
                                end. 
                              end.  
                                
                    end.
                v_horas = 0.    
            end.
           
           if last-of(items_control_tareas.dni_cuil) Then
              do:
                 if v_dias_feriados = 0 Then
                    do: 
                        run ver-condicion-periodo-anterior  (input control_tareas.id_empresa,
                                                             input items_control_tareas.dni_cuil,
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
                        run ver-condicion-periodo-siguiente (input control_tareas.id_empresa,
                                                             input items_control_tareas.dni_cuil,
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

if p_archivo = "" Then p_archivo = "c:\temp\repgral01.txt".

output to value(p_archivo).
  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Sector;". 
  put "Nombre Sector;".
  put "Legajo;". 
  put "Nombre;". 
  put "DNI/CUIL;". 
  put "DIAS CAL;".
  put "Dias Trab.;".
  put "Feriado trab.;".
  put "Feriado cond-a;".
  put "Feriado cond-b;".
  put "Enfermedad;".
  put "Vacaciones;".
  put "Otras Lic;".
  put "Adicional Feriado;".
  put "TOTAL MENSUAL;".
  put "DIAS CAL-1;".
  put "Feriado trab-1;".
  put "Feriado cond-a-1;".
  put "Feriado cond-b-1;".
  put "Enfermedad-1;".
  put "Vacaciones-1;".
  put "Otras Lic-1;".
  put "Adi Feriado-1;".
  put "Total-1;".
  put "DIAS CAL-2;".
  put "Feriado trab-2;".
  put "Feriado cond-a-2;".
  put "Feriado cond-b-2;".
  put "Enfermedad-2;".
  put "Vacaciones-2;".
  put "Otras Lic-2;".
  put "Adi Feriado-2;".
  put "Total-2;".
  
  put skip.
  for each rb_resumen_3eros no-lock:
     find first proveedores where proveedores.id_proveedor = rb_resumen_3eros.id_empresa no-lock no-error.
     find first sectores_agricolas where sectores_agricolas.id_sector = rb_resumen_3eros.id_sector no-lock no-error.

     v_total =  rb_resumen_3eros.dias_cal + rb_resumen_3eros.cant_dias_feriado + 
                rb_resumen_3eros.cant_feriado_cond-a + rb_resumen_3eros.cant_feriado_cond-b +
                rb_resumen_3eros.cant_dias_enfermedad + rb_resumen_3eros.cant_dias_vacaciones +
                rb_resumen_3eros.cant_dias_otraslic +  rb_resumen_3eros.cant_dias_feriado.


     v_total-1 =  rb_resumen_3eros.dias_cal-1 + rb_resumen_3eros.cant_dias_feriado-1 + 
                rb_resumen_3eros.cant_feriado_cond-a-1 + rb_resumen_3eros.cant_feriado_cond-b-1 +
                rb_resumen_3eros.cant_dias_enfermedad-1 + rb_resumen_3eros.cant_dias_vacaciones-1 +
                rb_resumen_3eros.cant_dias_otraslic-1 +  
                rb_resumen_3eros.cant_dias_feriado-1.

     v_total-2 =  rb_resumen_3eros.dias_cal-2 + rb_resumen_3eros.cant_dias_feriado-2 + 
                rb_resumen_3eros.cant_feriado_cond-a-2 + rb_resumen_3eros.cant_feriado_cond-b-2 +
                rb_resumen_3eros.cant_dias_enfermedad-2 + rb_resumen_3eros.cant_dias_vacaciones-2 +
                rb_resumen_3eros.cant_dias_otraslic-2 +  
                rb_resumen_3eros.cant_dias_feriado-2.



               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                if available proveedores Then proveedores.nombre Else " "
                rb_resumen_3eros.id_sector
                if available sectores_agricolas Then sectores_agricolas.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.dias_cal
                rb_resumen_3eros.cant_dias
                rb_resumen_3eros.cant_dias_feriado
                rb_resumen_3eros.cant_feriado_cond-a
                rb_resumen_3eros.cant_feriado_cond-b
                rb_resumen_3eros.cant_dias_enfermedad
                rb_resumen_3eros.cant_dias_vacaciones
                rb_resumen_3eros.cant_dias_otraslic
                rb_resumen_3eros.cant_dias_feriado
                v_total
                rb_resumen_3eros.dias_cal-1
                rb_resumen_3eros.cant_dias_feriado-1
                rb_resumen_3eros.cant_feriado_cond-a-1
                rb_resumen_3eros.cant_feriado_cond-b-1
                rb_resumen_3eros.cant_dias_enfermedad-1
                rb_resumen_3eros.cant_dias_vacaciones-1
                rb_resumen_3eros.cant_dias_otraslic-1
                rb_resumen_3eros.cant_dias_feriado-1
                v_total-1
                rb_resumen_3eros.dias_cal-2
                rb_resumen_3eros.cant_dias_feriado-2
                rb_resumen_3eros.cant_feriado_cond-a-2
                rb_resumen_3eros.cant_feriado_cond-b-2
                rb_resumen_3eros.cant_dias_enfermedad-2
                rb_resumen_3eros.cant_dias_vacaciones-2
                rb_resumen_3eros.cant_dias_otraslic-2
                rb_resumen_3eros.cant_dias_feriado-2
                v_total-2.
  end.
output close.

message "Archivo gral generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).





procedure ver-condicion-periodo-anterior:
define input parameter v_empresa as integer.
define input parameter v_dnicuil like items_control_tareas.dni_cuil.
define output parameter p_estado as logical initial no.

define buffer b_control for control_tareas.
define buffer b_items for items_control_tareas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.

v_fecha-1 = v_feriado - 10.
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
    (b_items.cant_jornal <> 0 or b_items.cantidad <> 0 or
     b_items.cant_horas <> 0),
    first tareas of b_items where tareas.id_grupo <> 13 no-lock 
    break by b_items.fecha:

      if last-of(b_items.fecha) Then
          v_cuenta_dias = v_cuenta_dias + 1.
           
    end.
    if v_cuenta_dias >= 6 Then p_estado = yes.
end procedure. 



procedure ver-condicion-periodo-siguiente:
define input parameter v_empresa as integer.
define input parameter v_dnicuil like items_control_finca.dni_cuil.
define output parameter p_estado as logical initial no.

define buffer b_control for control_tareas.
define buffer b_items for items_control_tareas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.
define var v_vispera as logical initial no.

v_fecha-1 = v_feriado - 1.
v_fecha-2 = v_feriado + 5.

     v_cuenta_dias = 0.
     v_vispera = no. 
    for each b_control where 
            b_control.id_empresa = v_empresa and
            b_control.id_sector = p_sector and
            b_control.fecha >= v_fecha-1 and
            b_control.fecha < v_fecha-2 no-lock,
            each b_items of b_control where id_tarea <> 0 and
            b_items.id_tarea <> 113 and /* Feriado */
            b_items.dni_cuil = v_dnicuil and
            (b_items.cant_jornal <> 0 or b_items.cantidad <> 0 or
             b_items.cant_horas <> 0) no-lock,
             first tareas of b_items where tareas.id_grupo <> 13 no-lock break by  b_items.fecha:
                     
             if last-of(b_items.fecha) Then
                do:
                   if b_items.fecha = v_fecha-1 Then               
                      v_vispera = yes.
                   v_cuenta_dias = v_cuenta_dias + 1.
                   if v_cuenta_dias = 2 Then
                      leave.
                end.  
    end.
    if v_vispera and v_cuenta_dias = 2 Then p_estado = yes.
end procedure. 



