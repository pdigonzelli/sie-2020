define input parameter p_reporte as integer.
define input parameter p_sector like control_tareas.id_sector.
define input parameter p_fecha_desde like control_tareas.fecha.
define input parameter p_fecha_hasta like control_tareas.fecha.

define var v_cargo like personal_finca.id_cargo.
define var v_empresa like control_tareas.id_empresa.

define var v_desde_anterior like control_tareas.fecha.
define var v_hasta_anterior like control_tareas.fecha.



v_desde_anterior = date(month(p_fecha_desde),day(p_fecha_desde),year(p_fecha_desde) - 1). .
v_hasta_anterior = date(month(p_fecha_hasta),day(p_fecha_hasta),year(p_fecha_hasta) - 1) no-error.

if error-status:error Then
     v_hasta_anterior = date(month(p_fecha_hasta),day(p_fecha_hasta - 1),year(p_fecha_hasta) - 1) no-error.


for each rb_resumen_agricola where id_reporte = p_reporte :
   delete rb_resumen_agricola.
end.

/*************** Carga periodo vigente *******************************/
                                     
for each items_control_tareas WHERE 
        items_control_tareas.id_sector = p_sector and 
        items_control_tareas.fecha >= p_fecha_desde and
        items_control_tareas.fecha <= p_fecha_hasta AND
        items_control_tareas.id_tarea <> 0 no-lock,
      first tareas of items_control_tareas no-lock,
      FIRST CONTROL_tareas OF items_control_tareas NO-LOCK:



      if tareas.abreviatura begins "L" Then next.
      
      if items_control_tareas.id_empresa = 1 Then v_empresa = 1. /* Propias */
                                              Else v_empresa = 2. /* De terceros */

      find first personal_finca where personal_finca.id_empresa_cosechera =
          items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
          if available personal_finca Then
              v_cargo = personal_finca.id_cargo.
            Else
              v_cargo = 0. 
    
       find first rb_resumen_agricola where 
                  rb_resumen_agricola.id_reporte = p_reporte and
                  rb_resumen_agricola.id_empresa = v_empresa and
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor and
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen and
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  no-error.
       if not available rb_resumen_agricola Then
          do:
             find first origenes of control_tareas no-lock.
             find first zonas_agricola of origenes no-lock.
             create rb_resumen_agricola.
             assign 
                  rb_resumen_agricola.id_reporte = p_reporte 
                  rb_resumen_agricola.id_empresa = v_empresa
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor 
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen
                  rb_resumen_agricola.id_sector = items_control_tareas.id_sector 
                  rb_resumen_agricola.id_zona = origenes.id_zona
                  rb_resumen_agricola.descrip_origenes = origenes.descripcion
                  rb_resumen_agricola.descrip_tareas = tareas.descripcion
                  rb_resumen_agricola.desc_abrev_tarea = tareas.abreviatura
                  rb_resumen_agricola.descrip_zona = zonas_agricola.descripcion.
          end.
         case p_sector:
         when 1 Then
         do:
             /* Jornal Peon */
             if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) and
                items_control_tareas.id_unidad_liquidacion <> 11 Then
               rb_resumen_agricola.jornal_peon = rb_resumen_agricola.jornal_peon + items_control_tareas.cant_jornal_norm.            

             /* Jornal Trac */
             if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
               rb_resumen_agricola.jornal_trac = rb_resumen_agricola.jornal_trac + items_control_tareas.cant_jornal_norm.            



            /* Jornal Ayudante capataz */
             if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) and
                items_control_tareas.id_unidad_liquidacion = 11 Then
                do:
                  if items_control_tareas.cant_jornal_norm <> 0 Then            
                     rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cant_jornal_norm.            
                   Else
                     rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cantidad.            
                end.  



         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon = rb_resumen_agricola.horas_peon + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac = rb_resumen_agricola.horas_trac + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont = rb_resumen_agricola.horas_trac_cont + items_control_tareas.cant_horas.            

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           rb_resumen_agricola.horas_pulv = rb_resumen_agricola.horas_pulv + items_control_tareas.cantidad.            

         /* Horas Pulv Trac*/ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0)) Then
           rb_resumen_agricola.horas_pulv_trac = rb_resumen_agricola.horas_pulv_trac + items_control_tareas.cantidad.            
           
         /* Licencias */
         
         if (items_control_tareas.id_tarea >= 107 and items_control_tareas.id_tarea <= 116) or items_control_tareas.id_tarea = 118 Then
           rb_resumen_agricola.lic = rb_resumen_agricola.lic + items_control_tareas.cantidad.            
         
         /* LLuvia */  
         if items_control_tareas.id_tarea = 117 Then
           rb_resumen_agricola.lluvia = rb_resumen_agricola.lluvia + items_control_tareas.cantidad.            
           
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo = rb_resumen_agricola.destajo + items_control_tareas.cantidad.            
         
         
         /* Viaje Colectivo */  
         if items_control_tareas.id_unidad_liquidacion = 12 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viaje = rb_resumen_agricola.viaje + items_control_tareas.cantidad.            
         
         end.
         
       when 2 Then
         do:   

         /* Jornal Peon */
         if (v_cargo <> 42 and v_cargo <> 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_peon = rb_resumen_agricola.jornal_peon + items_control_tareas.cant_jornal.            
             
         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_agricola.jornal_trac = rb_resumen_agricola.jornal_trac + items_control_tareas.cant_jornal.            

         /* Jornal Ayudante Capataz */
         if (v_cargo = 32 and nro_tractor = 0 and nro_maquina = 0) Then
           rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cant_jornal.            

         if items_control_tareas.id_unidad_liquidacion = 11 Then
           rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cantidad.            

         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon = rb_resumen_agricola.horas_peon + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac = rb_resumen_agricola.horas_trac + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont = rb_resumen_agricola.horas_trac_cont + items_control_tareas.cant_horas.            

         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo = rb_resumen_agricola.destajo + items_control_tareas.cantidad.            

         /* Hs.Tractoelev */
         
         if items_control_tareas.id_unidad_liquidacion = 17 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.horas_tractoelev = rb_resumen_agricola.horas_tractoelev + items_control_tareas.cantidad.            

         /* Plus x Bins */
         
         if items_control_tareas.id_unidad_liquidacion = 16 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.plus_x_bins = rb_resumen_agricola.plus_x_bins + items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if items_control_tareas.id_unidad_liquidacion = 8 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_peon = rb_resumen_agricola.viatico_peon + items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if items_control_tareas.id_unidad_liquidacion = 9 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_ayud = rb_resumen_agricola.viatico_ayud + items_control_tareas.cantidad.            

         end.
        end. 
end.


/*************** Carga periodo anterior *******************************/
  
 for each items_control_tareas WHERE
         items_control_tareas.id_sector = p_sector and 
         items_control_tareas.fecha >= v_desde_anterior and
         items_control_tareas.fecha <= v_hasta_anterior AND
        items_control_tareas.id_tarea <> 0 no-lock,
      first tareas of items_control_tareas no-lock,
      FIRST CONTROL_tareas OF items_control_tareas NO-LOCK:
      if tareas.abreviatura begins "L" Then next.

      if items_control_tareas.id_empresa = 1 Then v_empresa = 1. /* Propias */
                                       Else v_empresa = 2. /* De terceros */
    
      find first personal_finca where personal_finca.id_empresa_cosechera =
          items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
          if available personal_finca Then
              v_cargo = personal_finca.id_cargo.
            Else
              v_cargo = 0. 
    
       find first rb_resumen_agricola where 
                  rb_resumen_agricola.id_reporte = p_reporte and
                  rb_resumen_agricola.id_empresa = v_empresa and
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor and
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen and
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  no-error.
       if not available rb_resumen_agricola Then
          do:
             find first origenes of control_tareas no-lock.
             find first zonas_agricola of origenes no-lock.
             create rb_resumen_agricola.
             assign 
                  rb_resumen_agricola.id_reporte = p_reporte 
                  rb_resumen_agricola.id_empresa = v_empresa 
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor 
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen
                  rb_resumen_agricola.id_sector = items_control_tareas.id_sector 
                  rb_resumen_agricola.id_zona = origenes.id_zona
                  rb_resumen_agricola.descrip_origenes = origenes.descripcion
                  rb_resumen_agricola.descrip_tareas = tareas.descripcion
                  rb_resumen_agricola.desc_abrev_tarea = tareas.abreviatura
                  rb_resumen_agricola.descrip_zona = zonas_agricola.descripcion.
          end.
         case p_sector:
         when 1 Then
         do:
             /* Jornal Peon */
             if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) and
                items_control_tareas.id_unidad_liquidacion <> 11 Then
               rb_resumen_agricola.jornal_peon-1 = rb_resumen_agricola.jornal_peon-1 + items_control_tareas.cant_jornal_norm.            

             /* Jornal Trac */
             if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
               rb_resumen_agricola.jornal_trac-1 = rb_resumen_agricola.jornal_trac-1 + items_control_tareas.cant_jornal_norm.            



            /* Jornal Ayudante capataz */
             if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) and
                items_control_tareas.id_unidad_liquidacion = 11 Then
                do:
                  if items_control_tareas.cant_jornal_norm <> 0 Then            
                     rb_resumen_agricola.jornal_ayud-1 = rb_resumen_agricola.jornal_ayud-1 + items_control_tareas.cant_jornal_norm.            
                   Else
                     rb_resumen_agricola.jornal_ayud-1 = rb_resumen_agricola.jornal_ayud-1 + items_control_tareas.cantidad.            
                end.  

         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon-1 = rb_resumen_agricola.horas_peon-1 + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac-1 = rb_resumen_agricola.horas_trac-1 + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont-1 = rb_resumen_agricola.horas_trac_cont-1 + items_control_tareas.cant_horas.            

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           rb_resumen_agricola.horas_pulv-1 = rb_resumen_agricola.horas_pulv-1 + items_control_tareas.cantidad.            

         /* Horas Pulv Trac*/ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0)) Then
           rb_resumen_agricola.horas_pulv_trac-1 = rb_resumen_agricola.horas_pulv_trac-1 + items_control_tareas.cantidad.            
           
         /* Licencias */
         
         if (items_control_tareas.id_tarea >= 107 and items_control_tareas.id_tarea <= 116) or items_control_tareas.id_tarea = 118 Then
           rb_resumen_agricola.lic-1 = rb_resumen_agricola.lic-1 + items_control_tareas.cantidad.            
         
         /* LLuvia */  
         if items_control_tareas.id_tarea = 117 Then
           rb_resumen_agricola.lluvia-1 = rb_resumen_agricola.lluvia-1 + items_control_tareas.cantidad.            
           
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo-1 = rb_resumen_agricola.destajo-1 + items_control_tareas.cantidad.            
         
         
         /* Viaje Colectivo */  
         if items_control_tareas.id_unidad_liquidacion = 12 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viaje-1 = rb_resumen_agricola.viaje-1 + items_control_tareas.cantidad.            
         
         end.
         
       when 2 Then
         do:   

         /* Jornal Peon */
         if (v_cargo <> 42 and v_cargo <> 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_peon-1 = rb_resumen_agricola.jornal_peon-1 + items_control_tareas.cant_jornal.            
             
         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_agricola.jornal_trac-1 = rb_resumen_agricola.jornal_trac-1 + items_control_tareas.cant_jornal.            


         /* Jornal Ayudante Capataz */
         if (v_cargo = 32 and nro_tractor = 0 and nro_maquina = 0) Then
           rb_resumen_agricola.jornal_ayud-1 = rb_resumen_agricola.jornal_ayud-1 + items_control_tareas.cant_jornal.            

         if items_control_tareas.id_unidad_liquidacion = 11 Then
           rb_resumen_agricola.jornal_ayud-1 = rb_resumen_agricola.jornal_ayud-1 + items_control_tareas.cantidad.            

         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon-1 = rb_resumen_agricola.horas_peon-1 + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac-1 = rb_resumen_agricola.horas_trac-1 + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont-1 = rb_resumen_agricola.horas_trac_cont-1 + items_control_tareas.cant_horas.            

         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo-1 = rb_resumen_agricola.destajo-1 + items_control_tareas.cantidad.            

         /* Hs.Tractoelev */
         
         if items_control_tareas.id_unidad_liquidacion = 17 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.horas_tractoelev-1 = rb_resumen_agricola.horas_tractoelev-1 + items_control_tareas.cantidad.            

         /* Plus x Bins */
         
         if items_control_tareas.id_unidad_liquidacion = 16 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.plus_x_bins-1 = rb_resumen_agricola.plus_x_bins-1 + items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if items_control_tareas.id_unidad_liquidacion = 8 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_peon-1 = rb_resumen_agricola.viatico_peon-1 + items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if items_control_tareas.id_unidad_liquidacion = 9 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_ayud-1 = rb_resumen_agricola.viatico_ayud-1 + items_control_tareas.cantidad.            

         end.
        end. 
end.



