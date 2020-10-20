        /***Jornal****/
       if items_control_tareas.cant_jornal <> 0  Then
         if v_cargo <> 42 Then
            if (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) 
               and  items_control_tareas.id_unidad_liquidacion = 7 Then
               v_codigo = tareas.id_concepto[5].
            Else
               v_codigo =  tareas.id_concepto[1].
        Else
          v_codigo = tareas.id_concepto[5].
       Else
        v_codigo = 9999. 

      /*****Horas********/
      if items_control_tareas.cant_horas <> 0  Then
          IF items_control_tareas.id_tipo_planilla <> 4 THEN
          DO:
            if v_cargo <> 42 Then
               do:
                  if items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0 Then
                     v_codigo-1 = tareas.id_concepto[3].
                   Else  
                     v_codigo-1 = tareas.id_concepto[2].
               end.
               Else
                   v_codigo-1 = tareas.id_concepto[3].
           END.
           ELSE
             v_codigo-1 = 502. /* Planilla 4 */
      Else 
         v_codigo-1 = 9999.

     /****Otros******/    
     if items_control_tareas.cantidad <> 0 Then
       do:
           if items_control_tareas.id_unidad_liquidacion <> 0 Then 
            do:
              IF items_control_tareas.id_tipo_planilla <> 4 THEN
              DO:
                 if items_control_tareas.id_unidad_liquidacion = 7 Then
                     do:
                     if v_cargo <> 42 Then
                       if items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0 Then
                           v_codigo-2 = tareas.id_concepto[3].
                         Else 
                           v_codigo-2 = tareas.id_concepto[2].
                       Else
                          v_codigo-2 = tareas.id_concepto[3].
                     end.
                     Else
                     do:
                         if items_control_tareas.id_unidad_liquidacion <= 20 Then
                              v_codigo-2 = tareas.id_concepto[items_control_tareas.id_unidad_liquidacion].
                           Else
                            do:
                                find first r_tareas_unidades where
                                r_tareas_unidades.id_tarea = items_control_tareas.id_tarea and
                                r_tareas_unidades.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion  no-lock no-error.
                                if available r_tareas_unidades Then
                                  v_codigo-2 = r_tareas_unidades.id_concepto.
               
                                
                            end.
                     end.
               END.
               ELSE /* Planilla 4 */
               v_codigo-2 = 502.
           end. 
       end.  
      Else         
        v_codigo-2 = 9999.
