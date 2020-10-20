        /***Jornal****/
       if liq_items_control_tareas.cant_jornal <> 0  Then
         if v_cargo <> 42 Then
            if (liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0) 
               and  liq_items_control_tareas.id_unidad_liquidacion = 7 Then
               v_codigo = tareas.id_concepto_liq[5].
            Else
               v_codigo =  tareas.id_concepto_liq[1].
        Else
          v_codigo = tareas.id_concepto_liq[5].
       Else
        v_codigo = 9999. 

      /*****Horas********/
      if liq_items_control_tareas.cant_horas <> 0  Then
          IF liq_items_control_tareas.id_tipo_planilla <> 4 THEN
          DO:
            if v_cargo <> 42 Then
               do:
                  if liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0 Then
                     v_codigo-1 = tareas.id_concepto_liq[3].
                   Else  
                     v_codigo-1 = tareas.id_concepto_liq[2].
               end.
               Else
                   v_codigo-1 = tareas.id_concepto_liq[3].
           END.
           ELSE
             v_codigo-1 = 1430. /* Planilla 4 */
      Else 
         v_codigo-1 = 9999.

     /****Otros******/    
     if liq_items_control_tareas.cantidad <> 0 Then
       do:
           if liq_items_control_tareas.id_unidad_liquidacion <> 0 Then 
            do:
              IF liq_items_control_tareas.id_tipo_planilla <> 4 THEN
              DO:
                 if liq_items_control_tareas.id_unidad_liquidacion = 7 Then
                     do:
                     if v_cargo <> 42 Then
                       if liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0 Then
                           v_codigo-2 = tareas.id_concepto_liq[3].
                         Else 
                           v_codigo-2 = tareas.id_concepto_liq[2].
                       Else
                          v_codigo-2 = tareas.id_concepto_liq[3].
                     end.
                     Else
                     do:
                         if liq_items_control_tareas.id_unidad_liquidacion <= 20 Then
                              v_codigo-2 = tareas.id_concepto_liq[liq_items_control_tareas.id_unidad_liquidacion].
                           Else
                            do:
                                find first r_tareas_unidades where
                                r_tareas_unidades.id_tarea = liq_items_control_tareas.id_tarea and
                                r_tareas_unidades.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion  no-lock no-error.
                                if available r_tareas_unidades Then
                                  v_codigo-2 = r_tareas_unidades.id_concepto_liq.
               
                                
                            end.
                     end.
               END.
               ELSE /* Planilla 4 */
               v_codigo-2 = 1430.
           end. 
       end.  
      Else         
        v_codigo-2 = 9999.
