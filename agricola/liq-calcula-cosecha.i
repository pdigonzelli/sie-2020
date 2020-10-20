        /***Jornal****/
       if liq_items_control_tareas.cant_jornal <> 0  Then
        if v_cargo <> 32 Then /* Ayudante capataz*/
          v_codigo = tareas.id_concepto[14].
         Else
          v_codigo = tareas.id_concepto[11].
       Else
        v_codigo = 9999. 

      /*****Horas********/
      if liq_items_control_tareas.cant_horas <> 0  Then
        IF liq_items_control_tareas.id_tipo_planilla <> 4 THEN
         DO:
            if v_cargo <> 42 Then /* Tractorista */
             do:
              if liq_items_control_tareas.nro_tractor <> 0 or liq_items_control_tareas.nro_maquina <> 0 Then
                 v_codigo-1 = tareas.id_concepto[15].
               Else  
                 v_codigo-1 = tareas.id_concepto[2].
             end.
           Else
               v_codigo-1 = tareas.id_concepto[15].
        END.
        ELSE v_codigo-1 = 502.
      Else 
         v_codigo-1 = 9999.

     /****Otros******/    
     if liq_items_control_tareas.cantidad <> 0 Then
       do:
       if liq_items_control_tareas.id_unidad_liquidacion <> 0 Then 
         DO:
          IF liq_items_control_tareas.id_tipo_planilla <> 4 THEN
          DO:
              if liq_items_control_tareas.id_unidad_liquidacion <= 20 Then
                DO:
                   v_codigo-2 = tareas.id_concepto[liq_items_control_tareas.id_unidad_liquidacion].
                   IF v_codigo-2 = 0 THEN
                   DO:
                       find first r_tareas_unidades where
                       r_tareas_unidades.id_tarea = liq_items_control_tareas.id_tarea and
                       r_tareas_unidades.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion  no-lock no-error.
                       if available r_tareas_unidades Then
                          v_codigo-2 = r_tareas_unidades.id_concepto.

                   END.
                END.
                Else
                 do:
                     find first r_tareas_unidades where
                     r_tareas_unidades.id_tarea = liq_items_control_tareas.id_tarea and
                     r_tareas_unidades.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion  no-lock no-error.
                     if available r_tareas_unidades Then
                        v_codigo-2 = r_tareas_unidades.id_concepto.
                END.
          END.
            ELSE
             v_codigo-2 = 502.
         END.
       end.  
      Else         
        v_codigo-2 = 9999.
