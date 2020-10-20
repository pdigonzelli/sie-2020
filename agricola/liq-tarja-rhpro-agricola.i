   /*****Horas********/
      if liq_items_tarjas.cant_horas <> 0  Then
          IF liq_items_tarjas.id_tipo_planilla <> 4 THEN
          DO:
             v_codigo-1 = liq_items_tarjas.id_codigo_abacus.
           END.
           ELSE
             v_codigo-1 = 1430. /* Planilla 4 */
      Else 
         v_codigo-1 = 9999.

     /****Otros******/    
     if liq_items_tarjas.cantidad <> 0 Then
       do:
           if liq_items_tarjas.id_unidad_liquidacion <> 0 Then 
            do:
              IF liq_items_tarjas.id_tipo_planilla <> 4 THEN
               DO:
                  v_codigo-2 = liq_items_tarjas.id_codigo_abacus.
               END.
               ELSE /* Planilla 4 */
               v_codigo-2 = 1430.
           end. 
       end.  
      Else         
        v_codigo-2 = 9999.
