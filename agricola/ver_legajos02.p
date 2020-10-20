 for each liq_items_tarjas where 
     /* liq_items_tarjas.id_empresa = 1 AND */  
     /* liq_items_tarjas.legajo = 13053 AND  */
      liq_items_tarjas.fecha >= DATE("15/02/17") NO-LOCK
      :
     
     FIND FIRST liq_tarjas OF liq_items_tarjas NO-LOCK NO-ERROR.

     IF NOT  AVAILABLE liq_tarjas THEN
     DO:
         DISPLAY "no" liq_items_tarjas.id_proveedor liq_items_tarjas.id_origen liq_items_tarjas.legajo.
     END.

 END.
