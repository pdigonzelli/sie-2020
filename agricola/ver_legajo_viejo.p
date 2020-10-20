  DEF VAR v-tipo AS CHARACTER INITIAL " ".
          
  for each liq_items_control_tareas where 
      /*liq_items_control_tareas.id_empresa = 101 and  */
      liq_items_control_tareas.id_sucursal = 5 and 
      liq_items_control_tareas.fecha >= DATE("01/11/14") and 
      liq_items_control_tareas.fecha <= DATE("17/11/14") /*and
      liq_items_control_tareas.id_sector = 5 AND
      liq_items_control_tareas.id_tipo_planilla <> 4*/ /*AND 
      liq_items_control_tareas.legajo = 8476 AND 
      (liq_items_control_tareas.cant_jornal <> 0 OR liq_items_control_tareas.cant_horas <> 0 OR 
       liq_items_control_tareas.cantidad <> 0) */
      , first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
      by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo
      by liq_items_control_tareas.nombre by liq_items_control_tareas.fecha :
      
     /* FIND FIRST liq_control_tareas OF liq_items_control_tareas NO-LOCK NO-ERROR.
      IF AVAILABLE liq_control_tareas THEN
     DISPLAY nro_planilla liq_items_control_tareas.fecha liq_items_control_tareas.id_tipo_planilla
     cant_jornal cant_jornal_norm liq_items_control_tareas.cant_horas cant_hs_norm liq_items_control_tareas.cantidad id_tarea.
     */
    
      /*IF cant_hs_norm <> 0 THEN  */
          ASSIGN liq_items_control_tareas.cant_horas = liq_items_control_tareas.cant_hs_norm + liq_items_control_tareas.cant_hs_compensa.  
      
      IF cant_jornal_norm <> 0 THEN
          IF cant_jornal = 0 THEN ASSIGN liq_items_control_tareas.cant_jornal = liq_items_control_tareas.cant_jornal_norm. 
        
  END.
