  FOR EACH liq_items_tarjas WHERE
      liq_items_tarjas.id_empresa = 101  AND
      liq_items_tarjas.legajo = 33811 AND
      liq_items_tarjas.fecha >= DATE("16/03/2017") and
      liq_items_tarjas.fecha <= DATE("31/03/2017") ,
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                              liq_legajos.legajo = liq_items_tarjas.legajo NO-LOCK BREAK BY liq_legajos.legajo_rhpro BY liq_items_tarjas.fecha:

      FIND FIRST liq_tareas OF liq_items_tarjas NO-LOCK NO-ERROR.
      /*ASSIGN liq_items_tarjas.nombre = liq_legajos.apellido_nombre.*/

      DISPLAY liq_legajos.legajo_rhpro FORMAT ">>>>>>>>>>9" 
          liq_items_tarjas.nombre
          liq_items_tarjas.legajo
          liq_items_tarjas.id_tarea liq_items_tarjas.fecha liq_tareas.id_grupo_tarea. 
  END.
