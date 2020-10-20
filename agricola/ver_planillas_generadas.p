 for each liq_items_tarjas where 
      liq_items_tarjas.id_empresa = 505 AND   
      liq_items_tarjas.legajo = 69124 AND 
      liq_items_tarjas.fecha >= DATE("01/02/17") and 
      liq_items_tarjas.fecha <= DATE("15/02/17") /*AND
      liq_items_tarjas.id_tarea = 1102 AND 
      (liq_items_tarjas.cant_horas <> 0 OR liq_items_tarjas.cantidad <> 0)*/, 
     FIRST liq_tarjas OF liq_items_tarjas NO-LOCK,
     first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_tarjas.id_empresa and liq_legajos.legajo = liq_items_tarjas.legajo 
     NO-LOCK
     BREAK by liq_items_tarjas.id_empresa by liq_items_tarjas.legajo
      by liq_items_tarjas.nombre by liq_items_tarjas.fecha :
        
    /* DISPLAY liq_tarjas.id_sucursal liq_tarjas.id_sector liq_tarjas.fecha 
         liq_tarjas.id_tipo_planilla liq_tarjas.nro_planilla 
    liq_tarjas.id_sucursal liq_items_tarjas.legajo id_tarea cantidad.      */

    /*ASSIGN  liq_items_tarjas.dni_cuil = liq_legajos.cuil
            liq_items_tarjas.nombre = liq_legajos.apellido_nombre.  */

    /*IF liq_items_tarjas.id_codigo_abacus = 0 THEN */
    /*ASSIGN liq_items_tarjas.id_codigo_abacus = 0 
           liq_items_tarjas.id_codigo_abacus_cantidad = 700.   */


     DISPLAY liq_tarjas.id_sucursal
             liq_tarjas.id_sector
             liq_tarjas.nro_planilla
             liq_items_tarjas.id_empresa
             liq_items_tarjas.legajo liq_items_tarjas.dni_cuil 
             liq_items_tarjas.nombre id_tarea id_codigo_abacus FORMAT ">>>>>"
             liq_items_tarjas.fecha
             liq_items_tarjas.id_codigo_abacus_cantidad
             liq_items_tarjas.cantidad. 
 END.
