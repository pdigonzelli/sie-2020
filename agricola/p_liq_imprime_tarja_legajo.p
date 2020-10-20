DEF INPUT PARAMETER v-empresa AS INTEGER.
DEF INPUT PARAMETER v-sector AS INTEGER.
DEF INPUT PARAMETER v-sucursal AS INTEGER.
DEF INPUT PARAMETER v-desde-fecha AS DATE.
DEF INPUT PARAMETER v-hasta-fecha AS DATE.
DEF INPUT PARAMETER v-finca AS LOGICAL.
DEF INPUT PARAMETER v-tipo AS CHARACTER.
DEF INPUT PARAMETER v-impresora AS CHARACTER.
DEF INPUT PARAMETER v-legajo AS INTEGER.
 

 define var v_general as character.
 define var j as integer.
 DEFINE VAR i AS INTEGER.
 DEFINE VAR v_nro_reporte AS INTEGER.

  def var v_resumen as character.
  define var v_cargo like liq_legajos.id_cargo.
  define var v_centro like liq_legajos.id_centro_costo.
  DEFINE VAR v_cuenta AS INTEGER.
  DEF VAR v_pos AS INTEGER.
  DEF VAR v_total AS DECIMAL.

  DEF VAR v_cant_conceptos AS INTEGER.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

{s_varsis.i}



if v-empresa = 0 Then
   do:
     message "Debe ingresar una empresa" view-as alert-box.
     return.
   end.


 v_nro_reporte = RANDOM(1,1000000).




 for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
     rb_tarja.id_empresa = v-empresa AND
     rb_tarja.legajo = v-legajo:
   for each rb_items_tarja of rb_tarja:
      delete rb_items_tarja.
   end.
   delete rb_tarja.
 end.      
 
 j = 0.
 v_pos = 0.
 for each liq_items_tarjas where 
      liq_items_tarjas.id_empresa = v-empresa AND 
      (IF v-sucursal <> 0 THEN liq_items_tarjas.id_sucursal = v-sucursal ELSE true) and 
      liq_items_tarjas.fecha >= v-desde-fecha and 
      liq_items_tarjas.fecha <= v-hasta-fecha and
      (IF v-sector <> 0 THEN liq_items_tarjas.id_sector = v-sector ELSE true) AND
      liq_items_tarjas.id_tipo_planilla <> 4 AND
      liq_items_tarjas.legajo = v-legajo AND
      (liq_items_tarjas.cant_horas <> 0 OR liq_items_tarjas.cantidad <> 0) 
      no-lock,
     first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_tarjas.id_empresa and liq_legajos.legajo = liq_items_tarjas.legajo and
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
     BREAK by liq_items_tarjas.id_empresa by liq_items_tarjas.legajo
      by liq_items_tarjas.nombre by liq_items_tarjas.fecha :

      find first liq_tareas where liq_tareas.id_tarea = liq_items_tarjas.id_tarea no-lock no-error.
      if NOT available liq_tareas THEN NEXT.



      find first rb_tarja where rb_tarja.id_reporte = v_nro_reporte and
        rb_tarja.id_empresa = liq_items_tarjas.id_empresa AND
        rb_tarja.id_sucursal = liq_items_tarjas.id_sucursal AND
        rb_tarja.id_sector = liq_items_tarjas.id_sector AND
        rb_tarja.legajo = liq_items_tarjas.legajo and
        rb_tarja.nombre = liq_items_tarjas.nombre  no-error.
        if not available rb_tarja Then
           do:
              create rb_tarja.
              rb_tarja.id_reporte = v_nro_reporte.
              rb_tarja.id_empresa = liq_items_tarjas.id_empresa.
              rb_tarja.id_sucursal = liq_items_tarjas.id_sucursal.
              rb_tarja.id_sector = liq_items_tarjas.id_sector.
              rb_tarja.legajo = liq_items_tarjas.legajo.
              rb_tarja.nombre = liq_legajos.apellido_nombre.
              rb_tarja.dni_cuil = liq_legajos.cuil.
              rb_tarja.id_cargo = liq_legajos.id_cargo.
              rb_tarja.id_centro_abacus = liq_legajos.id_centro_costo.

              find first liq_centros_costos where liq_centros_costos.id_centro_costo = rb_tarja.id_centro_abacus no-lock
                   no-error.    
                   if available liq_centros_costos Then
                      rb_tarja.nombre_centro_abacus = liq_centros_costos.descripcion.   
           end. 



          v_cargo = liq_legajos.id_cargo.
          v_centro = liq_legajos.id_centro_costo.
        
     
     FIND FIRST rb_items_tarja WHERE
         rb_items_tarja.id_reporte = v_nro_reporte AND
         rb_items_tarja.id_empresa = liq_items_tarjas.id_empresa AND
         rb_items_tarja.id_sucursal = liq_items_tarjas.id_sucursal  AND
         rb_items_tarja.id_sector = liq_items_tarjas.id_sector AND
         rb_items_tarja.fecha = liq_items_tarjas.fecha  AND
         rb_items_tarja.id_proveedor = liq_items_tarjas.id_proveedor AND
         rb_items_tarja.id_origen  = liq_items_tarjas.id_origen  AND
         rb_items_tarja.legajo = liq_items_tarjas.legajo AND
         rb_items_tarja.nombre  = liq_items_tarjas.nombre AND
         rb_items_tarja.id_tarea = liq_items_tarjas.id_tarea AND
         rb_items_tarja.id_lote = liq_items_tarjas.id_lote  NO-ERROR.
    IF NOT AVAILABLE rb_items_tarja THEN
    DO:
  
     create rb_items_tarja.
     assign rb_items_tarja.id_reporte = v_nro_reporte
            rb_items_tarja.id_empresa = liq_items_tarjas.id_empresa
            rb_items_tarja.id_sucursal = liq_items_tarjas.id_sucursal
            rb_items_tarja.id_sector = liq_items_tarjas.id_sector
            rb_items_tarja.legajo = liq_items_tarjas.legajo
            rb_items_tarja.nombre = liq_items_tarjas.nombre
            rb_items_tarja.fecha = liq_items_tarjas.fecha
            rb_items_tarja.id_tarea = liq_items_tarjas.id_tarea 
            rb_items_tarja.id_proveedor = liq_items_tarjas.id_proveedor 
            rb_items_tarja.id_origen = liq_items_tarjas.id_origen 
            rb_items_tarja.id_lote = liq_items_tarjas.id_lote.
    END.

        ASSIGN rb_items_tarja.cant_jornal = liq_items_tarjas.cant_jornal_norm
            rb_items_tarja.cant_horas = liq_items_tarjas.cant_hs_norm
            rb_items_tarja.codigo_horas = liq_items_tarjas.id_codigo_abacus 
            rb_items_tarja.id_diferencial = liq_items_tarjas.id_diferencial
            rb_items_tarja.codigo_diferencial = liq_items_tarjas.id_codigo_abacus_diferencial 
            rb_items_tarja.cantidad = liq_items_tarjas.cantidad
            rb_items_tarja.nro_tractor = liq_items_tarjas.nro_tractor
            rb_items_tarja.nro_maquina = liq_items_tarjas.nro_maquina.
    
        /* Agregado 11/14*/
        IF liq_items_tarjas.hs_acond_finca <> 0 THEN
            ASSIGN rb_items_tarja.cant_hs_complementarias = liq_items_tarjas.hs_acond_finca
                   rb_items_tarja.codigo_hs_complementarias = 524.

        IF liq_items_tarjas.hs_plus_tareas_automatico <> 0 THEN
            ASSIGN rb_items_tarja.cant_hs_complementarias = liq_items_tarjas.hs_plus_tareas_automatico
                   rb_items_tarja.codigo_hs_complementarias = 285.

        IF liq_items_tarjas.hs_plus_tareas_trabajadas <> 0 THEN
            ASSIGN rb_items_tarja.cant_hs_complementarias = liq_items_tarjas.hs_plus_tareas_trabajadas
                   rb_items_tarja.codigo_hs_complementarias = 286.

        IF liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0 THEN
            ASSIGN rb_items_tarja.cant_hs_complementarias = liq_items_tarjas.hs_adicionales_tareas_trabajadas
                   rb_items_tarja.codigo_hs_complementarias = 287.
        /****************/

            
    find first liq_tareas where liq_tareas.id_tarea = liq_items_tarjas.id_tarea no-lock no-error.
    if available liq_tareas Then
       do:
         rb_items_tarja.nombre_tarea = liq_tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = liq_tareas.descripcion.
       end.  

    find first origenes of liq_items_tarjas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  liq_items_tarjas.id_proveedor and
         lotes_plantacion.id_origen =  liq_items_tarjas.id_origen and
         lotes_plantacion.id_lote =  liq_items_tarjas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.

    IF liq_tareas.id_grupo_consulta = 16  THEN
    DO:
        rb_items_tarja.codigo_licencias = liq_tareas.id_codigo_abacus.
        find first conceptos_abacus where conceptos_abacus.id_concepto = liq_tareas.id_codigo_abacus NO-LOCK NO-ERROR.
        IF AVAILABLE conceptos_abacus THEN
            rb_items_tarja.nombre_unidad = ENTRY(1,conceptos_abacus.descripcion,"-").
    END.

    IF rb_items_tarja.cantidad <> 0 AND rb_items_tarja.nombre_unidad = ""  THEN
    DO:
        find first conceptos_abacus where conceptos_abacus.id_concepto = liq_items_tarjas.id_codigo_abacus_cantidad NO-LOCK NO-ERROR.
        IF AVAILABLE conceptos_abacus THEN
            ASSIGN rb_items_tarja.codigo_cantidad = liq_items_tarjas.id_codigo_abacus_cantidad
                   rb_items_tarja.nombre_unidad = ENTRY(1,conceptos_abacus.descripcion,"-").
    END.
END.



j = 0. 
v_total = 0.
FOR EACH rb_items_tarja WHERE 
    rb_items_tarja.id_reporte = v_nro_reporte AND
    rb_items_tarja.id_empresa = v-empresa AND
    rb_items_tarja.legajo = v-legajo AND
    rb_items_tarja.codigo_horas <> 0 NO-LOCK BREAK BY rb_items_tarja.codigo_horas:
    v_total = v_total + rb_items_tarja.cant_horas.
    IF LAST-OF(rb_items_tarja.codigo_horas) THEN
    DO:
       j = j + 1.
       ASSIGN rb_tarja.id_concepto[j] = rb_items_tarja.codigo_horas. 
       find first conceptos_abacus where conceptos_abacus.id_concepto = rb_items_tarja.codigo_horas
             no-lock no-error.
        if available conceptos_abacus Then     
            rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
        
        rb_tarja.valor_concepto[j] =  v_total. 

        v_total = 0.
    END. 
END.

v_total = 0.
FOR EACH rb_items_tarja WHERE 
    rb_items_tarja.id_reporte = v_nro_reporte AND
    rb_items_tarja.id_empresa = v-empresa AND
    rb_items_tarja.legajo = v-legajo AND
    rb_items_tarja.codigo_cantidad <> 0 NO-LOCK BREAK BY rb_items_tarja.codigo_cantidad :
    v_total = v_total + rb_items_tarja.cantidad.
    IF LAST-OF(rb_items_tarja.codigo_cantidad) THEN
    DO:
       j = j + 1.
       ASSIGN rb_tarja.id_concepto[j] = rb_items_tarja.codigo_cantidad. 
       find first conceptos_abacus where conceptos_abacus.id_concepto = rb_items_tarja.codigo_cantidad
             no-lock no-error.
        if available conceptos_abacus Then     
            rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
        
        rb_tarja.valor_concepto[j] =  v_total. 

        v_total = 0.
    END. 
END.

v_total = 0.
FOR EACH rb_items_tarja WHERE 
    rb_items_tarja.id_reporte = v_nro_reporte AND
    rb_items_tarja.id_empresa = v-empresa AND
    rb_items_tarja.legajo = v-legajo AND
    rb_items_tarja.codigo_diferencial <> 0 NO-LOCK BREAK BY rb_items_tarja.codigo_diferencial :
    v_total = v_total + rb_items_tarja.cant_horas.
    IF LAST-OF(rb_items_tarja.codigo_diferencial) THEN
    DO:
       j = j + 1.
       ASSIGN rb_tarja.id_concepto[j] = rb_items_tarja.codigo_diferencial. 
       find first conceptos_abacus where conceptos_abacus.id_concepto = rb_items_tarja.codigo_diferencial
             no-lock no-error.
        if available conceptos_abacus Then     
            rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
        
        rb_tarja.valor_concepto[j] =  v_total. 

        v_total = 0.
    END. 
END.

v_total = 0.
FOR EACH rb_items_tarja WHERE 
    rb_items_tarja.id_reporte = v_nro_reporte AND
    rb_items_tarja.id_empresa = v-empresa AND
    rb_items_tarja.legajo = v-legajo AND
    rb_items_tarja.codigo_hs_complementarias <> 0 NO-LOCK BREAK BY codigo_hs_complementarias :
    v_total = v_total + rb_items_tarja.cant_hs_complementarias.
    IF LAST-OF(rb_items_tarja.codigo_hs_complementarias) THEN
    DO:
       j = j + 1.
       ASSIGN rb_tarja.id_concepto[j] = rb_items_tarja.codigo_hs_complementarias. 
       find first conceptos_abacus where conceptos_abacus.id_concepto = rb_items_tarja.codigo_hs_complementarias
             no-lock no-error.
        if available conceptos_abacus Then     
            rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
        
        rb_tarja.valor_concepto[j] =  v_total. 

        v_total = 0.
    END. 
END.

v_total = 0.
FOR EACH rb_items_tarja WHERE 
    rb_items_tarja.id_reporte = v_nro_reporte AND
    rb_items_tarja.id_empresa = v-empresa AND
    rb_items_tarja.legajo = v-legajo AND
    rb_items_tarja.codigo_licencias <> 0 NO-LOCK BREAK BY codigo_licencias :
    v_total = v_total + rb_items_tarja.cantidad.
    IF LAST-OF(rb_items_tarja.codigo_licencias) THEN
    DO:
       j = j + 1.
       ASSIGN rb_tarja.id_concepto[j] = rb_items_tarja.codigo_licencias. 
       find first conceptos_abacus where conceptos_abacus.id_concepto = rb_items_tarja.codigo_licencias
             no-lock no-error.
        if available conceptos_abacus Then     
            rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
        
        rb_tarja.valor_concepto[j] =  v_total. 

        v_total = 0.
    END. 
END.



  RUN imprime-reporte.

  for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
      rb_tarja.id_empresa = v-empresa AND
      rb_tarja.legajo = v-legajo :
    for each rb_items_tarja of rb_tarja:
       delete rb_items_tarja.
    end.
    delete rb_tarja.
  end. 


PROCEDURE imprime-reporte:
    define var v_filtro as character initial "".
    define var v_nombre_empresa as character.

      v_filtro = "rb_tarja.id_reporte = " + string(v_nro_reporte) + " and " +
          "rb_tarja.id_empresa = " + string(v-empresa) + " and " +
          "rb_tarja.legajo = " + STRING(v-legajo).
       
      IF v-sucursal <> 0  THEN
      DO:
          v_filtro = v_filtro + " and rb_tarja.id_sucursal = " + string(v-sucursal).
      END.
          
      IF v-sector <> 0  THEN
        DO:
            v_filtro = v_filtro + " and rb_tarja.id_sector = " + string(v-sector).
        END.

    find first liq_empresas where 
         liq_empresas.id_empresa_liq = v-empresa no-lock no-error.
    if available liq_empresas Then
       v_nombre_empresa = liq_empresas.descripcion.  
      ELSE
       v_nombre_empresa = "".

       vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".  

      RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(v_nro_reporte) + '.TXT'.
      
      RUN  aderb\_prntrb2(
                (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
         "resumen_tarjas_legajo2", /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         v-impresora,                   /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
          1,                              /* RB-NUMBER-COPIES  - zero */                  
          0,                              /* RB-BEGIN-PAGE - zero */
          0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Tarja Personal",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "v_general = " + STRING(v-desde-fecha) + "|" + STRING(v-hasta-fecha) + "|" +
         v_nombre_empresa + "|"
         /* RB-OTHER-PARAMETERS */,
         ""
         ).
  os-delete value(RB-MEMO-FILE).

END PROCEDURE.
