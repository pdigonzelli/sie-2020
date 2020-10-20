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
 DEFINE VAR v_nro_reporte AS INTEGER.

  def var v_codigo as integer.
  def var v_codigo-1 as integer.
  def var v_codigo-2 as integer.
  def var v_resumen as character.
  define var v_cargo like liq_legajos.id_cargo.
  define var v_centro like liq_legajos.id_centro_costo.

  define temp-table t-personal
     FIELD id_empresa LIKE liq_items_control_tareas.id_empresa 
     field legajo like liq_items_control_tareas.legajo
     field nombre like liq_items_control_tareas.nombre
     field dni_cuil like liq_items_control_tareas.dni_cuil
     field id_concepto like r_tareas_unidades.id_concepto
     field cantidad like liq_items_control_tareas.cantidad
     field id_centro_costo like liq_legajos.id_centro_costo.

    DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

{s_varsis.i}


if v-empresa = 0 Then
   do:
     message "Debe ingresar una empresa" view-as alert-box.
     return.
   end.

if v-sucursal = 0 Then
   do:
     message "Debe ingresar una sucursal" view-as alert-box.
     return.
   end.

 v_nro_reporte = RANDOM(1,1000000).

 for each t-personal:
   delete t-personal.
 end.
 
 for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
     rb_tarja.id_empresa = v-empresa AND
     rb_tarja.id_sucursal = v-sucursal AND
     rb_tarja.id_sector = v-sector AND
     rb_tarja.legajo = v-legajo:
   for each rb_items_tarja of rb_tarja:
      delete rb_items_tarja.
   end.
   delete rb_tarja.
 end.      
 
if v-finca = YES  THEN
 do:
 for each liq_items_control_tareas where 
      liq_items_control_tareas.id_empresa = v-empresa and 
      liq_items_control_tareas.id_sucursal = v-sucursal and 
      liq_items_control_tareas.fecha >= v-desde-fecha and 
      liq_items_control_tareas.fecha <= v-hasta-fecha and
      liq_items_control_tarea.id_sector = v-sector AND
      liq_items_control_tareas.id_tipo_planilla <> 4 AND
      liq_items_control_tareas.legajo = v-legajo AND
      (liq_items_control_tareas.cant_jornal <> 0 OR liq_items_control_tareas.cant_horas <> 0 OR liq_items_control_tareas.cantidad <> 0) 
      no-lock,
     first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo and
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
     by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo
      by liq_items_control_tareas.nombre by liq_items_control_tareas.fecha :

      if liq_items_control_tareas.id_tarea = 0 Then next.


          v_cargo = liq_legajos.id_cargo.
          v_centro = liq_legajos.id_centro_costo.
        
        find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.

    {liq-calcula-rhpro-agricola.i} 
     
     FIND FIRST rb_items_tarja WHERE
         rb_items_tarja.id_reporte = v_nro_reporte AND
         rb_items_tarja.id_empresa = liq_items_control_tareas.id_empresa AND
         rb_items_tarja.id_sucursal = liq_items_control_tareas.id_sucursal  AND
         rb_items_tarja.id_sector = liq_items_control_tareas.id_sector AND
         rb_items_tarja.fecha = liq_items_control_tareas.fecha  AND
         rb_items_tarja.id_proveedor = liq_items_control_tareas.id_proveedor AND
         rb_items_tarja.id_origen  = liq_items_control_tareas.id_origen  AND
         rb_items_tarja.legajo = liq_items_control_tareas.legajo AND
         rb_items_tarja.nombre  = liq_items_control_tareas.nombre AND
         rb_items_tarja.id_tarea = liq_items_control_tareas.id_tarea AND
         rb_items_tarja.id_lote = liq_items_control_tareas.id_lote AND
         rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion NO-ERROR.
    IF NOT AVAILABLE rb_items_tarja THEN
    DO:
  
     create rb_items_tarja.
     assign rb_items_tarja.id_reporte = v_nro_reporte
            rb_items_tarja.id_empresa = liq_items_control_tareas.id_empresa
            rb_items_tarja.id_sucursal = liq_items_control_tareas.id_sucursal
            rb_items_tarja.id_sector = liq_items_control_tareas.id_sector
            rb_items_tarja.legajo = liq_items_control_tareas.legajo
            rb_items_tarja.nombre = liq_items_control_tareas.nombre
            rb_items_tarja.fecha = liq_items_control_tareas.fecha
            rb_items_tarja.id_tarea = liq_items_control_tareas.id_tarea 
            rb_items_tarja.id_proveedor = liq_items_control_tareas.id_proveedor 
            rb_items_tarja.id_origen = liq_items_control_tareas.id_origen 
            rb_items_tarja.id_lote = liq_items_control_tareas.id_lote
            rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion.
    END.

        ASSIGN rb_items_tarja.cant_jornal = liq_items_control_tareas.cant_jornal_norm
            rb_items_tarja.cant_horas = liq_items_control_tareas.cant_horas
            rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion 
            rb_items_tarja.cantidad = liq_items_control_tareas.cantidad
            rb_items_tarja.nro_tractor = liq_items_control_tareas.nro_tractor
            rb_items_tarja.nro_maquina = liq_items_control_tareas.nro_maquina.
    
     
   /*  display rb_items_tarja.legajo v_codigo v_codigo-1 v_codigo-2.
     pause.*/
            
    find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of liq_items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  liq_items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  liq_items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  liq_items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.



        
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                    t-personal.id_empresa = liq_items_control_tareas.id_empresa
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_costo = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_jornal_norm.
       end.
    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = liq_items_control_tareas.id_empresa 
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_costo = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = liq_items_control_tareas.id_empresa 
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_costo = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cantidad.
       
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       liq_items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end. 
Else
 do:

  for each liq_items_control_tareas where 
      liq_items_control_tareas.id_empresa = v-empresa and 
      liq_items_control_tareas.id_sucursal = v-sucursal and 
      liq_items_control_tareas.fecha >= v-desde-fecha and 
      liq_items_control_tareas.fecha <= v-hasta-fecha and
      liq_items_control_tareas.id_sector = v-sector AND
      liq_items_control_tareas.id_tipo_planilla <> 4 AND
      liq_items_control_tareas.legajo = v-legajo AND
      (liq_items_control_tareas.cant_jornal <> 0 OR liq_items_control_tareas.cant_horas <> 0 OR liq_items_control_tareas.cantidad <> 0) 
      no-lock, first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v-tipo <> "" THEN liq_legajos.tipo_liquidacion = v-tipo ELSE TRUE)  NO-LOCK
      by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo
      by liq_items_control_tareas.nombre by liq_items_control_tareas.fecha :
      
      
      if liq_items_control_tareas.id_tarea = 0 Then next.
      
          v_cargo = liq_legajos.id_cargo.
          v_centro = liq_legajos.id_centro_costo.
        
        find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.

     {liq-calcula-rhpro-agricola.i} 
         FIND FIRST rb_items_tarja WHERE
             rb_items_tarja.id_reporte = v_nro_reporte AND
             rb_items_tarja.id_empresa = liq_items_control_tareas.id_empresa AND
             rb_items_tarja.id_sucursal = liq_items_control_tareas.id_sucursal  AND
             rb_items_tarja.id_sector = liq_items_control_tareas.id_sector AND
             rb_items_tarja.fecha = liq_items_control_tareas.fecha  AND
             rb_items_tarja.id_proveedor = liq_items_control_tareas.id_proveedor AND
             rb_items_tarja.id_origen  = liq_items_control_tareas.id_origen  AND
             rb_items_tarja.legajo = liq_items_control_tareas.legajo AND
             rb_items_tarja.nombre  = liq_items_control_tareas.nombre AND
             rb_items_tarja.id_tarea = liq_items_control_tareas.id_tarea AND
             rb_items_tarja.id_lote = liq_items_control_tareas.id_lote AND
             rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion NO-ERROR.
        IF NOT AVAILABLE rb_items_tarja THEN
        DO:

         create rb_items_tarja.
         assign rb_items_tarja.id_reporte = v_nro_reporte
                rb_items_tarja.id_empresa = liq_items_control_tareas.id_empresa
                rb_items_tarja.id_sucursal = liq_items_control_tareas.id_sucursal
                rb_items_tarja.id_sector = liq_items_control_tareas.id_sector
                rb_items_tarja.legajo = liq_items_control_tareas.legajo
                rb_items_tarja.nombre = liq_items_control_tareas.nombre
                rb_items_tarja.fecha = liq_items_control_tareas.fecha
                rb_items_tarja.id_tarea = liq_items_control_tareas.id_tarea 
                rb_items_tarja.id_proveedor = liq_items_control_tareas.id_proveedor 
                rb_items_tarja.id_origen = liq_items_control_tareas.id_origen 
                rb_items_tarja.id_lote = liq_items_control_tareas.id_lote
                rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion.
        END.

            ASSIGN rb_items_tarja.cant_jornal = liq_items_control_tareas.cant_jornal_norm
                rb_items_tarja.cant_horas = liq_items_control_tareas.cant_horas
                rb_items_tarja.id_unidad_liquidacion = liq_items_control_tareas.id_unidad_liquidacion 
                rb_items_tarja.cantidad = liq_items_control_tareas.cantidad
                rb_items_tarja.nro_tractor = liq_items_control_tareas.nro_tractor
                rb_items_tarja.nro_maquina = liq_items_control_tareas.nro_maquina.

            
    find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of liq_items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  liq_items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  liq_items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  liq_items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.


    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = liq_items_control_tareas.id_empresa 
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_costo = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_jornal_norm.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = liq_items_control_tareas.id_empresa 
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_costo = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
       t-personal.id_empresa = liq_items_control_tareas.id_empresa AND
       t-personal.legajo = liq_items_control_tareas.legajo and
       t-personal.nombre = liq_items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = liq_items_control_tareas.id_empresa 
                    t-personal.legajo = liq_items_control_tareas.legajo
                    t-personal.nombre = liq_items_control_tareas.nombre
                    t-personal.dni_cuil = liq_items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_costo = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + liq_items_control_tareas.cantidad.
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       liq_items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end.  

j = 0.
for each t-personal no-lock break by  t-personal.legajo by t-personal.nombre 
   by t-personal.id_concepto:
   find first rb_tarja where rb_tarja.id_reporte = v_nro_reporte and
   rb_tarja.id_empresa = t-personal.id_empresa AND
   rb_tarja.id_sucursal = v-sucursal AND
   rb_tarja.id_sector = v-sector AND
   rb_tarja.legajo = t-personal.legajo and
    rb_tarja.nombre = t-personal.nombre no-error.
   if not available rb_tarja Then
      do:
         create rb_tarja.
         rb_tarja.id_reporte = v_nro_reporte.
         rb_tarja.id_empresa = v-empresa.
         rb_tarja.id_sucursal = v-sucursal.
         rb_tarja.id_sector = v-sector.
         rb_tarja.legajo = t-personal.legajo.
         find first liq_legajos where liq_legajos.id_empresa_liq = v-empresa and liq_legajos.legajo = t-personal.legajo no-lock no-error.
         if available liq_legajos Then
            do:
              rb_tarja.nombre = t-personal.nombre.
              rb_tarja.dni_cuil = liq_legajos.cuil.
              rb_tarja.id_cargo = liq_legajos.id_cargo.
              rb_tarja.id_centro_abacus = liq_legajos.id_centro_costo.
              
              find first liq_centros_costos where liq_centros_costos.id_centro_costo = rb_tarja.id_centro_abacus no-lock
              no-error.    
              if available liq_centros_costos Then
                 rb_tarja.nombre_centro_abacus = liq_centros_costos.descripcion.   
            end.  
           Else
             do:
               rb_tarja.nombre = t-personal.nombre.
               rb_tarja.dni_cuil = t-personal.dni_cuil.
             end.
         j = 0.
      end. 
      j = j + 1.
      
      if j <= 20 Then
      do:
      rb_tarja.id_concepto[j] = t-personal.id_concepto.  
      rb_tarja.valor_concepto[j] =  t-personal.cantidad. 
      find first liq_conceptos where liq_conceptos.id_concepto = t-personal.id_concepto 
           no-lock no-error.
      if available liq_conceptos Then     
         rb_tarja.nombre_concepto[j] = liq_conceptos.descripcion.
      end.
      Else
        message rb_tarja.legajo rb_tarja.nombre view-as alert-box.   
end.

find first rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte and
    rb_tarja.id_empresa = v-empresa AND
    rb_tarja.id_sucursal = v-sucursal AND
    rb_tarja.id_sector = v-sector no-lock no-error.
if available rb_tarja Then
  run imprime-reporte.
Else
  message "No hay datos registrados para esta selección" view-as alert-box
  title "Atención".  


  for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
      rb_tarja.id_empresa = v-empresa AND
      rb_tarja.id_sucursal = v-sucursal AND
      rb_tarja.id_sector = v-sector:
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
          "rb_tarja.id_sucursal = " + string(v-sucursal) + " and " +
          "rb_tarja.id_sector = " + STRING(v-sector) + " and " +
          "rb_tarja.legajo = " + STRING(v-legajo).

    find first liq_empresas where 
         liq_empresas.id_empresa_liq = v-empresa no-lock no-error.
    if available liq_empresas Then
       v_nombre_empresa = liq_empresas.descripcion.  

      RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(v_nro_reporte) + '.TXT'.
      /* vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".  */
      
      RUN  aderb\_prntrb2(
                (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
         "resumen_tareas_legajo2", /* RB-REPORT-NAME */
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
         "v_general = " + STRING(v-desde-fecha) + ";" + STRING(v-hasta-fecha) + ";" +
         v_nombre_empresa + ";"
         /* RB-OTHER-PARAMETERS */,
         ""
         ).
  os-delete value(RB-MEMO-FILE).

END PROCEDURE.
