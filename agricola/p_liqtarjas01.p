define input parameter p_sector like liq_tarjas.id_sector.
define input parameter p_empresa like liq_tarjas.id_empresa.
define input parameter p_tipoliq like liq_legajo.tipo_liquidacion.
define input parameter p_fecha_desde like liq_tarjas.fecha.
define input parameter p_fecha_hasta like liq_tarjas.fecha.
define input parameter p_archivo as character.
 

define var v_cargo as integer.
DEFINE BUFFER bcon01 FOR conceptos_abacus.
DEFINE BUFFER bcon02 FOR conceptos_abacus.
DEFINE BUFFER bliqsec FOR liq_sectores.
DEF VAR v_liviana AS CHARACTER.
DEF VAR v_tipo_diferencial AS CHARACTER.
DEF VAR v_comp AS LOGICAL.
DEF VAR v_hs_norm_comp AS DECIMAL.

if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  PUT "Sector ".
  PUT (IF p_sector = 0 THEN "Todos" ELSE STRING(p_sector)).
  PUT SKIP.
  PUT "Tipo Liq ".
  PUT (IF p_tipoliq = "" THEN "Todos" ELSE p_tipoliq).
  PUT SKIP.

  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Sec Abacus;".
  put "Sec Gestion;".
  PUT "Tipo Liq;".
  put "Legajo;". 
  put "Nombre;".
  PUT "Categor¡a Empleado;".
  put "DNI/CUIL;". 
  put "Fecha;".
  put "Cod.Tarea;". 
  put "Tarea;". 
  put "Desc.Tarea ;".
  PUT "Liquida;".
  PUT "Categoria Tarea;".
  PUT "Grupo Tarea;".
  PUT "Subgrupo Tarea;".
  PUT "Ctro Costo Abacus;".
  put "Finca Gestion;". 
  PUT "Hs Trab;".
  PUT "Hs Norm;".
  PUT "Cod.Liq;".
  PUT "Desc.Liq;".
  PUT "Cantidad;".
  PUT "Cod.Cant;".
  PUT "Desc.Cant;".
  PUT "Tipo Dif;".
  PUT "Cant Dif;".
  PUT "Cod.Dif;".
  PUT "Desc.Dif;".
  PUT "Tareas Livianas;".
  PUT "Hs Acond Finca;".
  PUT "Hs Plus tareas automatico;".
  PUT "Hs Plus tareas trabajadas;".
  PUT "Hs Adic tareas tarbajadas;".
  PUT "Compensado;".
  PUT "Hs Extras;".
  PUT "Nro Tractor;".
  PUT "Nro Maq;".
  put skip.
  
  FOR EACH liq_items_tarjas WHERE
      (IF p_empresa <> 0 THEN liq_items_tarjas.id_empresa = p_empresa ELSE TRUE) and 
      (IF p_sector <> 0 THEN liq_items_tarjas.id_sector = p_sector ELSE true) and 
      liq_items_tarjas.fecha >= p_fecha_desde and
      liq_items_tarjas.fecha <= p_fecha_hasta AND
      liq_items_tarjas.id_tarea <> 0 no-lock,
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                              liq_legajos.legajo = liq_items_tarjas.legajo AND
      (IF p_tipoliq <> "" THEN liq_legajos.tipo_liquidacion = p_tipoliq ELSE TRUE) NO-LOCK BY liq_items_tarjas.id_empresa BY liq_items_tarjas.legajo BY liq_items_tarjas.fecha:
      
      find first liq_empresas where liq_empresas.id_empresa_liq = liq_items_tarjas.id_empresa no-lock no-error.
      
      find first liq_sectores where liq_sectores.id_empresa_liq = liq_items_tarjas.id_empresa AND
      liq_sectores.id_sector = liq_items_tarjas.id_sector no-lock no-error.
     
      find first liq_tareas of liq_items_tarjas no-lock no-error.
     find first origenes of liq_items_tarjas no-lock no-error.
     find first lotes_plantacion where lotes_plantacion.id_proveedor = liq_items_tarjas.id_proveedor and
     lotes_plantacion.id_origen = liq_items_tarjas.id_origen and
     lotes_plantacion.id_lote = liq_items_tarjas.id_lote
      no-lock no-error.

      FIND FIRST bliqsec OF liq_legajos NO-LOCK NO-ERROR.
      FIND FIRST liq_centros_costos OF liq_legajos NO-LOCK NO-ERROR.
     
      FIND FIRST conceptos_abacus WHERE conceptos_abacus.id_concepto = liq_items_tarjas.id_codigo_abacus NO-LOCK NO-ERROR.
      FIND FIRST bcon01 WHERE bcon01.id_concepto = liq_items_tarjas.id_codigo_abacus_cantidad NO-LOCK NO-ERROR.
      FIND FIRST bcon02 WHERE bcon02.id_concepto = liq_items_tarjas.id_codigo_abacus_dif NO-LOCK NO-ERROR.
      

      FIND FIRST tareas_livianas WHERE tareas_livianas.id_empresa = liq_items_tarjas.id_empresa AND
                                       tareas_livianas.legajo = liq_items_tarjas.legajo and
                                       tareas_livianas.fecha_hasta > liq_items_tarjas.fecha NO-LOCK NO-ERROR.
      IF AVAILABLE tareas_livianas THEN  v_liviana = "si".
                                   ELSE v_liviana = "no".
                                       
     FIND FIRST liq_categorias OF liq_legajos NO-LOCK NO-ERROR.

     FIND FIRST diferenciales OF liq_items_tarjas NO-LOCK NO-ERROR.
     IF AVAILABLE diferenciales THEN v_tipo_diferencial = diferenciales.descripcion.
                                ELSE v_tipo_diferencial = "".

     if available liq_tareas Then
     DO:
         FIND FIRST categorias_tareas OF liq_tareas NO-LOCK NO-ERROR.
         FIND FIRST liq_grupos_tareas OF liq_tareas NO-LOCK NO-ERROR.
         FIND FIRST liq_grupos_consultas OF liq_tareas NO-LOCK NO-ERROR.


         FIND FIRST resumen_compensado WHERE 
             resumen_compensado.id_empresa = liq_items_tarjas.id_empresa AND
             resumen_compensado.fecha = liq_items_tarjas.fecha AND
             resumen_compensado.legajo = liq_items_tarjas.legajo NO-LOCK NO-ERROR.
         IF AVAILABLE resumen_compensado THEN 
         DO:
             IF  (resumen_compensado.hs_categoria - resumen_compensado.hs_trab_norm) > 0 THEN
                 v_comp = YES.
               ELSE
                 v_comp = NO.
         END.
            ELSE 
              v_comp = NO.

         export delimiter ";" 
                liq_items_tarjas.id_empresa
                (if available liq_empresas Then liq_empresas.descripcion Else " ")
                (IF AVAILABLE bliqsec THEN bliqsec.descripcion ELSE " ")
                (if available liq_sectores Then liq_sectores.descripcion Else " ")
                (IF AVAILABLE liq_legajos THEN liq_legajos.tipo_liquidacion ELSE " ")
                liq_items_tarjas.legajo
                liq_items_tarjas.nombre
                (IF AVAILABLE liq_categorias THEN liq_categorias.descripcion ELSE " ")
                liq_items_tarjas.dni_cuil
                liq_items_tarjas.fecha
                liq_items_tarjas.id_tarea
                liq_tareas.abreviatura 
                liq_tareas.descripcion
                liq_tareas.liquida  
                (IF AVAILABLE categorias_tareas THEN categorias_tareas.descripcion ELSE " ")
                (IF AVAILABLE liq_grupos_tareas THEN liq_grupos_tareas.descripcion ELSE " ")
                (IF AVAILABLE liq_grupos_consulta THEN liq_grupos_consultas.descripcion ELSE " ")
                (IF AVAILABLE liq_centros_costos THEN liq_centros_costos.descripcion ELSE " ")
                if available origenes Then origenes.descripcion Else " "
                 liq_items_tarjas.cant_horas
                 liq_items_tarjas.cant_hs_norm
                 liq_items_tarjas.id_codigo_abacus
                (IF AVAILABLE conceptos_abacus THEN conceptos_abacus.descripcion ELSE " ")
                liq_items_tarjas.cantidad
                liq_items_tarjas.id_codigo_abacus_cantidad
                (IF AVAILABLE bcon01 THEN bcon01.descripcion ELSE " ")
                v_tipo_diferencial
                (IF liq_items_tarjas.id_codigo_abacus_dif <> 0 THEN  liq_items_tarjas.cant_hs_norm ELSE 0)
                liq_items_tarjas.id_codigo_abacus_dif
                (IF AVAILABLE bcon02 THEN bcon02.descripcion ELSE " ")
                v_liviana
                liq_items_tarjas.hs_acond_finca
                liq_items_tarjas.hs_plus_tareas_automatico
                liq_items_tarjas.hs_plus_tareas_trabajadas
                liq_items_tarjas.hs_adicionales_tareas
                v_comp
                liq_items_tarjas.cant_hs_extra
                liq_items_tarjas.nro_tractor
                liq_items_tarjas.nro_maquina.

     END.                   .
  end.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
