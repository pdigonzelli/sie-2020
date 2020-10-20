DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_sector AS INTEGER.
DEF INPUT PARAMETER p_fecha_desde AS DATE.
DEF INPUT PARAMETER p_fecha_hasta AS DATE.
define input parameter v_feriado as date.

define var p_archivo as character.

define var v_horas as decimal.

DEFINE VAR v_dias AS INTEGER.
DEF VAR v_dias_feriados AS INTEGER.
DEF VAR v_dias_feriados-1 AS INTEGER.
DEF VAR v_dias_feriados-2 AS INTEGER.

define var v_feriado_cond-a as integer.
define var v_feriado_cond-b as integer.

define var v_feriado_cond-a-1 as integer.
define var v_feriado_cond-b-1 as integer.
define var v_feriado_cond-a-2 as integer.
define var v_feriado_cond-b-2 as integer.

 
define temp-table rb_resumen_3eros 
    FIELD id_empresa AS INTEGER
    FIELD id_sucursal AS INTEGER
    FIELD id_sector AS INTEGER
    FIELD legajo AS INTEGER
    FIELD dni_cuil AS CHARACTER
    FIELD nombre AS CHARACTER
    field cant_dias as integer
    field cant_dias_feriado as integer
    field cant_feriado_cond-a as integer
    field cant_feriado_cond-b as integer.

define var v_cargo as integer.
define var v_estado as logical.
DEF BUFFER b_ult FOR liq_tarjas.
DEF VAR v-nro AS INTEGER.
DEF VAR v_codigo_feriado AS INTEGER INITIAL 700.
DEF VAR v_codigo_gremio AS INTEGER INITIAL 903.
DEF VAR v_codigo AS INTEGER.
DEF VAR v_proveedor AS INTEGER.
DEF VAR v_origen AS INTEGER.

IF v_feriado = DATE("08/10/16") THEN v_codigo = v_codigo_gremio.
                                ELSE v_codigo = v_codigo_feriado.

DEF TEMP-TABLE t-lluvia
    FIELD fecha AS DATE.

FOR EACH t-lluvia:
    DELETE t-lluvia.
END.

FOR EACH liq_items_tarjas WHERE liq_items_tarjas.fecha >= p_fecha_desde AND
    liq_items_tarjas.fecha <= p_fecha_hasta AND
    id_tarea = 1103 NO-LOCK, FIRST liq_tarjas OF liq_items_tarjas NO-LOCK
     BREAK BY liq_items_tarjas.fecha:
    IF LAST-OF(liq_items_tarjas.fecha) THEN
    DO:
        CREATE t-lluvia.
        ASSIGN t-lluvia.fecha = liq_items_tarjas.fecha.
    END.
END.




for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   


p_archivo = "z:\temp\jorpers.txt".


for each liq_tarjas where 
    liq_tarjas.id_empresa = p_empresa and
    liq_tarjas.fecha >= p_fecha_desde and
    liq_tarjas.fecha <= p_fecha_hasta no-lock,
    each liq_items_tarjas of liq_tarjas where id_tarea <> 0 and
         liq_items_tarjas.id_tarea <> 1092 and /* Feriado */
         liq_items_tarjas.id_tarea <> 1101 AND
         liq_items_tarjas.id_tarea <> 1102 AND
         liq_items_tarjas.dni_cuil <> "" no-lock
    break by liq_items_tarjas.id_empresa by 
    liq_items_tarjas.legajo by liq_items_tarjas.fecha:
      
    
       find first rb_resumen_3eros where 
                  rb_resumen_3eros.id_empresa = liq_items_tarjas.id_empresa and
                  rb_resumen_3eros.legajo = liq_items_tarjas.legajo  no-error.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_tarjas.id_empresa 
                    rb_resumen_3eros.id_sucursal = liq_items_tarjas.id_sucursal
                    rb_resumen_3eros.id_sector = liq_items_tarjas.id_sector 
                    rb_resumen_3eros.legajo = liq_items_tarjas.legajo
                    rb_resumen_3eros.dni_cuil = liq_items_tarjas.dni_cuil
                    rb_resumen_3eros.nombre = liq_items_tarjas.nombre.
          end.
        
         v_horas = v_horas + liq_items_tarjas.cant_horas.
        
         if last-of(liq_items_tarjas.fecha) Then
            do:
                 if v_horas > 0 Then
                    do:
                      if liq_items_tarjas.fecha = v_feriado and 
                          liq_items_tarjas.id_tarea <> 1086  and 
                          liq_items_tarjas.id_tarea <> 1095  Then
                              do:
                                v_dias_feriados = v_dias_feriados + 1.
                                if day(liq_items_tarjas.fecha) <= 15 Then
                                     v_dias_feriados-1 = v_dias_feriados-1 + 1.
                                    Else 
                                     v_dias_feriados-2 = v_dias_feriados-2 + 1.
                              end.  
                    end.
                v_horas = 0.    
            end.
           
           if last-of(liq_items_tarjas.legajo) Then
              do:
                 if v_dias_feriados = 0 Then
                    do: 
                        run ver-condicion-periodo-anterior  (input liq_tarjas.id_empresa,
                                                             input liq_items_tarjas.legajo,
                                                             output v_estado).
                         if v_estado Then
                           do:
                             v_feriado_cond-a = v_feriado_cond-a + 1.                                  
                             if day(v_feriado) <= 15 Then
                                v_feriado_cond-a-1 = v_feriado_cond-a-1 + 1.                                  
                               Else
                                v_feriado_cond-a-2 = v_feriado_cond-a-2 + 1.                                  
                                
       
                           end. 
                    end.
                            
                 if v_dias_feriados = 0 and v_feriado_cond-a = 0 Then
                    do:
                        run ver-condicion-periodo-siguiente (input liq_tarjas.id_empresa,
                                                             input liq_items_tarjas.legajo,
                                                             output v_estado).
                        if v_estado Then
                          do:
                           v_feriado_cond-b = v_feriado_cond-b + 1.                                  
                           if day(v_feriado) <= 15 Then
                              v_feriado_cond-b-1 = v_feriado_cond-b-1 + 1.                                  
                             Else 
                              v_feriado_cond-b-2 = v_feriado_cond-b-2 + 1.                                  
                          end. 
                    end.                                    
                            
               /**********************************************/
                 
                 
                 rb_resumen_3eros.cant_dias = v_dias.
                 rb_resumen_3eros.cant_dias_feriado = v_dias_feriados.
                 rb_resumen_3eros.cant_feriado_cond-a = v_feriado_cond-a.
                 rb_resumen_3eros.cant_feriado_cond-b = v_feriado_cond-b.
                 
                 v_dias = 0.
                 v_dias_feriados = 0.
                 v_feriado_cond-a = 0.
                 v_feriado_cond-b = 0.
                 v_horas = 0.    

              end. 
    end.


/* Genero feriados seg£n corresponda */
DEF BUFFER b-control FOR liq_tarjas.
DEF BUFFER b-items FOR liq_items_tarjas.
DEF BUFFER b-new FOR liq_items_tarjas.
DEF BUFFER b-items-1 FOR liq_items_tarjas.


FOR EACH rb_resumen_3eros NO-LOCK,
    FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = rb_resumen_3eros.id_empresa AND
    liq_legajos.legajo = rb_resumen_3eros.legajo NO-LOCK:

    FIND FIRST liq_ccostos WHERE liq_ccostos.id_centro_costo = liq_legajos.id_ccostos_liq NO-LOCK NO-ERROR.
    IF AVAILABLE liq_ccostos THEN
    DO:
        v_proveedor = liq_ccostos.id_proveedor.
        v_origen = liq_ccostos.id_origen.
    END.
    ELSE
    DO:
        IF rb_resumen_3eros.id_sucursal = 4 THEN
            ASSIGN v_proveedor = 1
                   v_origen = 4.
        ELSE
            ASSIGN v_proveedor = 1
                   v_origen = 5.
    END.

    FIND FIRST b-control WHERE
          b-control.id_empresa = rb_resumen_3eros.id_empresa AND
          b-control.id_sucursal = rb_resumen_3eros.id_sucursal AND
          b-control.id_sector = rb_resumen_3eros.id_sector AND
          b-control.fecha = v_feriado AND
          b-control.id_tipo_planilla = 6 AND
          b-control.id_proveedor = v_proveedor AND
          b-control.id_origen = v_origen NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b-control THEN
     DO:
         find last b_ult use-index suc_sector_tipo_nro where  
         b_ult.id_sucursal = rb_resumen_3eros.id_sucursal and 
         b_ult.id_sector = rb_resumen_3eros.id_sector AND
         b_ult.id_tipo_planilla = 6 no-lock no-error.
         IF AVAILABLE b_ult THEN
              v-nro = b_ult.nro_planilla + 1.
            ELSE 
              v-nro = 1.
         CREATE b-control.
         ASSIGN 
         b-control.id_empresa = rb_resumen_3eros.id_empresa 
         b-control.id_sector = rb_resumen_3eros.id_sector
         b-control.id_sucursal = rb_resumen_3eros.id_sucursal
         b-control.fecha = v_feriado 
         b-control.id_tipo_planilla = 6 
         b-control.nro_planilla = v-nro
         b-control.id_proveedor = v_proveedor 
         b-control.id_origen = v_origen.
     END.


  IF rb_resumen_3eros.cant_dias_feriado <> 0 THEN
    DO:
      FIND FIRST b-items OF b-control WHERE
          b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
      IF NOT AVAILABLE b-items THEN
      DO:
          /* Chequeo que no exista el feriado cargado en otro sector */

          FIND FIRST b-items-1 WHERE
                b-items-1.id_empresa = rb_resumen_3eros.id_empresa AND
                b-items-1.legajo = rb_resumen_3eros.legajo AND
                b-items-1.fecha = v_feriado and
                b-items-1.id_tarea = 1092 NO-LOCK NO-ERROR.
          IF NOT AVAILABLE b-items-1 THEN
          DO:
              CREATE b-new.
              BUFFER-COPY b-control TO b-new.
              ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                     b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                     b-new.nombre = rb_resumen_3eros.nombre
                     b-new.id_tarea = 1092
                     b-new.id_codigo_abacus_cantidad = v_codigo
                     b-new.cantidad = 1.
              ASSIGN b-new.c_fecha = TODAY
                     b-new.c_hora = STRING(TIME,"HH:MM:SS")
                     b-new.c_usuario = "auto". 
          END.
      END.
    END.

    IF rb_resumen_3eros.cant_feriado_cond-a <> 0 THEN
    DO:
        FIND FIRST b-items OF b-control WHERE
            b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-items THEN
        DO:
            /* Chequeo que no exista el feriado cargado en otro sector */

            FIND FIRST b-items-1 WHERE
                  b-items-1.id_empresa = rb_resumen_3eros.id_empresa AND
                  b-items-1.legajo = rb_resumen_3eros.legajo AND
                  b-items-1.fecha = v_feriado and
                  b-items-1.id_tarea = 1101 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b-items-1 THEN
            DO:
                CREATE b-new.
                BUFFER-COPY b-control TO b-new.
                ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                       b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                       b-new.nombre = rb_resumen_3eros.nombre
                       b-new.id_tarea = 1101
                       b-new.id_codigo_abacus_cantidad = v_codigo
                       b-new.cantidad = 1.
                ASSIGN b-new.c_fecha = TODAY
                       b-new.c_hora = STRING(TIME,"HH:MM:SS")
                       b-new.c_usuario = "auto". 
            END.
        END.
    END.

    IF rb_resumen_3eros.cant_feriado_cond-b <> 0 THEN
    DO:
        FIND FIRST b-items OF b-control WHERE
            b-items.legajo = rb_resumen_3eros.legajo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-items THEN
        DO:

            /* Chequeo que no exista el feriado cargado en otro sector */

            FIND FIRST b-items-1 WHERE
                  b-items-1.id_empresa = rb_resumen_3eros.id_empresa AND
                  b-items-1.legajo = rb_resumen_3eros.legajo AND
                  b-items-1.fecha = v_feriado and
                  b-items-1.id_tarea = 1102 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b-items-1 THEN
            DO:
                CREATE b-new.
                BUFFER-COPY b-control TO b-new.
                ASSIGN b-new.legajo = rb_resumen_3eros.legajo
                       b-new.dni_cuil = rb_resumen_3eros.dni_cuil
                       b-new.nombre = rb_resumen_3eros.nombre
                       b-new.id_tarea = 1102
                       b-new.id_codigo_abacus_cantidad = v_codigo
                       b-new.cantidad = 1.
                ASSIGN b-new.c_fecha = TODAY
                       b-new.c_hora = STRING(TIME,"HH:MM:SS")
                       b-new.c_usuario = "auto". 
            END.
        END.
    END.
END.
MESSAGE "Planilla de Feriados generada" VIEW-AS ALERT-BOX.



procedure ver-condicion-periodo-anterior:
define input parameter v_empresa as integer.
define input parameter v_legajo like liq_items_tarjas.legajo.
define output parameter p_estado as logical initial no.

define buffer b_control for liq_tarjas.
define buffer b_items for liq_items_tarjas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.

DEF VAR v_cant_dias AS INTEGER.


/* Federico Diaz 12/04/2011 se consideran dias habiles no corridos */
/* Jose Lopez 09/05/2012 se excluyen los d¡as de lluvia resgitrados */
/* Jose Lopez 25/05/2016 se incluyen los dias de lluvia registrados */

v_fecha-1 = v_feriado.
v_cant_dias = 0.

repeat:
    if v_cant_dias = 10 Then leave.
    v_fecha-1 = v_fecha-1 - 1.
    
    FIND FIRST feriados_agricola WHERE feriados_agricola.fecha = v_fecha-1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE feriados_agricola THEN
      DO:
        if weekday(v_fecha-1) <> 1 Then /* Distinto de domingo */
        DO:
            /*****
            FIND FIRST t-lluvia WHERE t-lluvia.fecha = v_fecha-1 NO-LOCK NO-ERROR. /* No lluvia */
            IF NOT AVAILABLE t-lluvia  THEN   ***/
              v_cant_dias = v_cant_dias + 1.
        END.
      END.
end.

v_fecha-2 = v_feriado.

v_cuenta_dias = 0.
for each b_control where 
    b_control.id_empresa = v_empresa and
    b_control.fecha >= v_fecha-1 and
    b_control.fecha < v_fecha-2 no-lock,
    each b_items of b_control where
    b_items.legajo = v_legajo and 
    b_items.id_tarea <> 0 and
    b_items.id_tarea <> 1092 /* Feriado */ and
    b_items.id_tarea <> 1101 AND
    b_items.id_tarea <> 1102 AND
    (b_items.cantidad <> 0 or b_items.cant_horas <> 0) NO-LOCK,
    first liq_tareas of b_items no-lock 
    break by b_items.fecha:
   
    IF (liq_tareas.id_tarea < 1084  OR
       liq_tareas.id_tarea > 1102) AND
        liq_tareas.id_tarea <> 1104 AND
        liq_tareas.id_tarea <> 1271 AND
        liq_tareas.id_tarea <> 1272  THEN
    DO:
        if last-of(b_items.fecha) Then
            v_cuenta_dias = v_cuenta_dias + 1.
    END.
END.
    if v_cuenta_dias >= 6 Then p_estado = yes.
end procedure. 



procedure ver-condicion-periodo-siguiente:
define input parameter v_empresa as integer.
define input parameter v_legajo like liq_items_control_finca.legajo.
define output parameter p_estado as logical initial no.

define buffer b_control for liq_tarjas.
define buffer b_items for liq_items_tarjas.
define var v_fecha-1 as date.
define var v_fecha-2 as date.
define var v_cuenta_dias as integer.
define var v_vispera as logical initial no.
DEFINE VAR v_cant_dias AS INTEGER.

v_fecha-1 = v_feriado - 1.
v_fecha-2 = v_feriado.

v_cant_dias = 0.
repeat:
    if v_cant_dias = 5 Then leave.
    v_fecha-2 = v_fecha-2 + 1.
    
    FIND FIRST feriados_agricola WHERE feriados_agricola.fecha = v_fecha-2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE feriados_agricola THEN
      DO:
        if weekday(v_fecha-2) <> 1 Then /* Distinto de domingo */
        DO:
            /****
            FIND FIRST t-lluvia WHERE t-lluvia.fecha = v_fecha-2 NO-LOCK NO-ERROR. /* No lluvia */
            IF NOT AVAILABLE t-lluvia  THEN  ***/
              v_cant_dias = v_cant_dias + 1.
        END.
      END.
end.


     v_cuenta_dias = 0.
     v_vispera = no. 
    for each b_control where 
            b_control.id_empresa = v_empresa and
            b_control.fecha >= v_fecha-1 and
            b_control.fecha < v_fecha-2 no-lock,
            each b_items of b_control where 
            b_items.id_tarea <> 0 and
            b_items.id_tarea <> 1092 and /* Feriado */
            b_items.id_tarea <> 1101 AND
            b_items.id_tarea <> 1102 AND
            b_items.legajo = v_legajo and
            (b_items.cantidad <> 0 OR b_items.cant_horas <> 0) no-lock,
             first liq_tareas of b_items  no-lock break by  b_items.fecha:
                     
        IF (liq_tareas.id_tarea < 1084  OR
            liq_tareas.id_tarea > 1102) AND
            liq_tareas.id_tarea <> 1104 AND
            liq_tareas.id_tarea <> 1271 AND
            liq_tareas.id_tarea <> 1272  THEN
            DO:
             if last-of(b_items.fecha) Then
                do:
                   if b_items.fecha = v_fecha-1 Then               
                      v_vispera = yes.
                   v_cuenta_dias = v_cuenta_dias + 1.
                   if v_cuenta_dias = 2 Then
                      leave.
                end.  
        END.
    end.
    if v_vispera = YES AND v_cuenta_dias = 2 Then p_estado = yes.
end procedure. 



