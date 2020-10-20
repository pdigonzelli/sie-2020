DEF INPUT PARAMETER p-tipo AS INTEGER NO-UNDO.
DEF INPUT PARAMETER p-empresa AS INTEGER NO-UNDO.
DEF INPUT PARAMETER p-cuadrilla AS INTEGER NO-UNDO.
define input parameter p-fecha as date no-undo.
define input parameter p-referencia as date no-undo.

DEF VAR v_causa AS INTEGER. 
DEF VAR v_genera AS LOGICAL.
DEF VAR v_existen AS INTEGER.
DEF BUFFER bctrol For liq_control_finca.
DEF BUFFER bitemsctrol FOR  liq_items_control_finca.
DEF BUFFER bctrollote FOR liq_control_finca_lotes.
DEF BUFFER bctrol-01 FOR liq_control_finca.
DEF VAR v_nro AS INTEGER.
DEF VAR v_cuenta AS INTEGER.


IF p-empresa = 0  THEN
DO:
    MESSAGE "Debe ingresar una empresa particular para la prueba" VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

IF p-cuadrilla = 0  THEN
DO:
    MESSAGE "Debe ingresar una cuadrilla particular para la prueba" VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.


CASE p-tipo:
    WHEN 1 THEN v_causa = 6. /* lluvia */
    WHEN 2 THEN v_causa = 8. /*ausente sin aviso */
END CASE.

v_existen = 0.
FOR EACH bitemsctrol WHERE
    bitemsctrol.id_empresa_cosechera = p-empresa AND
    bitemsctrol.id_cuadrilla = p-cuadrilla AND
    bitemsctrol.fecha = p-fecha  and
    bitemsctrol.id_causa_ausencia = v_causa NO-LOCK:
    v_existen = v_existen + 1.

END.

IF v_existen <> 0  THEN
DO:
    MESSAGE "Existen " + STRING(v_existen) + " registros para esa cuadrilla, fecha y causa de ausencia" SKIP
        "No se ejecuta el proceso " VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.


v_existen = 0.
FOR EACH bitemsctrol WHERE
    bitemsctrol.id_empresa_cosechera = p-empresa AND
    bitemsctrol.id_cuadrilla = p-cuadrilla AND
    bitemsctrol.fecha = p-fecha  NO-LOCK:
    v_existen = v_existen + 1.

END.

IF v_existen = 0 AND p-tipo = 2 THEN
DO:
    MESSAGE "No Existen registros de cosecha para esa cuadrilla y fecha de registro" SKIP
        "No se ejecuta el proceso para generar ausente con aviso " skip
        "Modifique fecha registro" VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

IF v_existen <> 0 AND p-tipo = 1 THEN
DO:
    MESSAGE "Existen " + STRING(v_existen) + " registros de cosecha para esa cuadrilla y fecha" SKIP
            "No se permite generar planilla de lluvia" SKIP
            "No se ejecuta el proceso " VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.


v_genera = NO.
FIND FIRST liq_control_finca WHERE 
    liq_control_finca.id_empresa = p-empresa AND
    liq_control_finca.id_cuadrilla = p-cuadrilla AND
    liq_control_finca.fecha = p-referencia NO-LOCK NO-ERROR.

IF AVAILABLE liq_control_finca THEN
DO:
     FIND FIRST bctrol WHERE 
          bctrol.id_empresa = liq_control_finca.id_empresa AND
          bctrol.id_cuadrilla = liq_control_finca.id_cuadrilla AND
          bctrol.fecha = p-fecha NO-LOCK NO-ERROR.
     IF NOT AVAILABLE bctrol THEN
     DO:
          IF v_causa = 6 THEN
          DO:
              v_nro = 0.            
              FIND LAST bctrol-01 WHERE
                    bctrol-01.nro_planilla < 1000000 USE-INDEX solo_nro_planilla NO-LOCK NO-ERROR.
              IF AVAILABLE bctrol-01 THEN
                    v_nro = bctrol-01.nro_planilla + 1.
              
             CREATE bctrol.
             BUFFER-COPY liq_control_finca EXCEPT fecha nro_planilla observaciones TO bctrol.
             ASSIGN bctrol.fecha = p-fecha
                    bctrol.nro_planilla = v_nro
                    bctrol.observaciones = "Planilla de lluvia automatica"
                    bctrol.c_usuario = "c-genauto"
                    bctrol.c_fecha = DATE(TODAY)
                    bctrol.c_hora = string(TIME,"HH:MM:SS").
              
             v_genera = YES.
          END.
    END.

    
    IF v_genera = YES AND v_causa = 6 THEN 
    DO:
        v_cuenta = 0.
        FOR EACH liq_items_control_finca OF liq_control_finca NO-LOCK:
              CREATE bitemsctrol.
              BUFFER-COPY bctrol TO bitemsctrol.
              ASSIGN bitemsctrol.legajo =  liq_items_control_finca.legajo
                     bitemsctrol.nombre = liq_items_control_finca.nombre
                     bitemsctrol.dni_cuil = liq_items_control_finca.dni_cuil
                     bitemsctrol.id_articulo = liq_items_control_finca.id_articulo
                     bitemsctrol.presente = NO
                     bitemsctrol.id_causa_ausencia = v_causa.
              ASSIGN  bitemsctrol.c_usuario = "c-genauto"
                      bitemsctrol.c_fecha = DATE(TODAY)
                      bitemsctrol.c_hora = string(TIME,"HH:MM:SS").
              v_cuenta = v_cuenta + 1.
        END.

        FOR EACH liq_control_finca_lotes WHERE
            liq_control_finca_lotes.id_empresa = liq_control_finca.id_empresa AND
            liq_control_finca_lotes.id_cuadrilla = liq_control_finca.id_cuadrilla AND
            liq_control_finca_lotes.fecha = liq_control_finca.fecha NO-LOCK:
            CREATE bctrollote.
            BUFFER-COPY bctrol TO bctrollote.
            ASSIGN bctrollote.id_articulo = liq_control_finca_lotes.id_articulo
                   bctrollote.id_proveedor = liq_control_finca_lotes.id_proveedor
                   bctrollote.id_origen = liq_control_finca_lotes.id_origen
                   bctrollote.id_lote = liq_control_finca_lotes.id_lote.

            ASSIGN  bctrollote.c_usuario = "c-genauto"
                    bctrollote.c_fecha = DATE(TODAY)
                    bctrollote.c_hora = string(TIME,"HH:MM:SS").
        END.
    END.

    IF v_genera = NO AND v_causa = 8 THEN
    DO:
        v_cuenta = 0.
        FOR EACH liq_control_finca WHERE liq_control_finca.id_empresa = p-empresa AND 
            liq_control_finca.id_cuadrilla = p-cuadrilla AND 
            liq_control_finca.fecha = p-referencia NO-LOCK:
            FOR EACH liq_items_control_finca OF liq_control_finca NO-LOCK:
                FIND FIRST bitemsctrol WHERE bitemsctrol.id_empresa = liq_control_finca.id_empresa AND
                                             bitemsctrol.id_cuadrilla = liq_control_finca.id_cuadrilla AND
                                             bitemsctrol.fecha = p-fecha AND
                                             bitemsctrol.legajo = liq_items_control_finca.legajo NO-LOCK NO-ERROR.
                IF NOT AVAILABLE bitemsctrol THEN
                DO:
                    FIND FIRST bctrol WHERE
                        bctrol.id_empresa = liq_control_finca.id_empresa AND
                        bctrol.id_cuadrilla = liq_control_finca.id_cuadrilla AND
                        bctrol.fecha = p-fecha NO-LOCK NO-ERROR.
                    IF AVAILABLE bctrol THEN
                    DO:
                        CREATE bitemsctrol.
                        ASSIGN bitemsctrol.id_empresa = liq_control_finca.id_empresa
                               bitemsctrol.id_cuadrilla = liq_control_finca.id_cuadrilla
                               bitemsctrol.fecha = p-fecha
                               bitemsctrol.legajo = liq_items_control_finca.legajo
                               bitemsctrol.nombre = liq_items_control_finca.nombre
                               bitemsctrol.dni_cuil = liq_items_control_finca.dni_cuil
                               bitemsctrol.id_articulo = liq_items_control_finca.id_articulo
                               bitemsctrol.presente = NO
                               bitemsctrol.id_causa_ausencia = 8 
                               bitemsctrol.observacion = "Generado automaticamente " + STRING(TODAY).   
                        ASSIGN  bitemsctrol.c_usuario = "c-genauto"
                                bitemsctrol.c_fecha = DATE(TODAY)
                                bitemsctrol.c_hora = string(TIME,"HH:MM:SS").
                        v_cuenta = v_cuenta + 1.
                    END.
                END.
            END.
        END.
    END.


END.
ELSE
DO:
    MESSAGE "No encuentra planilla de referencia" VIEW-AS ALERT-BOX.
END.
        
MESSAGE "Se generaron " + STRING(v_cuenta) + " registros" SKIP 
        "con causa " + string(v_causa) VIEW-AS ALERT-BOX INFORMATION.                  
