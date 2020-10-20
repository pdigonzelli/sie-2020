define stream sensor.
define var c            as character FORMAT '9999999999' INITIAL '0000000000'.
define var c1           as CHARACTER FORMAT 'X(3)'.
DEFINE VAR c2           AS INTEGER.
DEFINE VAR X_packing    AS CHARACTER NO-UNDO.
DEFINE VARIABLE isucTrabajo AS INTEGER NO-UNDO.
DEFINE VAR iPacking     AS INTEGER  NO-UNDO.
DEFINE VAR iTurno       AS INTEGER NO-UNDO.
DEF VAR vfechaop        AS DATE NO-UNDO.
define var  x_trazabil  as character no-undo.
define var  x_unidprod  as character no-undo.
define var iSucCaja as integer no-undo.
define var iCaja as integer no-undo.     
DEFINE VAR tParada AS INTEGER NO-UNDO.
DEFINE VAR tTransito AS INTEGER NO-UNDO.
DEFINE VAR topePeso AS DECIMAL NO-UNDO INITIAL 0.8.
DEFINE VAR fVolcado AS LOGICAL INITIAL FALSE.
DEFINE VAR i AS DATETIME NO-UNDO.
DEFINE VAR j AS DATETIME NO-UNDO.
DEFINE VAR vlector AS INTEGER NO-UNDO.
DEFINE VAR dpeso AS DECIMAL NO-UNDO.
DEFINE VAR iSuc AS INTEGER NO-UNDO.
DEFINE VAR conti AS INTEGER NO-UNDO.
DEFINE VAR fnoqu AS LOGICAL NO-UNDO.
DEFINE VAR cprogramaEtiqueta AS CHARACTER NO-UNDO.
DEFINE VAR fQ AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VAR fconsulta AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VAR dfactor AS DECIMAL NO-UNDO.


DEFINE TEMP-TABLE THORA 
    FIELD HORA AS INTEGER
    FIELD FECHA AS DATE
    FIELD EMBALADOR AS INTEGER
    INDEX I IS PRIMARY UNIQUE
        FECHA 
        HORA
        EMBALADOR.

DEFINE BUFFER vc FOR volcado_packing.
DEFINE BUFFER vc1 FOR volcado_packing.


SESSION:TIME-SOURCE = 'general'.


DEFINE FRAME X c1 cajoneras.descripcion embaladores.nombre embaladores.cuil.


OS-COMMAND SILENT (sudo sh packing/shell.sh) .  

iSucTrabajo = integer(entry(1,SESSION:PARAMETER )).
iPacking = integer(ENTRY(2,SESSION:PARAMETER)).

FIND FIRST fechas_operativas WHERE
  (fechas_operativas.fecha_inicio = TODAY AND STRING(TIME,'hh:mm:ss') >= fechas_operativas.hora_inicio) OR
  (fechas_operativas.fecha_fin = TODAY AND STRING(TIME,'hh:mm:ss') <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
IF  AVAILABLE fechas_operativas THEN 
  vfechaop = fechas_operativas.fecha_inicio.
ELSE vfechaop = today.    


FIND packing WHERE packing.id_suc_trabajo = isucTrabajo AND
                   packing.id_packing = iPacking NO-LOCK NO-ERROR.

IF NOT AVAILABLE packing THEN 
DO:
    RUN packing/d_etiqueta_status.p ('PACKING INEXISTENTE' ,'','').
    RETURN .
END.

iSuc = packing.id_suc_volcado.


RUN packing/d_etiqueta_status.p ('** ESTACION LISTA **' ,'','').

INPUT CLOSE.

REPEAT:

    UPDATE c1 WITH FRAME X NO-ERROR.
    IF LENGTH(C1) <> 3 THEN C1 = '000'.
    
    RUN lecturaEmbalador.
    
    MESSAGE 'salio'.
    PAUSE 1.
    RUN procesaCajonera.
/*    
    FIND THORA WHERE THORA.FECHA     = TODAY  AND 
                     THORA.HORA      = INTEGER(TIME / 3600) AND 
                     THORA.EMBALADOR = INTEGER(C) NO-ERROR.
    IF NOT AVAILABLE THORA  THEN
    DO: 
        CREATE THORA .
        ASSIGN THORA.FECHA     = TODAY
               THORA.HORA      = INTEGER(TIME / 3600)
               THORA.EMBALADOR = INTEGER(C).
        RELEASE thora.
       RUN packing/consultaEmbalador3.p (iSucTrabajo , ipacking , integer(c1) , integer(c)) NO-ERROR. 
    END.
    RELEASE thora.
*/    
END.

WAIT-FOR 'close' OF THIS-PROCEDURE.

PROCEDURE lecturaEmbalador.
    INPUT CLOSE.
    INPUT stream sensor from value("/dev/ttyUSB0").
    i = NOW.
    REPEAT:
        c = '0000000000'.
        IMPORT STREAM sensor c NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT.
        if c = "0000000000" OR c = "" then  NEXT.
        j = NOW.
        IF j - i > 50 THEN  
        DO:
            conti = 1.
            LEAVE.
        END.
    END. 
    OUTPUT STREAM sensor CLOSE.
    INPUT FROM TERMINAL.
    RETURN 'ok'.
END.


PROCEDURE procesaCajonera:

    DEFINE VAR nroVolcado AS INTEGER NO-UNDO.

    c2 = INTEGER(c1) NO-ERROR.
    find first cajoneras WHERE cajoneras.id_cajonera = c2 AND 
                                 cajoneras.id_suc_trabajo = packing.id_suc_trabajo AND 
                                 cajoneras.id_packing = packing.id_packing no-lock no-error.
    if available cajoneras then 
    do:
        find embaladores where embaladores.id_embalador = integer(c) no-lock         no-error.
        if NOT available embaladores then do:
            run packing/d_etiqueta_status.p (' EMBALADOR ' , c , ' INEXISTENTE ').
            RETURN.
        END.
/*

        RUN controlaEtiquetasEmbalador (embaladores.id_embalador).
        IF RETURN-VALUE = 'ERROR' THEN DO:
            PAUSE 1 MESSAGE ' ETIQUETAS AGOTADAS'.
            RETURN.
        END.
*/
    display cajoneras.descripcion embaladores.nombre embaladores.cuil with frame x.
    pause 0.
    find items_pedidos_packing of cajoneras no-lock no-error.
    if available items_pedidos_packing then
    _sale:
    DO TRANSACTION ON ERROR UNDO _sale, LEAVE _sale:

        FIND r_envases_prod WHERE   r_envases_prod.id_articulo = items_pedidos_packing.id_articulo AND
                                    r_envases_prod.id_envase = items_pedidos_packing.id_envase NO-LOCK NO-ERROR.

        RUN encuentraVolcado (OUTPUT nroVolcado).

        IF nroVolcado = 0 OR NOT AVAILABLE (VOLCADO_PACKING) THEN 
        DO:
            RUN packing/d_etiqueta_status.p ('VOLCADO INEXISTENTE O AGOTADO', '' , '').
            UNDO _sale , LEAVE _sale.
        END.


        
        find pedidos_packing of items_pedidos_packing no-lock no-error.
        RUN getTurnoPacking (iSucTrabajo , iPacking , OUTPUT iTurno) NO-ERROR.
        
        FIND FIRST fechas_operativas WHERE
          (fechas_operativas.fecha_inicio = TODAY AND STRING(TIME,'hh:mm:ss') >= fechas_operativas.hora_inicio) OR
          (fechas_operativas.fecha_fin = TODAY AND STRING(TIME,'hh:mm:ss') <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
        IF  AVAILABLE fechas_operativas THEN 
          vfechaop = fechas_operativas.fecha_inicio.
        ELSE vfechaop = today.    
 
        FIND fechas_produccion WHERE fechas_produccion.fecha = vfechaop no-lock NO-ERROR.
        FIND turnos_packing WHERE 
             turnos_packing.id_suc_trabajo = packing.id_suc_trabajo AND
             turnos_packing.id_packing = packing.id_packing AND
             turnos_packing.id_turno = iTurno NO-LOCK NO-ERROR.

         FIND FIRST ITEMS_STOCK WHERE items_stock.nro_partida = volcado_packing.nro_partida NO-LOCK NO-ERROR.
         IF NOT AVAILABLE items_stock THEN
         DO:
             RUN packing/d_etiqueta_status.p ('SIN ITEMS STOCK', STRING(VOLCADO_PACKING.NRO_PARTIDA) , '').
             UNDO _sale , LEAVE _sale.
         END.
         FIND lote WHERE lote.codigo_trazabilidad = items_stock.codigo_trazabilidad NO-LOCK NO-ERROR.
         FIND origenes OF lote NO-LOCK NO-ERROR.

         x_trazabil  = trim(if available fechas_produccion then fechas_produccion.fecha_senasa else '99') + '-' +          
                 string(turnos_packing.id_turno_packing, '99' )+ '-' + trim(volcado_packing.id_tipo_proceso) + '-' +
                 SUBSTRING(items_stock.codigo_trazabilidad, 1, 3).


         x_unidprod  = "UP-" + trim(origenes.zona_up)+ "-" + string(origenes.id_finca_senasa,"9999") + '-' + 
                   string(lote.id_lote_senasa,'999') +  '  NC.' + 
                  IF pedidos_packing.UNION_europea THEN STRING(lote.certificado,'99999') ELSE STRING(lote.cert_china,'99999').

         if x_trazabil = '' then  x_trazabil = 'SIN VOLCADO'.
         if x_unidprod = '' then  x_unidprod = 'SIN VOLCADO'.
         
         FIND FIRST items_stock WHERE items_stock.nro_partida = volcado_packing.nro_partida AND 
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK NO-ERROR.
         IF items_stock.estado_fruta  THEN dfactor = 0.9 . ELSE dfactor = 0.70.

         IF volcado_packing.peso_ultimo_proceso * dfactor < 
           volcado_packing.kilos_cajas_ultimo_proceso + 
           r_envases_prod.kilos_nominal THEN  DO:

             RUN packing/d_etiqueta_status.p ('VOLCADO AGOTADO' , '' , '' ).
             UNDO _sale , LEAVE _sale.

         END.
         
         RUN grabacaja (output iSucCaja , output iCaja) NO-ERROR.
         IF AVAILABLE contadores_embalador THEN RELEASE contadores_embalador.
         IF ERROR-STATUS:ERROR THEN 
         DO:
             RUN packing/d_etiqueta_status.p ('ERROR EN CAJA', '' , '').
             UNDO _sale , LEAVE _sale.
         END.

         RELEASE volcado_packing.
         FIND clientes_ventas OF pedidos_packing NO-LOCK NO-ERROR.
         IF clientes_ventas.programaEmisionEtiquetaCaja <> '' AND  
           clientes_ventas.habilitacionEmisionEtiquetaCaja  THEN  
                cProgramaEtiqueta = 'packing/' + clientes_ventas.programaEmisionEtiquetaCaja.
         ELSE
         DO:
             CASE items_pedidos_packing.id_caract:
                 WHEN  2 THEN
                        cprogramaetiqueta = 'packing/dEtiquetaEsp.p'.
                 WHEN  19 THEN
                        cprogramaetiqueta = 'packing/dEtiquetaEsp.p'.
                 WHEN 21   THEN
                        cprogramaetiqueta = 'packing/dEtiquetaUwd.p'.
                 WHEN 14   THEN
                        cprogramaetiqueta = 'packing/dEtiquetaUwd.p'.
                 OTHERWISE
                             cprogramaetiqueta = 'packing/dEtiquetaStd.p'.
             END CASE.
         END.
        /*
             IF items_pedidos_packing.id_caract = 17 
                 THEN  cprogramaetiqueta =  'packing/dEtiquetaEsp.p'.
             ELSE IF items_pedidos_packing.id_caract = 21  THEN
                 cprogramaetiqueta =  'packing/dEtiquetaUwd.p'.
             ELSE  cProgramaEtiqueta =  'packing/dEtiquetaStd.p'. */

         RUN  packing/dEtiquetaFromCaja.p  (iSucCaja , iCaja  , cprogramaEtiqueta).
         RUN actualizaPesoCajasVolcado (nroVolcado).
        END.
        else RUN packing/d_etiqueta_status.p (  "CAJONERA SIN PEDIDO" , string(c1) , '').
    end. else RUN packing/d_etiqueta_status.p (  "NO HAY PEDIDO O CAJONERA" ,string(c1) , '').
END PROCEDURE.


PROCEDURE controlaEtiquetasEmbalador.
    DEFINE INPUT PARAMETER iEmbalador AS INTEGER NO-UNDO.

    DEFINE VAR i AS INTEGER.
    DEFINE VAR dcreacion AS DATETIME NO-UNDO.

    DEFINE BUFFER c FOR cajas.

    FIND LAST c WHERE c.id_embalador = iEmbalador AND c.id_suc_trabajo = iSucTrabajo AND c.id_packing = iPacking NO-LOCK NO-ERROR.
    IF NOT AVAILABLE  c  THEN RETURN ''.
    dcreacion = c.creacion.
    i = 0.
    REPEAT:
        i = i + 1.
        FIND PREV c WHERE c.id_embalador = iEmbalador AND c.id_suc_trabajo = iSucTrabajo AND c.id_packing = iPacking  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE c  THEN LEAVE. 
        IF ( dcreacion - c.creacion  )  / 1000  >= 60 THEN LEAVE.
    END.
    IF i > 5 THEN RETURN 'ERROR' . 
                  ELSE RETURN ''.
END.


PROCEDURE encuentraVolcado.
    
    DEFINE OUTPUT PARAMETER nroVolcado AS INTEGER.
     

    FIND caracteristicas OF items_pedidos_packing.

/*    RUN packing/d_etiqueta_status.p ( 'pablo1 ' , '' , ''). */
    
    FOR EACH VOLCADO_PACKING WHERE VOLCADO_PACKING.FECHA >= TODAY - 3 AND abierto AND 
             volcado_packing.id_sucursal = iSuc NO-LOCK BY volcado_packing.id_lector.

/*        RUN packing/d_etiqueta_status.p ( STRING(ITEMS_PEDIDOS_PACKING.ID_CARACT), '' , '' ). */


        IF (items_pedidos_packing.id_caract = 19 OR items_pedidos_packing.id_caract = 2)  AND  trim(volcado_packing.id_tipo_proceso) <> 'Q' AND TRIm(volcado_packing.id_tipo_proceso) <> 'W' AND TRIm(volcado_packing.id_tipo_proceso) <> 'V' 
            AND TRIm(volcado_packing.id_tipo_proceso) <> 'E'  AND TRIm(volcado_packing.id_tipo_proceso) <> 'D' THEN  NEXT.
        IF items_pedidos_packing.id_caract <> 14 AND items_pedidos_packing.id_caract <> 21 AND items_pedidos_packing.id_caract <> 19 AND items_pedidos_packing.id_caract <> 2 AND 
            ( volcado_packing.id_tipo_proceso = 'Q' OR volcado_packing.id_tipo_proceso = 'V'/*ùOR volcado_packing.id_tipo_proceso = 'D' */) THEN NEXT.

/*        IF ITEMS_PEDIDOS_PACKING.ID_CARACT = 17  THEN RUN packing/d_etiqueta_status.p (  "HAY VOLCADO 17" , '' ,  ''). */

        FIND FIRST items_stock WHERE items_stock.nro_partida = volcado_packing.nro_partida AND 
                                     items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK NO-ERROR.
        IF items_stock.estado_fruta  THEN dfactor = 0.9 . ELSE dfactor = 0.70.

        
        IF volcado_packing.peso_ultimo_proceso * dfactor < 
          volcado_packing.kilos_cajas_ultimo_proceso + 
          r_envases_prod.kilos_nominal THEN   NEXT.

        /*IF ITEMS_PEDIDOS_PACKING.ID_CARACT = 17  THEN RUN packing/d_etiqueta_status.p (  "HAY VOLCADO 17 1" , '' ,  '').*/

        IF volcado_packing.peso * dfactor < 
          volcado_packing.kilos_cajas + 
          r_envases_prod.kilos_nominal THEN   NEXT.

/*        IF ITEMS_PEDIDOS_PACKING.ID_CARACT = 17  THEN RUN packing/d_etiqueta_status.p (  "HAY VOLCADO 17 2" , '' ,  ''). */

         
        RUN duracionParadaProduccion  (cajoneras.id_suc_trabajo , cajoneras.id_packing , 
                                        cajoneras.id_linea_produccion , 
                                        volcado_packing.inicio_ultimo_proceso , NOW , 
                                        OUTPUT tParada ).

         tTransito = NOW - volcado_packing.inicio_ultimo_proceso - tParada.

        IF  (VOLCADO_PACKING.FIN_ULTIMO_PROCESO = ? AND tTransito < CAJONERAS.TIEMPO1) OR
            (VOLCADO_PACKING.FIN_ULTIMO_PROCESO <> ? AND tTransito > cajoneras.tiempo1) THEN NEXT.
        
/*        IF ITEMS_PEDIDOS_PACKING.ID_CARACT = 17  THEN RUN packing/d_etiqueta_status.p (  "HAY VOLCADO 17 3" , '' ,  ''). */
        
        nroVolcado = volcado_packing.nro_volcado.
        
        LEAVE.
    END.
END.

PROCEDURE actualizaPesoCajasVolcado.
    DEFINE INPUT PARAMETER nroVolcado AS INTEGER.
    DEFINE VAR i AS INTEGER.
    REPEAT ON ERROR UNDO , RETRY :
       i = i + 1.
       IF i > 10000  THEN  DO:
           LEAVE.
       END.
       find volcado_packing WHERE volcado_packing.nro_volcado = nroVolcado exclusive-lock no-wait no-error.
       if locked(volcado_packing) then next. 
       volcado_packing.kilos_cajas = volcado_packing.kilos_cajas + r_envases_prod.kilos_nominal.
       volcado_packing.kilos_cajas_ultimo_proceso = volcado_packing.kilos_cajas_ultimo_proceso + r_envases_prod.kilos_nominal.
       RELEASE volcado_packing.
       LEAVE.
    END.
END.


PROCEDURE getFechaOperativa.
    DEFINE INPUT PARAMETER piFecha AS DATE.
    DEFINE OUTPUT PARAMETER poFecha AS DATE.

        FIND FIRST fechas_operativas WHERE
          (fechas_operativas.fecha_inicio = piFecha AND STRING(TIME,'hh:mm:ss') >= fechas_operativas.hora_inicio) OR
          (fechas_operativas.fecha_fin = piFecha AND STRING(TIME,'hh:mm:ss') <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
        IF  AVAILABLE fechas_operativas THEN 
          poFecha = fechas_operativas.fecha_inicio.
        ELSE poFecha = today.    
END.

PROCEDURE getTurnoPacking:
    DEFINE INPUT PARAMETER  iSucTrabajo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER  ipacking AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER iTurno AS INTEGER  NO-UNDO.

    FOR LAST turnos_packing WHERE turnos_packing.id_suc_trabajo = isuctrabajo AND  turnos_packing.id_packing = ipacking  AND 
     turnos_packing.hora_inicio < string(TIME,"hh:mm") NO-LOCK 
        BY turnos_packing.hora_inicio .
        iTurno = turnos_packing.id_turno_packing.
    END.
    IF iTurno = 0 THEN
        FOR  LAST turnos_packing WHERE turnos_packing.id_suc_trabajo = isuctrabajo AND  turnos_packing.id_packing = ipacking and 
          turnos_packing.hora_fin > string(time,"hh:mm")NO-LOCK BY turnos_packing.hora_inicio.
            iTurno = turnos_packing.id_turno_packing.
        END.
END PROCEDURE.

procedure grabacaja.
      define output parameter iSuc as integer no-undo.
      define output parameter iCaja as integer no-undo.
      
    DEFINE VAR fop AS DATE NO-UNDO.
    DEFINE VAR ie AS INTEGER NO-UNDO.
    
    RUN fechaOperativa (OUTPUT fop).

    FIND calidades OF items_pedidos_packing NO-LOCK NO-ERROR.

    FIND r_envases_prod OF items_pedidos_packing NO-LOCK NO-ERROR.
 


    CREATE cajas.

    ASSIGN
      cajas.id_suc_trabajo  = cajoneras.id_suc_trabajo
      cajas.id_caja         = NEXT-VALUE(cajas)
      cajas.etiqueta        = cajas.id_suc_trabajo * 100000000000 + cajas.id_caja
      cajas.id_cajonera     = cajoneras.id_cajonera
      cajas.id_orden        = pedidos_packing.id_orden
      cajas.item            = ITEMs_pedidos_packing.item
      cajas.id_empresa      = pedidos_packing.id_empresa
      cajas.id_punto_emisor = pedidos_packing.id_punto_emisor
      cajas.id_packing      = cajoneras.id_packing
      cajas.id_embalador    = embaladores.id_embalador
      cajas.nro_volcado     = volcado_packing.nro_volcado
      cajas.id_turno        = turnos_packing.id_turno_packing
      cajas.fecha           = TODAY
      cajas.fecha_operativa = fop
      cajas.leida           = FALSE
      cajas.creacion        = NOW
      cajas.trazabilidad    = X_trazabil
      cajas.uniprod         = X_unidprod
      cajas.codigo_trazabilidad =  items_stock.codigo_trazabilidad
      cajas.letra_color     = IF cajoneras.color_fruta = 1 THEN 'A' ELSE 'R'
      cajas.id_tipo_proceso = volcado_packing.id_tipo_proceso
      cajas.empapelado      = IF AVAILABLE calidades THEN calidades.empapelado ELSE FALSE
      cajas.id_envase       = items_pedidos_packing.id_envase
      cajas.peso_nominal    = IF AVAILABLE r_envases_prod THEN r_envases_prod.kilos_nominal ELSE 0 
      cajas.id_caract       = items_pedidos_packing.id_caract
      cajas.id_calidad      = items_pedidos_packing.id_calidad
      cajas.id_categoria    = items_pedidos_packing.id_categoria
      cajas.id_envase       = items_pedidos_packing.id_envase
      cajas.id_marca        = items_pedidos_packing.id_marca
      cajas.UNION_europea   = pedidos_packing.UNION_europea
      cajas.contramarca     = pedidos_packing.contramarca
      cajas.china           = pedidos_packing.china
      cajas.calibre         = items_pedidos_packing.calibre
      cajas.bultos          = items_pedidos_packing.bultos 
      cajas.id_articulo     = items_pedidos_packing.id_articulo
      cajas.id_tipo_pallet  = items_pedidos_packing.id_tipo_pallet 
      cajas.id_tipo_esquinero = items_pedidos_packing.id_tipo_esquinero 
      cajas.id_variedad       = items_pedidos_packing.id_variedad.
      
      iSuc = cajas.id_suc_trabajo.
      icaja = cajas.id_caja.
      

      cajas.kilos = r_envases_prod.kilos.

      /************* contadores por confeccion ************************************************/

      FIND contadores_embalador WHERE    contadores_embalador.fecha_operativa = cajas.fecha_operativa AND 
                                         contadores_embalador.id_embalador = cajas.id_embalador AND
                                         contadores_embalador.id_packing = cajas.id_packing AND
                                         contadores_embalador.id_suc_trabajo = cajas.id_suc_trabajo AND
                                         contadores_embalador.id_turno_packing = cajas.id_turno_packing NO-LOCK NO-ERROR.

      IF NOT AVAILABLE contadores_embalador THEN
      DO:
          CREATE contadores_embalador.
          ASSIGN 
              contadores_embalador.fecha_operativa = cajas.fecha_operativa 
              contadores_embalador.id_embalador = cajas.id_embalador
              contadores_embalador.id_packing = cajas.id_packing
              contadores_embalador.id_suc_trabajo = cajas.id_suc_trabajo
              contadores_embalador.id_turno_packing = cajas.id_turno_packing.
          RELEASE contadores_embalador.
      END.
      ie = 0.
      REPEAT ON ERROR UNDO, RETRY: 
          FIND contadores_embalador WHERE    contadores_embalador.fecha_operativa = cajas.fecha_operativa AND 
                                             contadores_embalador.id_embalador = cajas.id_embalador AND
                                             contadores_embalador.id_packing = cajas.id_packing AND
                                             contadores_embalador.id_suc_trabajo = cajas.id_suc_trabajo AND
                                             contadores_embalador.id_turno_packing = cajas.id_turno_packing EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
          IF ie >= 1000  THEN  LEAVE.
          ie = ie + 1.
          IF LOCKED(contadores_embalador) THEN NEXT.
          
          IF cajas.peso_nominal < 10 AND cajas.peso_nominal <> 0 THEN
          IF cajas.empapelado THEN
              ASSIGN confecciones[1]  = confecciones[1] + 1.       
          ELSE
              ASSIGN confecciones[2] = confecciones[2] + 1.
          ELSE                 
          IF cajas.peso_nominal >= 10 AND cajas.peso_nominal < 20 THEN
          IF cajas.empapelado THEN
            ASSIGN confecciones[3] = confecciones[3] + 1.
          ELSE
            ASSIGN confecciones[4] = confecciones[4] + 1.
          ELSE
          IF cajas.peso_nominal >= 20 THEN
            ASSIGN confecciones[5] = confecciones[5] + 1.

          confecciones[6] = confecciones[6] + 1.

          FIND CURRENT contadores_embalador NO-LOCK.
          LEAVE.
      END.
      RELEASE contadores_embalador.
      RELEASE cajas.

/*
     IF cajas.peso_nominal < 10 AND cajas.peso_nominal <> 0 THEN
     IF cajas.empapelado THEN
         ASSIGN confecciones[1]  = confecciones[1] + 1.       
     ELSE
         ASSIGN confecciones[2] = confecciones[2] + 1.
     ELSE                 
     IF cajas.peso_nominal >= 10 AND cajas.peso_nominal < 20 THEN
     IF cajas.empapelado THEN
       ASSIGN confecciones[3] = confecciones[3] + 1.
     ELSE
       ASSIGN confecciones[4] = confecciones[4] + 1.
     ELSE
     IF cajas.peso_nominal >= 20 THEN
       ASSIGN confecciones[5] = confecciones[5] + 1.

     confecciones[6] = confecciones[6] + 1.

      RELEASE contadores_embalador.
*/

end procedure.

PROCEDURE fechaOperativa.
    DEFINE OUTPUT PARAMETER fop AS DATE NO-UNDO.
    DEFINE BUFFER tur FOR turnos_packing.
    DEFINE BUFFER tur1 FOR turnos_packing.

    FIND FIRST tur WHERE    tur.id_suc_trabajo  = packing.id_suc_trabajo AND
                            tur.id_packing      = packing.id_packing AND
                            tur.hora_inicio     > STRING(TIME,'hh:mm') 
                            USE-INDEX hora_inicio NO-LOCK NO-ERROR.

    FIND FIRST tur1 WHERE   tur1.id_suc_trabajo  = packing.id_suc_trabajo AND
                            tur1.id_packing      = packing.id_packing                          
                            USE-INDEX hora_inicio NO-LOCK NO-ERROR.

   IF ROWID(tur) = ROWID(tur1) THEN fop = TODAY - 1 . ELSE fop = TODAY.
    
END PROCEDURE.


PROCEDURE duracionParadaProduccion.

    DEFINE INPUT PARAMETER isuctrabajo AS INTEGER.
    DEFINE INPUT PARAMETER ipacking AS INTEGER.
    DEFINE INPUT PARAMETER iLinea AS INTEGER.
    DEFINE INPUT PARAMETER tInicio AS DATETIME.
    DEFINE INPUT PARAMETER tFin AS DATETIME.
    DEFINE OUTPUT PARAMETER duracion AS INTEGER.

    FOR EACH paradas_linea WHERE paradas_linea.id_suc_trabajo = iSucTrabajo AND
                                 paradas_linea.id_packing = iPacking AND
                                 paradas_linea.id_linea_produccion = iLinea AND 
                                 paradas_linea.inicio >= tInicio AND
                                 paradas_linea.fin >= tFin NO-LOCK , FIRST causas_paradas OF paradas_linea WHERE causas_paradas.produccion NO-LOCK .
        duracion = duracion + paradas_linea.fin - paradas_linea.inicio. 

    END.

END PROCEDURE.
