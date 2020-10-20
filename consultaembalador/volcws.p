
/*------------------------------------------------------------------------
    File        : volcws.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 29 00:44:49 ART 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT PARAMETER CETIQUETA        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER CSUCURSALPACKING AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER PCRESPUESTA     AS CHARACTER NO-UNDO.

DEFINE VAR I                            AS INTEGER NO-UNDO.

DEFINE BUFFER aux_volcado FOR volcado_packing.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
 SESSION:TIME-SOURCE = "produccion".
 
 
 
 
 RUN DATVOL (INPUT cEtiqueta,INPUT CSUCURSALPACKING).
 
 PCRESPUESTA = 'OK'.
 
 
 CATCH E AS Progress.Lang.Error :
     
     DO I = 1 TO E:NumMessages:
        PCRESPUESTA = PCRESPUESTA + CHR(13) + E:GetMessage(I).
     END.
         
 END CATCH.
 
 FINALLY:
    RETURN PCRESPUESTA.
 END FINALLY.
 


/* **********************  Internal Procedures  *********************** */


 PROCEDURE DATVOL.
    DEFINE INPUT PARAMETER  x_dato AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  X_SUCPACKING AS INTEGER NO-UNDO.
 
 
    DEFINE VARIABLE x_longitud  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_lector    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_partida   AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_salida    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_volcado   AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE x_sucursal  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE xSucPack    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_proceso   AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE x_uenoue    AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE x_etiqueta  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_sucvac    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE x_hora      AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE x_fecha_des AS DATE         NO-UNDO.
    DEFINE VARIABLE xPartida_s  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE xUnion      AS LOGICAL      NO-UNDO.
    DEFINE VARIABL  xNumLec     AS INTEGER      NO-UNDO.
    
    
    
    xSucPack    = X_SUCPACKING.
    
    IF XSUCPACK <> 98 AND XSUCPACK <> 97 THEN
        UNDO, THROW NEW Progress.Lang.AppError('SUCURSAL INCORRECTA: ' + STRING(XSUCPACK) ,550). 
        
    CASE XSUCPACK:
        WHEN 98 THEN DO:
            XNUMLEC = 5.
        END.
        WHEN 97 THEN DO:
            XNUMLEC = 3.
        END.
    END CASE.    
    
    x_hora      = SUBSTR(STRING(TIME,"hh:mm:ss"),1,2) +
                  SUBSTR(STRING(TIME,"hh:mm:ss"),4,2) +
                  SUBSTR(STRING(TIME,"hh:mm:ss"),7,2).
    
    IF x_hora < "060000" THEN
        x_fecha_des = TODAY - 1.
    ELSE
        x_fecha_des = TODAY.
 
 
    x_longitud  = LENGTH(x_dato).

    x_lector  = INTEGER(TRIM(SUBSTR(x_dato,1,1))).
 
 
    IF X_LONGITUD <> 2 AND X_LONGITUD <> 3 AND X_LONGITUD <> 4 AND X_LONGITUD <> 11 THEN
        UNDO, THROW NEW Progress.Lang.AppError('LONGITUD DE ETIQUETA INCORRECTA: ' + STRING(X_LONGITUD) ,550). 
    
    
    /******************************************/
    /******* Lectura de Tipo de Proceso *******/
    /******************************************/
    
    IF x_longitud = 2 THEN DO:
        x_proceso = SUBSTR(x_dato,2,1).
    
        IF x_lector > 0 AND x_lector < XNUMLEC THEN DO:
            FIND FIRST volcado_lectores WHERE
                volcado_lectores.id_sucursal_packing = xSucPack AND
                volcado_lectores.id_lector = x_lector NO-ERROR.
            IF AVAILABLE volcado_lectores THEN DO:
                FIND FIRST tipos_procesos WHERE
                    tipos_procesos.id_tipo_proceso = x_proceso NO-LOCK NO-ERROR.
                IF AVAILABLE tipos_procesos THEN DO:
                    ASSIGN volcado_lectores.id_tipo_proceso = x_proceso.
                    RELEASE volcado_lectores.
                END.
                ELSE 
                    UNDO, THROW NEW Progress.Lang.AppError('L2 - NO EXISTE REGISTRO DE TIPOS_PROCESOS PARA ' + X_PROCESO ,550).
            END.
            ELSE 
                UNDO, THROW NEW Progress.Lang.AppError('L2 - NO EXISTE REGISTRO DE VOLCADO_LECTORES PARA LECTOR-SUCURSALPACKING: ' + STRING(X_LECTOR) + '-' + STRING(XSUCPACK),550).
        END.
        ELSE UNDO, THROW NEW Progress.Lang.AppError('L2 - CODIGO DE LECTOR > ' + STRING(XNUMLEC - 1) +  ':' + STRING(X_LECTOR) ,550).
        
    END.
    
    
    /**********************************************/
    /******* Lectura Volcado U.E. y NO U.E. *******/
    /**********************************************/
    
    IF x_longitud = 3 THEN DO:
        x_uenoue  = SUBSTR(x_dato,2,2).
    
        IF x_lector > 0 AND x_lector < XNUMLEC THEN DO:
            FIND FIRST volcado_lectores WHERE
                volcado_lectores.id_sucursal_packing = xSucPack AND
                volcado_lectores.id_lector = x_lector NO-ERROR.
            IF AVAILABLE volcado_lectores THEN DO:
                ASSIGN volcado_lectores.letra_color = SUBSTR(x_uenoue,1,1).
                RELEASE volcado_lectores.
            END.
            ELSE 
                UNDO, THROW NEW Progress.Lang.AppError('L3 - NO EXISTE REGISTRO DE VOLCADO_LECTORES PARA LECTOR-SUCURSALPACKING: ' + STRING(X_LECTOR) + '-' + STRING(XSUCPACK),550).
        END.
        ELSE UNDO, THROW NEW Progress.Lang.AppError('L3 - CODIGO DE LECTOR > ' + STRING(XNUMLEC - 1) +  ':' + STRING(X_LECTOR),550).
    END.
    
    
    /**********************************************************/
    /******* Lectura de Sucursal para Cierre de Volcado *******/
    /**********************************************************/
    
    IF x_longitud = 4 THEN DO:
        x_sucursal  = INTEGER(TRIM(SUBSTR(x_dato,2,3))).
    
        FIND FIRST cierre_volcado WHERE
            cierre_volcado.id_sucursal_packing = xSucPack AND
            cierre_volcado.id_sucursal = x_sucursal AND
            cierre_volcado.id_lector = x_lector NO-ERROR.
        IF AVAILABLE cierre_volcado THEN DO:
            ASSIGN cierre_volcado.estado = TRUE.
            RELEASE cierre_volcado.
        END.
        ELSE
          UNDO, THROW NEW Progress.Lang.AppError('L4 - NO EXISTE REGISTRO DE CIERRE VOLCADO PARA LECTOR-SUCURSAL-SUCURSALPACKING: ' + STRING(X_LECTOR) + '-' + STRING(X_SUCURSAL) + '-' + STRING(XSUCPACK),550).
    END.
    
    
    /*********************************************************/
    /******* Lectura de Etiqueta para Volcado y Cierre *******/
    /******* Lectura de Etiqueta para Despacho de Bins *******/
    /*********************************************************/
    
    IF x_longitud = 11 THEN DO:
            
        x_partida   = INTEGER(TRIM(SUBSTR(x_dato,2,8))).
        xPartida_s  = INTEGER(TRIM(SUBSTR(x_dato,10,2))).
    
        /******* Lectura de Etiqueta para Volcado y Cierre *******/

        MESSAGE "DATO: " X_DATO X_PARTIDA XPARTIDA_S .
    
        FIND FIRST volcado_lectores WHERE
            volcado_lectores.id_sucursal_packing = xSucPack AND
            volcado_lectores.id_lector  = x_lector NO-ERROR.
    
        IF AVAILABLE volcado_lectores AND
            volcado_lectores.id_lector > 0 AND
            volcado_lectores.id_lector < XNUMLEC THEN DO:
            FIND FIRST cierre_volcado WHERE
                cierre_volcado.id_sucursal_packing = xSucPack AND
                cierre_volcado.id_lector = x_lector NO-ERROR.
            IF AVAILABLE cierre_volcado AND cierre_volcado.estado THEN DO:
                RUN dd_cierra.p (INPUT cierre_volcado.id_sucursal, INPUT x_partida, INPUT xPartida_s).
                ASSIGN cierre_volcado.estado = FALSE.
                RELEASE cierre_volcado.
            END.
            ELSE DO:
                FIND LAST volcado_packing USE-INDEX volcado_packing WHERE
                    volcado_packing.nro_partida         = x_partida AND
                    volcado_packing.nro_partida_serial  = xPartida_s AND
                    volcado_packing.abierto NO-LOCK NO-ERROR.
    
                IF NOT AVAILABLE volcado_packing THEN DO:
                    FIND FIRST saldos_packing WHERE
                        saldos_packing.nro_partida = x_partida AND
                        saldos_packing.nro_partida_serial = xPartida_s AND
                        saldos_packing.habilitado AND
                        saldos_packing.abierto
                        NO-LOCK NO-ERROR.
    
                    IF AVAILABLE saldos_packing THEN DO:
                        FIND FIRST items_stock WHERE
                            items_stock.nro_partida = x_partida AND
                            items_stock.nro_partida_serial = xPartida_s
                            NO-LOCK NO-ERROR.
    
                        IF AVAILABLE items_stock THEN
                            RUN graba_packing(X_LECTOR,X_PARTIDA,XPARTIDA_S).
                        ELSE    
                            UNDO, THROW NEW Progress.Lang.AppError('L11 - NO EXISTE ITEMS_STOCK PARA PARTIDA-PARTIDASERIAL: ' + STRING(X_PARTIDA) + '-' + STRING(XPARTIDA_S) ,550).
                    END.
                    ELSE
                        UNDO, THROW NEW Progress.Lang.AppError('L11 - NO EXISTE SALDOS_PACKING PARA PARTIDA-PARTIDASERIAL: ' + STRING(X_PARTIDA) + '-' + STRING(XPARTIDA_S) ,550).
                END.
                ELSE
                    UNDO, THROW NEW Progress.Lang.AppError('L11 - NO EXISTE VOLCADO_PACKING ABIERTO PARA PARTIDA-PARTIDASERIAL: ' + STRING(X_PARTIDA) + '-' + STRING(XPARTIDA_S) ,550).
            END.
        END.
        ELSE 
            UNDO, THROW NEW Progress.Lang.AppError('L11 - NO EXISTE REGISTRO DE VOLCADO_LECTORES PARA LECTOR-SUCURSALPACKING: ' + STRING(X_LECTOR) + '-' + STRING(XSUCPACK),550).
    END.
 
 
 
 
 END PROCEDURE. 

PROCEDURE GRABA_PACKING:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER X_LECTOR    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER x_partida   AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER xpartida_S   AS INTEGER     NO-UNDO.


    FIND FIRST sucursales OF volcado_lectores
        NO-LOCK NO-ERROR.
    IF AVAILABLE sucursales THEN DO:
        CREATE volcado_packing.
        ASSIGN
            volcado_packing.nro_volcado                 = NEXT-VALUE(nro_volcado)
            volcado_packing.nro_partida                 = x_partida
            volcado_packing.nro_partida_serial          = xPartida_s
            volcado_packing.id_lector                   = x_lector
            volcado_packing.id_sucursal_etiqueta        = saldos_packing.id_sucursal_etiqueta
            volcado_packing.id_etiqueta                 = saldos_packing.id_etiqueta
            volcado_packing.id_sucursal                 = volcado_lectores.id_sucursal
            volcado_packing.id_proveedor                = items_stock.id_proveedor
            volcado_packing.id_origen                   = items_stock.id_origen
            volcado_packing.id_lote                     = items_stock.id_lote
            volcado_packing.codigo_trazabilidad         = items_stock.codigo_trazabilidad
            volcado_packing.id_mercado                  = items_stock.id_mercado
            volcado_packing.union_europea               = items_stock.union_europea
            volcado_packing.china                       = items_stock.china
            volcado_packing.cantidad                    = 0
            volcado_packing.abierto                     = TRUE
            volcado_packing.fecha                       = TODAY
            volcado_packing.hora                        = STRING(TIME,"hh:mm:ss")
            volcado_packing.inicio_ultimo_proceso       = NOW
            volcado_packing.peso                        = saldos_packing.cantidad_total * 20
            volcado_packing.peso_ultimo_proceso         = saldos_packing.saldo * 20
            volcado_packing.kilos_cajas_ultimo_proceso  = 0
            volcado_packing.id_tipo_proceso             = volcado_lectores.id_tipo_proceso
            volcado_packing.letra_color                 = volcado_lectores.letra_color
            volcado_packing.nombrePerfil                = saldos_packing.nombrePerfil.
    END.


END PROCEDURE.
