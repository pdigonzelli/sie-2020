define input parameter p_suc as integer.
define input parameter p_fecha as date.

define var v_lote as integer format ">>>9".
define var v_anio_lote as integer format "9999".
define var v_suc as integer.
define var v_cliente as integer.
define buffer b_items_fac for items_factura.
define buffer b_remitos for remitos.
define var v_estabien_crealo as logical initial false.

define temp-table tt_tambores
    field id_empresa as integer
    field id_sucursal as integer
    field id_sucursal_ubicacion as integer
    field id_lote as integer
    field id_tipotambor as integer
    field id_articulo as integer
    field id_calidad as integer
    field id_envase as integer
    field id_contrato_of as char
    field id_tipocontrato_of as integer
    field anio_of as integer
    field item_of as integer
    field kilos_tambor as decimal
    field id_etiqueta as integer
    field fecha as date
    index etiqueta id_etiqueta.
    
define output parameter table for tt_tambores.
/**************************** BUSCO EL STOCK ACTUAL EN LA SUCURSAL ESPECIFICA ******************/

FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion = p_suc
                                      AND tambores_industria.id_locacion_ubicacion = 4.

    CREATE tt_tambores.
    ASSIGN tt_tambores.id_empresa               = tambores_industria.id_empresa
           tt_tambores.id_sucursal              = tambores_industria.id_sucursal
           tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
           tt_tambores.id_lote                  = tambores_industria.id_lote
           tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
           tt_tambores.id_articulo              = tambores_industria.id_articulo
           tt_tambores.id_calidad               = tambores_industria.id_calidad
           tt_tambores.id_envase                = tambores_industria.id_envase
           tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
           tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
           tt_tambores.anio_of                  = tambores_industria.anio_of
           tt_tambores.item_of                  = tambores_industria.item_of
           tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
           tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
           tt_tambores.fecha                    = tambores_industria.fecha.

END.    


/*****************************************************************************************************/

/************** BUSCO LOS LOTES DESPACHADOS DESDE LA SUCURSAL DESPUES DE LA FECHA ESPECIFICA *********/
/************** SIN REPETIR TAMBORES CON LOS EXISTENTES                                      *********/

FOR EACH remitos NO-LOCK WHERE remitos.fecha        > p_fecha
                           AND remitos.fecha        <= TODAY
                           AND remitos.id_sucursal  = p_suc
                           AND remitos.estado       = TRUE.
    
    FOR EACH items_factura OF remitos NO-LOCK.
        v_lote      = INTEGER(SUBSTRING(items_factura.nro_lote,1,4)).
        v_anio_lote = INTEGER(SUBSTRING(items_factura.nro_lote,6,2)).
        v_anio_lote = v_anio_lote + 2000.
            
        FOR EACH tambores_industria WHERE tambores_industria.id_tambor      >= items_factura.desde_lote
                                      AND tambores_industria.id_tambor      <= items_factura.hasta_lote
                                      AND tambores_industria.id_lote        = v_lote
                                      AND YEAR(tambores_industria.fecha)    = v_anio_lote
                                      AND tambores_industria.id_articulo    = items_factura.id_articulo
                                      NO-LOCK.
                                          
            FIND FIRST tt_tambores WHERE tt_tambores.id_etiqueta = tambores_industria.id_etiqueta 
                                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt_tambores THEN DO:
                
                CREATE tt_tambores.
                ASSIGN tt_tambores.id_empresa               = tambores_industria.id_empresa
                       tt_tambores.id_sucursal              = tambores_industria.id_sucursal
                       tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
                       tt_tambores.id_lote                  = tambores_industria.id_lote
                       tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
                       tt_tambores.id_articulo              = tambores_industria.id_articulo
                       tt_tambores.id_calidad               = tambores_industria.id_calidad
                       tt_tambores.id_envase                = tambores_industria.id_envase
                       tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
                       tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
                       tt_tambores.anio_of                  = tambores_industria.anio_of
                       tt_tambores.item_of                  = tambores_industria.item_of
                       tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
                       tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
                       tt_tambores.fecha                    = tambores_industria.fecha.
            END.
        END.
    END.
END. 

/******************************************************************************************************/


/************** BUSCO LOS LOTES QUE LLEGARON A LA SUCURSAL DESPUES DE LA FECHA ESPECIFICA *********/
/************** Y LOS BORRO DE LA TABLA TEMPORAL                                          *********/

FOR EACH lugar_descarga WHERE lugar_descarga.id_sucursal = p_suc NO-LOCK.
    /* BUSCO TODOS LOS LUGARES DE DESCARGA QUE SE ASOCIAN A LA SUCURSAL ELEGIDA */
    /* PORQUE ESA ES LA FORMA DE SABER CUALES REMITOS FUERON HACIA LA SUCURSAL
       QUE ESTOY TRATANDO DE SACAR STOCK A FECHA */
    FOR EACH remitos NO-LOCK WHERE remitos.fecha        > p_fecha
                               AND remitos.fecha        <= TODAY
                               AND remitos.id_lugdes    = lugar_descarga.id_lugdes
                               AND remitos.estado       = TRUE.
                             
        FOR EACH items_factura OF remitos NO-LOCK.
            v_lote      = INTEGER(SUBSTRING(items_factura.nro_lote,1,4)).
            v_anio_lote = INTEGER(SUBSTRING(items_factura.nro_lote,6,2)).
            v_anio_lote = v_anio_lote + 2000.
            
            FOR EACH tambores_industria WHERE tambores_industria.id_tambor      >= items_factura.desde_lote
                                          AND tambores_industria.id_tambor      <= items_factura.hasta_lote
                                          AND tambores_industria.id_lote        = v_lote
                                          AND YEAR(tambores_industria.fecha)    = v_anio_lote
                                          AND tambores_industria.id_articulo    = items_factura.id_articulo
                                          NO-LOCK.
                                              
                FIND FIRST tt_tambores WHERE tt_tambores.id_etiqueta = tambores_industria.id_etiqueta 
                                    NO-LOCK NO-ERROR.
                IF AVAILABLE tt_tambores THEN DO:
                    DELETE tt_tambores.
                    /* LO BORRO PORQUE SIGNIFICA QUE LLEGO A LA SUCURSAL DESPUES DE LA
                       FECHA QUE ESTOY CONSULTANDO */
                    
                END.
                
            END.
        END.
    END.
END.


/******************************************************************************************************/

