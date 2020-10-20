
/*------------------------------------------------------------------------
    File        : veotransf.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Mon Apr 10 11:04:01 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR CSTATUS AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 280.

FOR FIRST MOVSUCU NO-LOCK WHERE FECHA >= DATE("01/01/2016") AND ID_TIPO_MOVSTO = 78
    BY NRO.

    FOR EACH ITEMS_STOCK OF MOVSUCU NO-LOCK.
        /*        

        DISP    movsucu.FECHA 
                MOVSUCU.NRO
                ITEMS_STOCK.NRO_PARTIDA 
                items_stock.nro_partida_serial.

        FOR EACH BIT WHERE BIT.nro_partida = items_stock.NRO_PARTIDA NO-LOCK
            BY BIT.NRO_PARTIDA_SERIAL BY BIT.FECHA.
                             
            FIND BMO OF BIT NO-LOCK NO-ERROR.
            FIND tipo_movsto OF BMO NO-LOCK NO-ERROR.
            FIND SUCURSALES WHERE SUCURSALES.ID_SUCURSAL = BMO.ID_SUC_ORIGEN.
            FIND BSU WHERE BSU.id_sucursal = BMO.ID_SUC_ENVIO.
            
            FIND balanza_tickets WHERE
                balanza_tickets.nro_partida = BIT.nro_partida AND
                balanza_tickets.nro_partida_serial = 1
                NO-LOCK NO-ERROR.
                                         
    
            DISP    BMO.FECHA 
                    BMO.NRO
                    BIT.NRO_PARTIDA
                    BMO.id_suc_origen
                    SUCURSALES.NOMBRE
                    BMO.ID_SUC_ENVIO 
                    BSU.nombre
                    BIT.nro_partida_serial
                    BIT.id_tipo_movsto
                    tipo_movsto.descripcion
                    tipo_movsto.codigo_stock
                    BIT.CANTIDAD
                    BIT.nro 
                    BIT.documento_sap
                    BIT.ESTADO_FRUTA
                    STRING(INTEGER(BIT.orden_entrega_sap),'9999999999') format 'x(14)' column-label "Lote"
                    REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000')
                        FORMAT 'X(19)' label "Material"
               /*     balanza_tickets.cod_barra_sap
                    balanza_tickets.orden_compra_sap
                    balanza_tickets.pos_orden_compra_sap */
                    WITH WIDTH 270 SCROLLABLE.
        END.
        */                       
        RUN MM400INGP.P(BUFFER ITEMS_STOCK , OUTPUT CSTATUS).
    END.

END.

CATCH EX AS Progress.Lang.Error :
    MESSAGE EX:GETMESSAGE(1) VIEW-AS ALERT-BOX ERROR TITLE "ERROR".
    UNDO, LEAVE.
END CATCH.

FINALLY.
    MESSAGE CSTATUS VIEW-AS ALERT-BOX ERROR TITLE "CSTATUS".
END FINALLY. 
