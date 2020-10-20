
/*------------------------------------------------------------------------
    File        : mm400ingp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Feb 20 20:20:51 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/



/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE T-ITEMS
    FIELD T-SUC     AS INTEGER
    FIELD T-TIPO    AS INTEGER
    FIELD T-NRO     AS INTEGER
    FIELD T-ITEM    AS INTEGER
    INDEX I AS UNIQUE T-SUC T-TIPO T-NRO T-ITEM.



DEFINE INPUT PARAMETER TABLE FOR T-ITEMS.
DEFINE OUTPUT PARAMETER CSTATUS AS CHARACTER NO-UNDO.
 


DEFINE TEMP-TABLE Retorno NO-UNDO
    NAMESPACE-URI "" 
    FIELD ID AS CHARACTER 
    FIELD Entrega AS CHARACTER 
    FIELD Mensaje AS CHARACTER .
    
DEFINE DATASET MT_Response NAMESPACE-URI "http://sanmiguel.com/MM400ING" 
    FOR Retorno.


DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hSI_OS_MM400ING AS HANDLE NO-UNDO.
DEFINE VARIABLE MT_Request AS LONGCHAR NO-UNDO.

DEFINE VAR EX AS Progress.Lang.AppError.

FUNCTION isNumeric RETURNS LOGICAL (textvalue AS CHAR):
    DEF VAR i AS INT NO-UNDO.

    IF textvalue = ? OR LENGTH(textvalue) = 0 THEN RETURN FALSE.

    DO i = 1 TO (LENGTH(textvalue) - 1):
        INT(SUBSTRING(textvalue, i, (i + 1))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN FALSE.
    END.

    RETURN TRUE.
END FUNCTION.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


RUN CREATE-REQUEST (INPUT TABLE T-ITEMS, OUTPUT MT_Request).

        
CREATE SERVER hWebService.
/*hWebService:CONNECT("-WSDL 'http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/e2114f11e44e3c51bdf1451e40ef74c8'*/
hWebService:CONNECT("-WSDL 'http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/e3d0e1d88a9a36348f08cf12f590cd5f'
                     -WSDLUserid PIAPPLPID
                     -WSDLPassword SanMigu3l2015
                     -SOAPEndpointUserid PIAPPLPID 
                     -SOAPEndpointPassword SanMigu3l2015
                     -Service SI_OS_MM400INGService
                     -Port HTTP_Port").



RUN SI_OS_MM400ING SET hSI_OS_MM400ING ON hWebService.
RUN SI_OS_MM400ING IN hSI_OS_MM400ING(INPUT MT_Request, OUTPUT DATASET MT_Response).

CSTATUS = ''.

FIND FIRST T-ITEMS.
FOR EACH RETORNO.
    IF NOT ISNUMERIC(RETORNO.Entrega) THEN
    DO:
        CSTATUS = CSTATUS + CHR(10) + CHR(13) + 
                 'SUC:' + STRING(T-ITEMS.T-SUC) + ' TIPO:' + STRING(T-ITEMS.T-TIPO) + ' NRO:' + STRING(T-ITEMS.T-NRO)  + ' ITEM:' 
                 + STRING(T-ITEMS.T-NRO) + ' RESULTADO:' + RETORNO.MENSAJE.
        UNDO, RETURN ERROR NEW Progress.Lang.AppError(CSTATUS,550). 
    END.
    ELSE 
    DO:
    
        FOR EACH T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO.
            FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                    ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                    ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                    ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-ERROR.
            IF AVAILABLE items_stock THEN
            DO:
                ASSIGN items_stock.orden_entrega_procesado = RETORNO.ENTREGA.
                CSTATUS = CSTATUS + CHR(10) + CHR(13) + 
                     'PARTIDA:' + STRING(ITEMS_STOCK.NRO_PARTIDA) + ' SERIAL:' + STRING(items_stock.NRO_PARTIDA_SERIAL)  + ' ENTREGA:' + RETORNO.ENTREGA.
            END.
            ELSE 
                CSTATUS = CSTATUS + CHR(10) + CHR(13) + 
                     'PARTIDA: XXXX ' + ' SERIAL:XXXX ' + ' ENTREGA:' + RETORNO.ENTREGA.
                
            FIND movsucu OF items_stock NO-ERROR.
            IF AVAILABLE movsucu THEN 
                ASSIGN  movsucu.orden_entrega_procesado = RETORNO.ENTREGA. 
        END.
    
/*        CSTATUS = CSTATUS + CHR(10) + CHR(13) + 
                 'SUC:' + STRING(T-ITEMS.T-SUC) + ' TIPO:' + STRING(T-ITEMS.T-TIPO) + ' NRO:' + STRING(T-ITEMS.T-NRO)  + ' ITEM:' 
                 + STRING(T-ITEMS.T-NRO) + ' ENTREGA:' + RETORNO.ENTREGA. */
    END.
END.

IF TRIM(REPLACE(CSTATUS, CHR(10) , ''))= '' THEN
    CSTATUS = 'ENTREGA CONTABILIZADA'. 

CATCH E AS Progress.Lang.AppError:
    CSTATUS = E:GETMESSAGE(1).
    IF CSTATUS = '' THEN
        CSTATUS = ERROR-STATUS:GET-MESSAGE(1).
    IF CSTATUS = '' THEN
        CSTATUS = RETURN-VALUE.
    UNDO, RETURN ERROR NEW Progress.Lang.AppError(CSTATUS,550). 
END CATCH.


FINALLY:
    
END FINALLY.
    

/* **********************  Internal Procedures  *********************** */

PROCEDURE CREATE-REQUEST:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE  FOR T-ITEMS.
    DEFINE OUTPUT PARAMETER MT_Request AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE hDoc            AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRow            AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot           AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot1          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hField          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hText           AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE CCENTRO         AS CHARACTE NO-UNDO.
    DEFINE VARIABLE CALMACEN        AS CHARACTER NO-UNDO.

    DEFINE VARIABLE CMATERIALSAP    AS CHARACTE NO-UNDO.
    DEFINE VARIABLE CLOTESAP        AS CHARACTER NO-UNDO.


    DEFINE VARIABLE CMATERIALSAPR   AS CHARACTE NO-UNDO.
    DEFINE VARIABLE CLOTESAPR       AS CHARACTER NO-UNDO.


    DEFINE VARIABLE CCENTROD         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE CALMACEND        AS CHARACTER NO-UNDO.


    DEFINE VARIABLE CCENTRODR         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE CALMACENDR        AS CHARACTER NO-UNDO.

    DEFINE VARIABLE CNRO             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE IPOSICION        AS INTEGER NO-UNDO.



    FOR LAST T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.
        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.
        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
        IF AVAILABLE movsucu THEN 
            CNRO = STRING(STRING(movsucu.id_tipo_movsto, '999') + STRING(movsucu.nro, '999999')) . 
    END.


    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRoot1.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    
    hDoc:CREATE-NODE(hRoot,"MT_Request","ELEMENT").
    hDoc:APPEND-CHILD(hRoot).

    hDoc:CREATE-NODE(hField,"ID","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText).
    hRoot:APPEND-CHILD(hField).
    hText:NODE-VALUE = CNRO.

    hDoc:CREATE-NODE(hField,"Sociedad","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText   ).
    hRoot:APPEND-CHILD(hField).
    hText:NODE-VALUE = 'AR10'.


    hDoc:CREATE-NODE(hField,"Org_Compras","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText).
    hRoot:APPEND-CHILD(hField).
    hText:NODE-VALUE = 'A500'.


    hDoc:CREATE-NODE(hField,"Gpo_Compras","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText).
    hRoot:APPEND-CHILD(hField).
    hText:NODE-VALUE = '502'.

    hDoc:CREATE-NODE(hField,"Centro_Suministrador","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText).
    hRoot:APPEND-CHILD(hField).
    hText:NODE-VALUE = 'A100'.




    FOR FIRST T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.
        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.

        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
        
            
        FIND balanza_tickets WHERE
             balanza_tickets.nro_partida = ITEMS_STOCK.nro_partida AND
             balanza_tickets.nro_partida_serial = 1
             NO-LOCK.
        
        CASE movsucu.id_suc_ORIGEN:
            WHEN 110 THEN 
                ASSIGN CCENTRODR  = 'A100'
                       CALMACENDR = '1102'. 
            WHEN 121 THEN 
                ASSIGN CCENTRODR  = 'A100'
                       CALMACENDR = '1301'. 
        END CASE.
    
    
        IF items_stock.estado_fruta THEN  
            CMATERIALSAPR = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), 
                                    SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
        ELSE    
            CMATERIALSAPR = SUBSTRING(balanza_tickets.cod_barra_sap,1,11).

    END.


    DEFINE VAR ICANTIDAD AS INTEGER.
    
    FOR EACH T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.
        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.

        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
        
            
        FIND balanza_tickets WHERE
             balanza_tickets.nro_partida = ITEMS_STOCK.nro_partida AND
             balanza_tickets.nro_partida_serial = 1
             NO-LOCK.
    
    
        
        CASE movsucu.id_suc_ORIGEN:
            WHEN 110 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1102'. 
            WHEN 121 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1301'. 
        END CASE.
    
        IF CCENTROD     <> CCENTRODR   THEN 
            RETURN ERROR NEW Progress.Lang.AppError('MAS DE UN CENTRO. NO SE PUEDE REALIZAR EL MOVIMIENTO',550). .
        IF CALMACEND    <> CALMACENDR  THEN 
            RETURN ERROR NEW Progress.Lang.AppError('MAS DE UN ALMACEN. NO SE PUEDE REALIZAR EL MOVIMIENTO').
    
        IF items_stock.estado_fruta THEN  
           CMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), 
                                    SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
        ELSE
            CMATERIALSAP = SUBSTRING(balanza_tickets.cod_barra_sap,1,11).

        IF CMATERIALSAP     <> CMATERIALSAPR   THEN RETURN ERROR 'DISTINTOS MATERIALES. NO SE PUEDE REALIZAR EL MOVIMIENTO'.

        ICANTIDAD = ICANTIDAD + items_stock.CANTIDAD.

    END.


    FOR FIRST T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.

        IPOSICION = IPOSICION + 10.

        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.

        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
        
            
        FIND balanza_tickets WHERE
             balanza_tickets.nro_partida = ITEMS_STOCK.nro_partida AND
             balanza_tickets.nro_partida_serial = 1
             NO-LOCK.
        
        CASE movsucu.id_suc_ORIGEN:
            WHEN 110 THEN 
                ASSIGN CCENTRODR  = 'A100'
                       CALMACENDR = '1102'. 
            WHEN 121 THEN 
                ASSIGN CCENTRODR  = 'A100'
                       CALMACENDR = '1301'. 
        END CASE.
    
    
        IF items_stock.estado_fruta THEN  
            CMATERIALSAPR = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), 
                                    SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
        ELSE    
            CMATERIALSAPR = SUBSTRING(balanza_tickets.cod_barra_sap,1,11).


        hDoc:CREATE-NODE(hRoot1,"Posiciones","ELEMENT").
        hRoot:APPEND-CHILD(hRoot1).

    
    
        hDoc:CREATE-NODE(hField,"ID","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = CNRO.
    
        hDoc:CREATE-NODE(hField,"Posicion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = STRING(IPOSICION, '99999').
    
        hDoc:CREATE-NODE(hField,"Material","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CMATERIALSAPR.
    
        hDoc:CREATE-NODE(hField,"Centro","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CCENTRODR.
    
        hDoc:CREATE-NODE(hField,"Almacen","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CALMACENDR.
    
        hDoc:CREATE-NODE(hField,"Cantidad","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  STRING(ICANTIDAD * 20).
    
        hDoc:CREATE-NODE(hField,"Clase_Valoracion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  "".


    END.


/*
    FOR EACH T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.

        IPOSICION = IPOSICION + 10.

        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.

        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
        
            
        FIND balanza_tickets WHERE
             balanza_tickets.nro_partida = ITEMS_STOCK.nro_partida AND
             balanza_tickets.nro_partida_serial = 1
             NO-LOCK.
            
        CASE movsucu.id_suc_ORIGEN:
            WHEN 110 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1102'. 
            WHEN 121 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1301'. 
        END CASE.
    
    
        IF items_stock.estado_fruta THEN  
           CMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), 
                                    SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
        ELSE
            CMATERIALSAP = SUBSTRING(balanza_tickets.cod_barra_sap,1,11).
        
        
        hDoc:CREATE-NODE(hRoot1,"Posiciones","ELEMENT").
        hRoot:APPEND-CHILD(hRoot1).

    
    
        hDoc:CREATE-NODE(hField,"ID","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = STRING(STRING(movsucu.ID_TIPO_MOVSTO, '999') + STRING(movsucu.NRO, '999999')).
    
        hDoc:CREATE-NODE(hField,"Posicion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = STRING(IPOSICION, '99999').
    
        hDoc:CREATE-NODE(hField,"Material","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CMATERIALSAP.
    
        hDoc:CREATE-NODE(hField,"Centro","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CCENTROD.
    
        hDoc:CREATE-NODE(hField,"Almacen","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CALMACEND.
    
        hDoc:CREATE-NODE(hField,"Cantidad","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  STRING(items_stock.CANTIDAD * 20).
    
        hDoc:CREATE-NODE(hField,"Clase_Valoracion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  "".
    
    END.
*/
    
    IPOSICION = 0.


    FOR EACH T-ITEMS BY T-ITEMS.T-SUC BY T-ITEMS.T-TIPO BY T-ITEMS.T-NRO BY T-ITEMS.T-ITEM.

        IPOSICION = 10.

        FIND items_stock WHERE  ITEMS_STOCK.ID_SUCURSAL     = T-ITEMS.T-SUC AND
                                ITEMS_STOCK.ID_TIPO_MOVSTO  = T-ITEMS.T-TIPO AND
                                ITEMS_STOCK.NRO             = T-ITEMS.T-NRO AND
                                ITEMS_STOCK.ITEM            = T-ITEMS.T-ITEM  NO-LOCK NO-ERROR.

        FIND movsucu OF items_stock NO-LOCK NO-ERROR.
            
        FIND balanza_tickets WHERE
             balanza_tickets.nro_partida = ITEMS_STOCK.nro_partida AND
             balanza_tickets.nro_partida_serial = 1
             NO-LOCK.
    
    
        CASE movsucu.id_suc_envio:
            WHEN 101 THEN 
                ASSIGN CCENTRO = 'A100'
                       CALMACEN = '1101'. 
            WHEN 106 THEN 
                ASSIGN CCENTRO = 'A100'
                       CALMACEN = '1102'. 
            WHEN 111 THEN 
                ASSIGN CCENTRO = 'A100'
                       CALMACEN = '1200'. 
            WHEN 114 THEN 
                ASSIGN CCENTRO = 'A100'
                       CALMACEN = '1201'. 
        END CASE.
        
        CASE movsucu.id_suc_ORIGEN:
            WHEN 110 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1102'. 
            WHEN 121 THEN 
                ASSIGN CCENTROD  = 'A100'
                       CALMACEND = '1301'. 
        END CASE.
    
    
        IF items_stock.estado_fruta THEN  
        DO:
           CMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), 
                                  SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
           CLOTESAP     = STRING(INTEGER(ITEMS_stock.orden_entrega_sap),'9999999999').
        END.
        ELSE
        DO:
            CMATERIALSAP = SUBSTRING(balanza_tickets.cod_barra_sap,1,11).
            CLOTESAP     = SUBSTRING(balanza_tickets.cod_barra_sap,12).
        END.        
        

        hDoc:CREATE-NODE(hRoot1,"Picking","ELEMENT").
        hRoot:APPEND-CHILD(hRoot1).

    
    
        hDoc:CREATE-NODE(hField,"ID","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = CNRO.
    
        hDoc:CREATE-NODE(hField,"Posicion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE = STRING(IPOSICION, '99999').
    
        hDoc:CREATE-NODE(hField,"Material","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CMATERIALSAP.
    
        hDoc:CREATE-NODE(hField,"Almacen","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CALMACEN.
    
        hDoc:CREATE-NODE(hField,"Lote","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  CLOTESAP.
    
        hDoc:CREATE-NODE(hField,"Cantidad","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot1:APPEND-CHILD(hField).
        hText:NODE-VALUE =  STRING(items_stock.CANTIDAD * 20).

    END.
    
    hDoc:ENCODING= "UTF-8".
    hDoc:SAVE("LONGCHAR", MT_Request).
    
    hDoc:SAVE("FILE", 'C:\TEMP\PRUEBA.XML').
    
    DELETE OBJECT hRoot.
    DELETE OBJECT hRoot1.
    DELETE OBJECT hRow.
    DELETE OBJECT hField.
    DELETE OBJECT hText.


	CATCH e AS Progress.Lang.Error:
	    UNDO, THROW E.

	END CATCH.


	FINALLY:

	END FINALLY.
END PROCEDURE.


    



        

