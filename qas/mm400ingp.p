
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

DEFINE PARAMETER BUFFER items_stock FOR items_stock.
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

    IF textvalue = ? THEN RETURN TRUE.

    DO i = 1 TO (LENGTH(textvalue) - 1):
        INT(SUBSTRING(textvalue, i, (i + 1))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN FALSE.
    END.

    RETURN TRUE.
END FUNCTION.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


RUN CREATE-REQUEST (BUFFER ITEMS_STOCK, OUTPUT MT_Request).

        
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
FOR EACH RETORNO.
    IF NOT ISNUMERIC(RETORNO.Entrega) THEN
    DO:
        CSTATUS = RETORNO.MENSAJE.
        UNDO, RETURN ERROR NEW Progress.Lang.AppError(CSTATUS,550). 
    END.
    ELSE CSTATUS = CSTATUS + RETORNO.MENSAJE + CHR(10).
END.
IF TRIM(REPLACE(CSTATUS, CHR(10) , ''))= '' THEN
    CSTATUS = 'ENTREGA CONTABILIZADA'. 

CATCH E AS Progress.Lang.Error:
    CSTATUS = E:GETMESSAGE(1).
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
    DEFINE PARAMETER BUFFER ITEMS_STOCK FOR ITEMS_STOCK.
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

    DEFINE VARIABLE CCENTROD         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE CALMACEND        AS CHARACTER NO-UNDO.



    FIND CURRENT  items_stock NO-LOCK.
    
    FIND movsucu OF items_stock.
        
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
    hText:NODE-VALUE = STRING(STRING(movsucu.ID_TIPO_MOVSTO, '999') + STRING(movsucu.NRO, '999999')).

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
    hText:NODE-VALUE = '00010'.

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

    hDoc:CREATE-NODE(hRoot1,"Picking","ELEMENT").
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
    hText:NODE-VALUE = '00010'.

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

    
    hDoc:ENCODING= "UTF-8".
    hDoc:SAVE("LONGCHAR", MT_Request).
    
    DELETE OBJECT hDoc.
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


    



        

