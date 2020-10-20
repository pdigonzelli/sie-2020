
/*------------------------------------------------------------------------
    File        : pp159ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Feb 20 20:20:51 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT    PARAMETER VPALLET AS CHARACTER NO-UNDO.
DEFINE OUTPUT   PARAMETER CSTATUS AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE resultado NO-UNDO
    NAMESPACE-URI "" 
    FIELD EXIDV AS CHARACTER 
    FIELD result AS CHARACTER 
    FIELD MESSAGE1 AS CHARACTER 
        XML-NODE-NAME "MESSAGE" .

DEFINE DATASET MT_Response NAMESPACE-URI "http://sanmiguel.com/PP377ING" 
    FOR resultado.

 

DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hSI_OS_PP377ING AS HANDLE NO-UNDO.
DEFINE VARIABLE MT_Request AS LONGCHAR NO-UNDO.

DEFINE VAR EX AS Progress.Lang.AppError.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN CREATE-REQUEST(VPALLET, OUTPUT MT_Request).
        
CREATE SERVER hWebService.
hWebService:CONNECT("-WSDL 'http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/08b3370daf2a396592e703975276cdeb'
                     -WSDLUserid PIAPPLPID
                     -WSDLPassword SanMigu3l2015
                     -SOAPEndpointUserid PIAPPLPID 
                     -SOAPEndpointPassword SanMigu3l2015
                     -Service SI_OS_PP377INGService
                     -Port HTTP_Port").


RUN SI_OS_PP377ING SET hSI_OS_PP377ING ON hWebService.
RUN SI_OS_PP377ING IN hSI_OS_PP377ING(INPUT MT_Request, OUTPUT DATASET MT_Response).

FOR EACH RESULTADO.
    CSTATUS = CSTATUS + " " + resultado.MESSAGE1.
    IF RESULTADO.RESULT <> 'OK'THEN
    DO:
        CSTATUS =  resultado.MESSAGE1.
        EX = NEW Progress.Lang.AppError(CSTATUS, 550).
        UNDO, THROW EX.
    END.
END.


CATCH E AS Progress.Lang.Error:
    CSTATUS = E:GETMESSAGE(1).
    UNDO, THROW E.
END CATCH.


FINALLY:

END FINALLY.
    


/* **********************  Internal Procedures  *********************** */

PROCEDURE CREATE-REQUEST:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PVPALLET      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER MT_Request  AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE hDoc            AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRow            AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot           AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot1          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hField          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hText           AS HANDLE NO-UNDO.
    
    
    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRoot1.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    
    hDoc:CREATE-NODE(hRoot,"MT_Request","ELEMENT").
    hDoc:APPEND-CHILD(hRoot).

    hDoc:CREATE-NODE(hRoot1,"Unidad","ELEMENT").
    hRoot:APPEND-CHILD(hRoot1).

    hDoc:CREATE-NODE(hField,"EXIDV","ELEMENT"). /* create  a row node */
    hDoc:CREATE-NODE(hText, "", "TEXT").
    hField:APPEND-CHILD(hText).
    hRoot1:APPEND-CHILD(hField).
    hText:NODE-VALUE = STRING(PVPALLET).
    
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


    



        

