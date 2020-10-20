
/*------------------------------------------------------------------------
    File        : pruebapallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Mar 15 08:58:30 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VAR cStatus AS CHARACTER NO-UNDO.
DEFINE VAR DOC1 AS CHARACTER NO-UNDO.
DEFINE VAR CPALLET AS CHARACTER NO-UNDO.

DEFINE BUFFER BP FOR PALLETS.
/* ***************************  Main Block  *************************** */
ETIME(TRUE).
FOR EACH  PALLETS WHERE (id_suc_trabajo = 97 OR id_suc_trabajo = 98) AND 
                        pallets.fecha_prod >= TODAY - 7  AND 
                        pallets.hora_prod < string(TIME - 600 , 'HH:MM:SS') AND
                        pallets.ESTADO AND
                        PALLETS.ID_PALLET_SAP = ''.
    IF pallets.temporada = 99 THEN 
    DO:

        OUTPUT TO VALUE('D:\TEMP\DPA0' + STRING(TODAY, '99-99-9999') + '.DEC') APPEND.
        PUT UNFORMATTED 'PALLET #' + STRING(pallets.id_pallet) +  ' TIENE TEMPORADA 99  '  + RETURN-VALUE.
        PUT UNFORMATTED CHR(10).
        OUTPUT CLOSE.
        NEXT.
    END.
    /*RUN DECLARAPALLETSAPNUEVO (PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET) NO-ERROR .*/ 
    IF ERROR-STATUS:ERROR THEN
    DO:
        OUTPUT TO VALUE('D:\TEMP\DPA0' + STRING(TODAY, '99-99-9999') + '.DEC') APPEND.
        PUT UNFORMATTED 'PALLET #' + STRING(pallets.id_pallet) +  ' FALLO EN LA DECLARACION  '  + RETURN-VALUE.
        PUT UNFORMATTED CHR(10).
        OUTPUT CLOSE.
    END.
    ELSE
    DO:    
        FIND BP WHERE BP.ID_SUC_TRABAJO = PALLETS.ID_SUC_TRABAJO AND
                      BP.ID_PALLET = PALLETS.ID_PALLET NO-LOCK NO-ERROR.
        IF AVAILABLE BP THEN
        DO:
            /*MESSAGE 'SE DECLARO EL PALLET #' BP.ID_PALLET ' CON UNIDAD DE MANIPULEO ' BP.ID_PALLET_SAP VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            OUTPUT TO VALUE('D:\TEMP\DPA0' + STRING(TODAY, '99-99-9999') + '.DEC') APPEND.
            PUT UNFORMATTED STRING(ETIME)+ ' SE DECLARO EL PALLET #' + STRING(BP.ID_PALLET) + ' CON UNIDAD DE MANIPULEO ' + STRING(BP.ID_PALLET_SAP) .
            PUT UNFORMATTED CHR(10).
            OUTPUT CLOSE.
        END.
    END.
END.    

/*
RUN DECLARAPALLETSAPNUEVO (PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET) .
FIND CURRENT PALLETS NO-LOCK.
MESSAGE pallets.id_pallet_sap VIEW-AS ALERT-BOX INFORMATION TITLE 'PALLET SAP'.
CATCH EX  AS Progress.Lang.Error :
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.		
END CATCH.
*/


{declaraPalletn.i}