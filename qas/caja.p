
/*------------------------------------------------------------------------
    File        : mm050ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sun Dec 20 19:25:32 ACT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
ROUTINE-LEVEL ON ERROR UNDO, THROW.
SESSION:NUMERIC-FORMAT = 'european'.

FUNCTION WLOG RETURNS LOGICAL
    ( INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER)  FORWARD.


DEFINE INPUT  PARAMETER PCETIQUETA      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER PCLECTOR        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER PCRESPUESTA     AS CHARACTER NO-UNDO.

DEFINE VAR VTEXTO AS CHARACTER NO-UNDO.
DEFINE VAR LSTATUS AS LOGICAL NO-UNDO.

DEFINE STREAM ESTADO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
SESSION:TIME-SOURCE = 'general'.


RUN LEEETIQUETA1(PCETIQUETA,PCLECTOR,OUTPUT LSTATUS).

IF LSTATUS THEN PCRESPUESTA = 'OK'. ELSE PCRESPUESTA = 'NOK'.


CATCH ErrC AS Progress.Lang.ProError .
    PCRESPUESTA = 'NOK'.
    UNDO.
END CATCH.

FINALLY.
END FINALLY.

PROCEDURE leeEtiqueta1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PCETIQUETA AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER PCLECTOR AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER lStatus AS LOGICAL NO-UNDO.


        
    OUTPUT STREAM ESTADO TO VALUE(SESSION:TEMP-DIRECTORY + "ESTADO.TXT") APPEND.

    PUT  STREAM ESTADO UNFORMATTED "____________________________________"  STRING(NOW,"99/99/9999 hh:mm:ss") SKIP.
    PUT  STREAM ESTADO UNFORMATTED "COMIENZA GRABACION ETIQUETA " PCETIQUETA " " PCLECTOR SKIP.

       
    FIND FINES_LINEA WHERE FINES_LINEA.CODIGO =  PCLECTOR NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FINES_LINEA THEN
    DO:
        lStatus = FALSE.
        PUT STREAM ESTADO UNFORMATTED 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA' 'NO ENCUENTRA FIN LINEA'.
        RETURN ERROR 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA. NO ENCUENTRA FIN DE LINEA'.
    END.    
    
    REPEAT:
        FIND cajas WHERE cajas.etiqueta = DECIMAL(PCEtiqueta) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF LOCKED(cajas) THEN  NEXT.
        ASSIGN  cajas.leida = TRUE
                cajas.lectura = NOW
                cajas.id_suc_trabajo_lector_fin_linea = IF AVAILABLE FINES_LINEA THEN  fines_linea.ID_SUC_TRABAJO ELSE 0
                cajas.id_lector_fin_linea = IF AVAILABLE fines_linea THEN  fines_linea.id_FIN_LINEA ELSE 0
                cajas.cant_lecturas = cajas.cant_lecturas + 1.                
        FIND CURRENT cajas  NO-LOCK NO-ERROR.
        PUT STREAM ESTADO UNFORMATTED "GRABO CAJA " PCETIQUETA FORMAT "99999999999999" SKIP .
        lstatus = TRUE.
        LEAVE.
    END.

    IF LSTATUS THEN
    DO:
        REPEAT:
            FIND CURRENT FINES_LINEA EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(FINES_LINEA) THEN  NEXT.
            IF FINES_LINEA.PRIMER_LECTURA = ? THEN
                ASSIGN    FINES_LINEA.PRIMER_LECTURA    = CAJAS.LECTURA 
                          FINES_LINEA.PRIMER_CAJA       = STRING(CAJAS.ETIQUETA,"99999999999999").

            ASSIGN  FINES_LINEA.ULTIMA_LECTURA = cajas.lectura 
                    FINES_LINEA.ULTIMA_CAJA = STRING(CAJAS.ETIQUETA,"99999999999999")
                    FINES_LINEA.CANT = FINES_LINEA.CANT + 1.
            FIND CURRENT FINES_LINEA  NO-LOCK NO-ERROR.
            PUT STREAM ESTADO UNFORMATTED "ACTUALIZO LECTOR " PCLECTOR SKIP. 
            LEAVE.
        END.
    END.
    
    
    CATCH ERROR1 AS Progress.Lang.SysError.
        lStatus = FALSE.
        PUT STREAM ESTADO UNFORMATTED 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA' ERROR1:GETMESSAGE(1).
        RETURN ERROR 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA' + ERROR1:GETMESSAGE(1).
    END CATCH.
    
    CATCH ERROR2 AS Progress.Lang.AppError.
        lStatus = FALSE.
        PUT STREAM ESTADO UNFORMATTED 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA' ERROR2:GETMESSAGE(1).
        RETURN ERROR 'ERROR DE GRABACION DE LECTURA DE FIN DE LINEA EN CAJA' + ERROR2:GETMESSAGE(1).
    END CATCH.
    
    FINALLY:
        PUT STREAM ESTADO UNFORMATTED 'TERMINA GRABACION ETIQUETA ' PCETIQUETA PCLECTOR SKIP.
        OUTPUT STREAM ESTADO CLOSE.
    END FINALLY.
    
END PROCEDURE.



FUNCTION devuelvefechaoperativa RETURNS DATE
    ( INPUT pfecha AS DATE, INPUT phora AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:   
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE vfechaop AS DATE.
  
    FIND FIRST fechas_operativas WHERE
        (fechas_operativas.fecha_inicio = pfecha AND phora >= fechas_operativas.hora_inicio) OR
        (fechas_operativas.fecha_fin = pfecha AND phora <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
    IF  AVAILABLE fechas_operativas THEN 
        vfechaop = fechas_operativas.fecha_inicio.



    RETURN vfechaop.
END FUNCTION.     

