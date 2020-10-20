&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE  INPUT PARAMETER pibalanza           AS INTEGER NO-UNDO.
DEFINE  INPUT PARAMETER pipesada            AS INTEGER NO-UNDO.
DEFINE  INPUT PARAMETER plprogress2Sap      AS LOGICAL NO-UNDO.
DEFINE  OUTPUT PARAMETER pcStatus           AS CHARACTER.

DEFINE VARIABLE hlib      AS HANDLE.
DEFINE VARIABLE v_balanza AS INTEGER.
DEFINE VARIABLE v_pesada  AS INTEGER.
DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.

DEFINE VARIABLE fok       AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMes      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMes1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY      AS LOGICAL NO-UNDO.
DEFINE VARIABLE cprod     AS CHARACTER NO-UNDO.
DEFINE VARIABLE FPR       AS LOGICAL NO-UNDO.
DEFINE VAR cError         AS CHARACTER NO-UNDO.
DEFINE VAR xcod AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


DO TRANSACTION :
                                                
    pcStatus = 'OK'.

    FIND balanza_pesadas WHERE balanza_pesadas.id_balanza = pibalanza AND
                               balanza_pesadas.id_pesada  = pipesada NO-LOCK NO-ERROR.

    IF NOT AVAILABLE balanza_pesadas THEN
    DO:
        pcStatus = 'Registro de pesada inexistente'.
        UNDO , RETURN ERROR NEW Progress.Lang.AppError(pcStatus).
        
    END.


    ASSIGN v_balanza = balanza_pesadas.id_balanza
           v_pesada  = balanza_pesadas.id_pesada.
    
    fok = TRUE.
    FPR = FALSE.    


    FOR EACH balanza_tickets OF balanza_pesadas:
        IF balanza_tickets.id_tipo_cosecha = 0 /* descarte */ OR balanza_tickets.id_tipo_cosecha >= 4 /* procesado y otros */ OR NOT balanza_tickets.finca THEN 
        DO:
            IF balanza_pesadas.tara = 0 THEN FPR = TRUE.
            NEXT.
        END.
    END.


    /*************************************/
    /******* Graba Ingresos en SAP *******/
    /*************************************/
    CASE FPR :
        WHEN TRUE  THEN
             RUN pimprimeEtiquetasIngreso.p (balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , OUTPUT xcod) NO-ERROR.
        WHEN FALSE THEN
            IF FOK THEN
            DO:
                FIND CURRENT BALANZA_PESADAS NO-LOCK NO-ERROR.
                xcod = ''.
                RUN pimprimeEtiquetasIngreso.p (balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , OUTPUT xcod) NO-ERROR.
            END.
    END CASE.
    CATCH vError AS Progress.Lang.ProError.
        pcStatus = pcStatus + vError:GETMESSAGE(1).
        UNDO , THROW NEW Progress.Lang.AppError(pcStatus, 550).
    END CATCH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


