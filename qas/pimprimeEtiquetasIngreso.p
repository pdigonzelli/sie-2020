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

DEFINE INPUT PARAMETER  pibalanza    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  pipesada     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER pocod        AS CHARACTER NO-UNDO.


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
 
 FIND FIRST balanza_pesadas WHERE   balanza_pesadas.id_balanza  = pibalanza AND 
                                    balanza_pesadas.id_pesada   = pipesada NO-LOCK NO-ERROR.
 IF NOT AVAILABLE balanza_pesadas THEN
     RETURN ERROR 'No puedo imprimir etiquetas de una pesada inexistente'.
 
 xcod = ''.

 FOR EACH balanza_tickets OF balanza_pesadas NO-LOCK:
     RUN dd_etibal_sap.p (input balanza_tickets.nro_partida , balanza_tickets.nro_partida_serial). 
     xcod = xcod + CHR(13) + 'Item: ' + balanza_tickets.cod_barra_sap.
 END.
 CATCH ex AS PROGRESS.Lang.PRoerror .
    RETURN ERROR ex:getmessage(1).
 END CATCH.
 FINALLY.
    pocod = xcod.
 END FINALLY.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


