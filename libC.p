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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-conectaAppServerCadena) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD conectaAppServerCadena Procedure 
FUNCTION conectaAppServerCadena RETURNS HANDLE
  ( INPUT StringConeccion AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-errorNoExisteArchivoAppServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE errorNoExisteArchivoAppServer Procedure 
PROCEDURE errorNoExisteArchivoAppServer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER NombreModulo AS CHARACTER NO-UNDO.
  
  DEFINE VAR NombreArchivo AS CHARACTER NO-UNDO.
  DEFINE VAR StringConeccion AS CHARACTER NO-UNDO FORMAT 'x(20)'.


 RUN adm2/support/gStringConeccion.w (NombreModulo , OUTPUT StringConeccion).


 IF StringConeccion <> '' THEN
 DO:
     NombreArchivo = '../' + NombreModulo + '/appsercon.r'.
     OUTPUT TO VALUE(NombreArchivo).
     EXPORT StringConeccion.
     OUTPUT CLOSE.
 END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStringConeccionAppServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStringConeccionAppServer Procedure 
PROCEDURE getStringConeccionAppServer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER NombreModulo AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER  StringConeccion  AS CHARACTER NO-UNDO.

  DEFINE VAR NombreArchivo AS CHARACTER NO-UNDO.
  DEFINE VAR cadena AS CHARACTER NO-UNDO.
  DEFINE VAR i AS INTEGER NO-UNDO.


  NombreArchivo = '../' + NombreModulo + '/appsercon.r'.
  IF SEARCH(NombreArchivo) = ? THEN 
      RUN errorNoExisteArchivoAppServer(NombreModulo).
  
  INPUT FROM VALUE(NombreArchivo).
  IMPORT UNFORMATTED cadena.
  INPUT CLOSE.
  DO i = 1 TO NUM-ENTRIES(cadena,'|'):
      StringConeccion = StringConeccion  + ' ' + ENTRY(i,cadena,'|').
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-conectaAppServerCadena) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION conectaAppServerCadena Procedure 
FUNCTION conectaAppServerCadena RETURNS HANDLE
  ( INPUT StringConeccion AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VAR hServer AS HANDLE NO-UNDO.

  CREATE SERVER hServer.

  hServer:CONNECT(replace(StringConeccion,'"','')) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      hServer = ?.

  RETURN hServer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

