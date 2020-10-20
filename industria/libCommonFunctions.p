&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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

  DEFINE VAR chExcelApplication AS COM-HANDLE.
  DEFINE VAR chWorkbook         AS COM-HANDLE.
  DEFINE VAR chWorkSheet        AS COM-HANDLE.
  DEFINE VAR chChart            AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange   AS COM-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-connectApp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectApp Procedure 
FUNCTION connectApp RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppConfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAppConfig Procedure 
FUNCTION getAppConfig RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFechasSemana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFechasSemana Procedure 
FUNCTION getFechasSemana RETURNS CHARACTER
  (piSemana AS INTEGER, 
   piAnio   AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileNameFromPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileNameFromPath Procedure 
FUNCTION getFileNameFromPath RETURNS CHARACTER
  (cPath AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilterString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilterString Procedure 
FUNCTION getFilterString RETURNS CHARACTER
  (pcValuesList AS CHARACTER, 
   pcOperator   AS CHARACTER, 
   pcField      AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextSequence Procedure 
FUNCTION getNextSequence RETURNS INTEGER
  (pcTable AS CHARACTER,
   pcField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNroSemana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNroSemana Procedure 
FUNCTION getNroSemana RETURNS INTEGER
  (pdFecha AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPersistentLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPersistentLib Procedure 
FUNCTION getPersistentLib RETURNS HANDLE
  (pcLibrary AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProductColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProductColor Procedure 
FUNCTION getProductColor RETURNS INTEGER
  (piArticulo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistryKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegistryKey Procedure 
FUNCTION getRegistryKey RETURNS CHARACTER
  (pcSection AS CHARACTER, 
   pcKey     AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValidUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValidUser Procedure 
FUNCTION getValidUser RETURNS LOGICAL
  (pcUser AS CHARACTER, 
   pcFile AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openExcelApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openExcelApplication Procedure 
FUNCTION openExcelApplication RETURNS COM-HANDLE
  (pcDoc AS CHARACTER, 
   plVis AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reportToPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD reportToPdf Procedure 
FUNCTION reportToPdf RETURNS CHARACTER
  (pcReportLib    AS CHARACTER, 
   pcReportName   AS CHARACTER,
   pcPrinterPdf   AS CHARACTER,
   pcFilterString AS CHARACTER, 
   pcOtherFields  AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistreyKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRegistreyKey Procedure 
FUNCTION setRegistreyKey RETURNS CHARACTER
  (pcSection AS CHARACTER,
   pcKey     AS CHARACTER, 
   pcValue   AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistryKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRegistryKey Procedure 
FUNCTION setRegistryKey RETURNS CHARACTER
  (pcSection AS CHARACTER,
   pcKey     AS CHARACTER, 
   pcValue   AS CHARACTER)  FORWARD.

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

&IF DEFINED(EXCLUDE-addToolbarButton) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addToolbarButton Procedure 
PROCEDURE addToolbarButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phToolBar        AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pcActionName     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcButtonName     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcButtonCaption  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcButtonImage    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcButtonOnChoose AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcButtonParent   AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE xcColumns AS CHARACTER INITIAL "Name,Caption,Image,Type,OnChoose,AccessType,Parent".
  
  &SCOP dlmt + CHR(1) +
   
  
  /* define an action for my button */
  DYNAMIC-FUNCTION("defineAction" IN phToolBar,
                                    pcActionName,
                                    xcColumns,
                                    pcButtonName     {&dlmt}   /* ButtonName*/
                                    pcButtonCaption  {&dlmt}   /* Caption*/
                                    pcButtonImage    {&dlmt}   /* Image */
                                    "PUBLISH"        {&dlmt}   /* TYPE */
                                    pcButtonOnChoose {&dlmt}   /* OnChoose */
                                    "READ"           {&dlmt}   /* AccessType */
                                    pcButtonParent)           /* parent *//* Parent - change it to FUNCTION if you don't want a new group */ 
    NO-ERROR.


END PROCEDURE.

  /*
  /* only to define a new Action Group - NO FUNCIONA            */
  IF pcButtonParent <> "FUNCTION" THEN DO:
    DYNAMIC-FUNCTION("defineAction" IN hToolBar,
                                       pcButtonParent,               /* action group */  
                                       "Name,Caption",
                                       pcButtonParent {&dlmt}          /* Name    */
                                       pcButtonParent {&dlmt}         /* Caption */
                                       "").
  END.
  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-closeExcelApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeExcelApplication Procedure 
PROCEDURE closeExcelApplication :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phApp AS COM-HANDLE     NO-UNDO.

  IF VALID-HANDLE(phApp)              THEN RELEASE OBJECT phApp.
  IF VALID-HANDLE(chExcelApplication) THEN RELEASE OBJECT chExcelApplication.
  IF VALID-HANDLE(chWorkBook)         THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)        THEN RELEASE OBJECT chWorkSheet. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-confirmPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmPdf Procedure 
PROCEDURE confirmPdf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcPdfFile AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER plReturn  AS LOGICAL    NO-UNDO.

  RUN wdConfirmPdf.w (pcPdfFile, OUTPUT plReturn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openPdf Procedure 
PROCEDURE openPdf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcFileName AS CHARACTER  NO-UNDO.

  /* The following Visual Style parameters define the initial state of the application's main window. */
  &GLOBAL-DEFINE SW-SHOWNORMAL 1         /* Start the application in a normal size window. */
  &GLOBAL-DEFINE SW-SHOWMINIMIZED 2      /* Start the application minimized. Show an icon at the bottom of the screen. */
  &GLOBAL-DEFINE SW-SHOWMAXIMIZED 3      /* Start the application in a maximized window. */
  &GLOBAL-DEFINE SW-SHOWNOACTIVATE 4     /* Start the application but set the focus back to the calling program. */
  &GLOBAL-DEFINE SW-SHOWMINNOACTIVE 7    /* Start the application minimized and set the focus back to the calling program. */
  
  DEFINE VARIABLE cProgramName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
  DEFINE VARIABLE cCmdLine      AS CHARACTER  NO-UNDO.
  

  LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
  USE  "SOFTWARE".
  GET-KEY-VALUE SECTION "Microsoft\Windows\CurrentVersion\App Paths\AcroRd32.exe"
                KEY     DEFAULT
                VALUE   cCmdLine.
  UNLOAD "SOFTWARE".

  
  ASSIGN cProgramName = cCmdLine
         cFileName    = pcFileName.
  
  RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT {&SW-SHOWNORMAL}, OUTPUT iReturnResult).
  
  IF iReturnResult < 32 THEN
      MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.


  

 END PROCEDURE.

PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
  DEFINE INPUT PARAMETER ProgramName AS CHARACTER.
  DEFINE INPUT PARAMETER VisualStyle AS LONG.
  DEFINE RETURN PARAMETER StatusCode AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendMail Procedure 
PROCEDURE sendMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcTo      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcSubject AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcBody    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcAttach  AS CHARACTER  NO-UNDO.


  RUN SendMail.p(INPUT "",                    /* SIEMPRE TIENE QUE IR */
                 INPUT 2,                     /* PRIORIDAD */
                 INPUT pcSubject,             /* SUBJECT */
                 INPUT pcBody,                /* BODY     */
                 INPUT pcTo,                  /* DEST. SEP COMAS */
                 INPUT pcAttach).             /* ARCHIVOS ATTACHED SEP POR COMAS */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActiveWindowData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setActiveWindowData Procedure 
PROCEDURE setActiveWindowData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phCurrWin AS WIDGET-HANDLE     NO-UNDO.

  DEFINE VARIABLE hRet AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  
  hLib = SESSION:FIRST-PROCEDURE.

  DO WHILE VALID-HANDLE(hLib):
    IF hLib:FILE-NAME MATCHES "*wInicioIndustria.w" THEN DO:
      hRet = hLib.
      LEAVE.
    END.
    
    hLib = hLib:NEXT-SIBLING.
  END.

  RUN setRunningProc IN hRet (phCurrWin:TITLE, phCurrWin:FILE-NAME).

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-connectApp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectApp Procedure 
FUNCTION connectApp RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hApp    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAsName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHost   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPort   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cConf   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFlg    AS LOGICAL    NO-UNDO.

  ASSIGN cConf   = getAppConfig()
         cAsName = ENTRY(1, cConf)
         cHost   = ENTRY(2, cConf)
         cPort   = ENTRY(3, cConf).
  
  cConf = "-AppService " + cAsName + 
          " -H " + cHost + 
          " -S " + cPort.
    
  CREATE SERVER hApp.  
  lFlg = hApp:CONNECT(cConf) NO-ERROR.

  IF lFlg THEN
    RETURN hApp.
  ELSE 
    RETURN ?.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppConfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAppConfig Procedure 
FUNCTION getAppConfig RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArchivoConf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParam       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValue       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLine        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos         AS INTEGER    NO-UNDO.
  

  cArchivoConf = "..\industria\appserver.conf".
  INPUT FROM VALUE(cArchivoConf).
  REPEAT :
    IMPORT UNFORMATTED cLine.
    IF LENGTH(cLine) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    iPos = INDEX(cLine, "#").
    IF iPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.

    cParam = ENTRY(1, cLine, "=").
    cValue = ENTRY(2, cLine, "=").

    cRet = cRet + TRIM(cValue) + ",".

  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFechasSemana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFechasSemana Procedure 
FUNCTION getFechasSemana RETURNS CHARACTER
  (piSemana AS INTEGER, 
   piAnio   AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFecha AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecTo AS DATE       NO-UNDO.
  DEFINE VARIABLE iSem   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet   AS CHARACTER  NO-UNDO.


  dFecha = DATE(01, 01, piAnio).
  iSem   = getNroSemana(dFecha).

  DO WHILE iSem <> piSemana:
    dFecha = dFecha + 1.
    iSem   = getNroSemana(dFecha).
  END.

  dFecha = dFecha - 1. /*agregue esto para el año 2007*/
  dFecTo = dFecha + 7.
  cRet   = STRING(dFecha + 1) + "," + STRING(dFecTo).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFileNameFromPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileNameFromPath Procedure 
FUNCTION getFileNameFromPath RETURNS CHARACTER
  (cPath AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE c         AS INTEGER  NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  
  c = NUM-ENTRIES(cPath, "\").
  DO i = 1 TO c:
    cFileName = ENTRY(i, cPath, "\").
  END.

  RETURN cFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFilterString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilterString Procedure 
FUNCTION getFilterString RETURNS CHARACTER
  (pcValuesList AS CHARACTER, 
   pcOperator   AS CHARACTER, 
   pcField      AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.


  DO i = 1 TO NUM-ENTRIES(pcValuesList):
    cVal = ENTRY(i, pcValuesList).
    cRet = cRet + " " + pcField + " = " + cVal + " " + pcOperator.
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 3).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextSequence Procedure 
FUNCTION getNextSequence RETURNS INTEGER
  (pcTable AS CHARACTER,
   pcField AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hQry AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hBuf AS HANDLE     NO-UNDO.
  
  DEFINE VARIABLE cTbl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFld AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.
  
  
  cTbl = pcTable.
  cFld = pcField.
  
  
  CREATE QUERY hQry.
  CREATE BUFFER hBuf FOR TABLE cTbl.
  
  hQry:SET-BUFFERS(hBuf).
  hQry:QUERY-PREPARE("FOR EACH " + hBuf:TABLE + " BY " + cFld + " DESC").
  hQry:QUERY-OPEN().
  hQry:GET-FIRST().

  IF hQry:QUERY-OFF-END THEN
    iRet = 1.
  ELSE 
    iRet = hBuf:BUFFER-FIELD(cFld):BUFFER-VALUE + 1.
  
  hQry:QUERY-CLOSE().
  DELETE OBJECT hQry.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNroSemana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNroSemana Procedure 
FUNCTION getNroSemana RETURNS INTEGER
  (pdFecha AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFec AS DATE       NO-UNDO.
  DEFINE VARIABLE iSem AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  

  dFec = DATE('01/01/' + STRING(YEAR(pdFecha))).
  iSem = TRUNCATE(((pdFecha - dFec) / 7 + 1), 0).
  iAno = YEAR(pdFecha).
  
  IF pdFecha > DATE('26/12/2004') AND pdFecha < DATE('03/01/2005') THEN DO:
    iSem = 53.
    IF YEAR(pdFecha) = 2005 THEN 
      iAno = iAno - 1.
  END.

  IF iSem > 52 THEN DO:
    iSem = 1.
    iAno = YEAR(pdFecha) + 1.
  END.

  RETURN iSem.


END FUNCTION.


/*


define variable b   as date.

b = date(01,01,year(fecha)).

semana  = (fecha - b) / 7 + 1.
anio    = year(fecha).

if fecha > date("26/12/04") and fecha < date("03/01/05") then do:
    semana  = 53.
    if year(fecha) = 2005 then anio = anio - 1.
    return.
end.

if semana > 52 then do:
    semana  = 1.
    anio    = year(fecha) + 1.
end.

  RETURN 0.   /* Function return value. */

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPersistentLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPersistentLib Procedure 
FUNCTION getPersistentLib RETURNS HANDLE
  (pcLibrary AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hRet AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  
  hLib = SESSION:FIRST-PROCEDURE.

  DO WHILE VALID-HANDLE(hLib):
    IF hLib:FILE-NAME = pcLibrary THEN DO:
      hRet = hLib.
      LEAVE.
    END.
    hLib = hLib:NEXT-SIBLING.
  END.

  IF NOT VALID-HANDLE(hRet) THEN
    RUN VALUE(pcLibrary) PERSISTENT SET hRet.
  

  RETURN hRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProductColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProductColor Procedure 
FUNCTION getProductColor RETURNS INTEGER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER  NO-UNDO.


  CASE piArticulo: 
    WHEN 20 THEN 
      iRet = 3.

    WHEN 21 THEN
      iRet = 2.

    WHEN 46 THEN
      iRet = 1.

    WHEN 66 THEN
      iRet = 10.

    WHEN 401 THEN
      iRet = 4.

    WHEN 500 THEN
      iRet = 11.

    WHEN 505 THEN
      iRet = 6.

    WHEN 506 THEN
      iRet = 7.

    WHEN 507 THEN
      iRet = 3.

    WHEN 600 THEN
      iRet = 10.

    WHEN 602 THEN
      iRet = 4.

  END CASE.


  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistryKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegistryKey Procedure 
FUNCTION getRegistryKey RETURNS CHARACTER
  (pcSection AS CHARACTER, 
   pcKey     AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cData AS CHARACTER  NO-UNDO.

  LOAD "SOFTWARE" BASE-KEY "HKEY_CURRENT_USER".
  USE "SOFTWARE".
  
  GET-KEY-VALUE SECTION pcSection
  KEY pcKey
  VALUE cData.
  
  UNLOAD "SOFTWARE".
  RETURN (cData).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValidUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValidUser Procedure 
FUNCTION getValidUser RETURNS LOGICAL
  (pcUser AS CHARACTER, 
   pcFile AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose: recorre un archivo de usuarios habilitados y comprueba contra el usuario
           logueado.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcArchivoUsuarios AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLine            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE viPos             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vlPermitido       AS LOGICAL    NO-UNDO INITIAL FALSE.

  vcArchivoUsuarios = "..\industria\" + pcFile.
  INPUT FROM VALUE(vcArchivoUsuarios).
  REPEAT :
    IMPORT UNFORMATTED vcLine.
    IF LENGTH(vcLine) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    viPos = INDEX(vcLine, "#").
    IF viPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.
    
    IF pcUser = vcLine THEN DO:
      vlPermitido = TRUE.      
    END.
  END.

  RETURN vlPermitido.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openExcelApplication) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openExcelApplication Procedure 
FUNCTION openExcelApplication RETURNS COM-HANDLE
  (pcDoc AS CHARACTER, 
   plVis AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF SEARCH(pcDoc) <> ? THEN DO:
    CREATE "Excel.Application" chExcelApplication. 
    chExcelApplication:VISIBLE = plVis. 
    chWorkbook  = chExcelApplication:Workbooks:OPEN(pcDoc). 
  END.
  ELSE
    MESSAGE "No se encontro el archivo" SKIP pcDoc
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  
  

  RETURN chExcelApplication.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reportToPdf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION reportToPdf Procedure 
FUNCTION reportToPdf RETURNS CHARACTER
  (pcReportLib    AS CHARACTER, 
   pcReportName   AS CHARACTER,
   pcPrinterPdf   AS CHARACTER,
   pcFilterString AS CHARACTER, 
   pcOtherFields  AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFilePdf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrevKey AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hApi     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  cPrinter = "pdf995".
  cFilePdf = "..\industria\pdfs\" + pcReportName + ".pdf".

  OS-DELETE VALUE(cFilePdf).

  RUN  aderb\_prntrb2(pcReportLib,                      /* RB-REPORT-LIBRARY */
                      pcReportName,                     /* RB-REPORT-NAME */
                      "",                               /* RB-DB-CONNECTION */
                      "O",                              /* RB-INCLUDE-RECORDS */
                      pcFilterString,                   /* RB-FILTER */
                      RB-MEMO-FILE,                     /* RB-MEMO-FILE */
                      "",                               /* RB-PRINT-DESTINATION */
                      cPrinter,                         /* RB-PRINTER-NAME */
                      "",                               /* RB-PRINTER-PORT */
                      "",                               /* RB-OUTPUT-FILE */
                      1,                                /* RB-NUMBER-COPIES  - zero */                  
                      0,                                /* RB-BEGIN-PAGE - zero */
                      0,                                /* RB-END-PAGE - zero */
                      NO,                               /* RB-TEST-PATTERN */
                      "",                               /* RB-WINDOW-TITLE */
                      YES,                              /* RB-DISPLAY-ERRORS */
                      YES,                              /* RB-DISPLAY-STATUS */
                      NO,                               /* RB-NO-WAIT */
                      pcOtherFields,                    /* RB-OTHER-PARAMETERS */
                      "").   
 
  
  /* delay */
  DO i = 1 TO 3000000:
    j = j + 1.
  END.

  IF SEARCH(cFilePdf) <> ? THEN
    RETURN cFilePdf.
  ELSE 
    RETURN "".



END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistreyKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRegistreyKey Procedure 
FUNCTION setRegistreyKey RETURNS CHARACTER
  (pcSection AS CHARACTER,
   pcKey     AS CHARACTER, 
   pcValue   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  LOAD "SOFTWARE" BASE-KEY "HKEY_CURRENT_USER".
  USE "SOFTWARE".
  
  PUT-KEY-VALUE SECTION pcSection
  KEY pcKey
  VALUE pcValue.
  
  UNLOAD "SOFTWARE".

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistryKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRegistryKey Procedure 
FUNCTION setRegistryKey RETURNS CHARACTER
  (pcSection AS CHARACTER,
   pcKey     AS CHARACTER, 
   pcValue   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  LOAD "SOFTWARE" BASE-KEY "HKEY_CURRENT_USER".
  USE "SOFTWARE".
  
  PUT-KEY-VALUE SECTION pcSection
  KEY pcKey
  VALUE pcValue.
  
  UNLOAD "SOFTWARE".

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

