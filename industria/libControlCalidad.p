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

DEFINE VARIABLE selectedNode AS COM-HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getLibName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLibName Procedure 
FUNCTION getLibName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPopupMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPopupMenu Procedure 
FUNCTION getPopupMenu RETURNS HANDLE
  (phMenu AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUsuario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUsuario Procedure 
FUNCTION getUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

RUN libToDos.p PERSISTENT SET hLib.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib, SEARCH-SELF).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-buttonAction1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttonAction1 Procedure 
PROCEDURE buttonAction1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTreeView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeView Procedure 
PROCEDURE fillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  RUN SUPER (phTree, pcArgs).

  CASE ENTRY(4, pcArgs):
    WHEN "jugos"   THEN RUN fillTreeWithJugos (phTree, pcArgs).
    WHEN "aceites" THEN RUN fillTreeWithAceites (phTree, pcArgs).
    WHEN "cascara" THEN RUN fillTreeWithCascara (phTree, pcArgs).
  END CASE.

  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTreeWithAceites) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeWithAceites Procedure 
PROCEDURE fillTreeWithAceites :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iWeek AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE cFlia AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRgo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dDesde AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta AS DATE       NO-UNDO.

  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chAux   AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iSemDes = INTEGER(ENTRY(1,pcArgs)).
  iSemHas = INTEGER(ENTRY(2, pcArgs)).
  iAnio   = INTEGER(ENTRY(3, pcArgs)).
  cFlia   = ENTRY(4, pcArgs).
  iWeek   = iSemDes.


  /*phTree:NODES:CLEAR().*/

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iWeek, iAnio).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.
  
  chNode  = phTree:Nodes:ADD(, , "CtrlCalidadAceite" + STRING(iWeek), "Control Calidad Aceite Semana "  + STRING(iWeek), "semana").  
  chOpen  = phTree:Nodes:ADD(chNode:KEY, 4, "PendientesAceite", "Pendientes", "pendientes").
  chClose = phTree:Nodes:ADD(chNode:KEY, 4, "CerradasAceite", "Aprobados", "cerradas").

  /* recupero lotes creados esta semana */
  FOR EACH lotes_aceite
      WHERE lotes_aceite.anio  >= 2004
        AND (lotes_aceite.id_articulo = 51 OR lotes_aceite.id_articulo = 58)
      BY lotes_aceite.id_lote
      .

    FIND FIRST productos_terminados OF lotes_aceite NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_aceite NO-LOCK NO-ERROR.

    cKey = "loteaceite-" + STRING(lotes_aceite.nromov).
    cTxt = "Lote " + STRING(lotes_aceite.id_lote) + "/" + STRING(lotes_aceite.anio) + " " + 
           productos_terminados.descripcion + " " + 
           envases_prod.descripcion + " " + 
           "Fecha: " + STRING(lotes_aceite.fecha) + " " + STRING(lotes_aceite.nromov).
    cIco = "not".
    
    
    IF (lotes_aceite.CONTROL_calidad = FALSE ) THEN DO:
    
      cTag = STRING(lotes_aceite.id_lote) + "," + 
             STRING(lotes_aceite.anio) + "," + 
             STRING(lotes_aceite.id_tipotambor) + "," + 
             STRING(lotes_aceite.nromov).
      IF lotes_aceite.id_contrato <> "" THEN
        ASSIGN chNode = phTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "not") 
               chNode:Tag = cTag.
    END.           
    ELSE DO:
      cTag = STRING(lotes_aceite.id_lote) + "," + 
             STRING(lotes_aceite.anio) + "," + 
             STRING(lotes_aceite.id_tipotambor) + "," + 
             STRING(lotes_aceite.nromov).

      IF lotes_aceite.fecha >= dDesde  THEN 
        ASSIGN chNode = phTree:Nodes:ADD(chClose:KEY, 4, cKey, cTxt, "yes")
               chNode:Tag = cTag.
    END.
    
      
  END.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTreeWithJugos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeWithJugos Procedure 
PROCEDURE fillTreeWithJugos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iWeek AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE cFlia AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRgo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dDesde AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta AS DATE       NO-UNDO.

  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chAux   AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iSemDes = INTEGER(ENTRY(1,pcArgs)).
  iSemHas = INTEGER(ENTRY(2, pcArgs)).
  iAnio   = INTEGER(ENTRY(3, pcArgs)).
  cFlia   = ENTRY(4, pcArgs).
  iWeek   = iSemDes.


  phTree:NODES:CLEAR().

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iWeek, iAnio).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.
  
  chNode  = phTree:Nodes:ADD(, , "CtrlCalidad" + STRING(iWeek), "Control Calidad Jugos Semana "  + STRING(iWeek), "semana").  
  chOpen  = phTree:Nodes:ADD(chNode:KEY, 4, "Pendientes", "Pendientes", "pendientes").
  chClose = phTree:Nodes:ADD(chNode:KEY, 4, "Cerradas", "Aprobados", "cerradas").


  
  
  /* recupero lotes creados esta semana */
  FOR EACH lotes_jugo
      WHERE lotes_jugo.fecha >= dDesde
        AND lotes_jugo.anio  >= 2004
      BY lotes_jugo.id_lote
      .

    FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_jugo NO-LOCK NO-ERROR.

    cKey = "lote-" + STRING(lotes_jugo.nromov).
    cTxt = "Lote " + STRING(lotes_jugo.id_lote) + "/" + STRING(lotes_jugo.anio) + " " + 
           productos_terminados.descripcion + " " + 
           calidades.descripcion + " " + 
           envases_prod.descripcion + " " + 
           STRING(lotes_jugo.fecha).
    cIco = "not".
    cTag = STRING(lotes_jugo.id_empresa) + "," + 
           STRING(lotes_jugo.id_sucursal) + "," + 
           STRING(lotes_jugo.id_tipotambor) + "," + 
           STRING(lotes_jugo.nromov).

    IF (lotes_jugo.CONTROL_calidad = FALSE OR lotes_jugo.microbiologia = FALSE) THEN
      chNode = phTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "not").
    ELSE 
      chNode = phTree:Nodes:ADD(chClose:KEY, 4, cKey, cTxt, "yes").

    chNode:Tag = cTag.
  
    FIND FIRST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN DO:
      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.acidez_w_w).
      cTxt = "Acidez p/p: " + STRING(inspecciones_lote.acidez_w_w).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).
      
      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.acidez_w_v).
      cTxt = "Acidez p/v: " + STRING(inspecciones_lote.acidez_w_v).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.bx_20_20).
      cTxt = "Brix 20°: " + STRING(inspecciones_lote.bx_20_20).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.bx_correg).
      cTxt = "Brix Corregido: " + STRING(inspecciones_lote.bx_correg).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.litros).
      cTxt = "Litros: " + STRING(inspecciones_lote.litros).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.ratio).
      cTxt = "Ratio: " + STRING(inspecciones_lote.ratio).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

      cKey = "acidez" + STRING(lotes_jugo.nromov) + "-" + STRING(inspecciones_lote.id_inspeccion) + "-" + STRING(inspecciones_lote.porcentaje_pulpa).
      cTxt = "Pulpa %: " + STRING(inspecciones_lote.porcentaje_pulpa).
      cIco = "anal".
      chAux = phTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, cIco).

    END.


        
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-nodeClick) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nodeClick Procedure 
PROCEDURE nodeClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phNode AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  RUN SUPER (phNode, pcArgs).

  selectedNode = phNode.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-nodeDoubleClick) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nodeDoubleClick Procedure 
PROCEDURE nodeDoubleClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phNode AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  RUN SUPER (phNode, pcArgs).

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openAprobacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openAprobacion Procedure 
PROCEDURE openAprobacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hWin AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hPnt AS HANDLE     NO-UNDO.

  IF NOT VALID-HANDLE(selectedNode) THEN RETURN.
  IF selectedNode:TAG = "" THEN RETURN.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.

  RUN saveParamsFile IN hLib (INTEGER(ENTRY(1, selectedNode:TAG)),
                              INTEGER(ENTRY(2, selectedNode:TAG)),
                              INTEGER(ENTRY(3, selectedNode:TAG)),
                              INTEGER(ENTRY(4, selectedNode:TAG))).
  hPnt = CURRENT-WINDOW:HANDLE.

  IF ENTRY(3, selectedNode:TAG) = "3" THEN
    RUN wControlCalidadJugo.w PERSISTENT SET hWin.

  IF ENTRY(3, selectedNode:TAG) = "6" THEN
    RUN wCtrlAceite.w PERSISTENT SET hWin.

  hWin:CURRENT-WINDOW:PARENT = hPnt:PARENT.
  RUN initializeObject IN hWin .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-openLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openLote Procedure 
PROCEDURE openLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hWin AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hPnt AS HANDLE     NO-UNDO.

  IF selectedNode:TAG = "" THEN RETURN.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.
  
  RUN saveParamsFile IN hLib (INTEGER(ENTRY(1, selectedNode:TAG)),
                              INTEGER(ENTRY(2, selectedNode:TAG)),
                              INTEGER(ENTRY(3, selectedNode:TAG)),
                              INTEGER(ENTRY(4, selectedNode:TAG))).
  hPnt = CURRENT-WINDOW:HANDLE.

  IF ENTRY(3, selectedNode:TAG) = "3" THEN
    RUN wLotesJugo.w PERSISTENT SET hWin.

  IF ENTRY(3, selectedNode:TAG) = "6" THEN
    RUN wLotesAceite.w PERSISTENT SET hWin.

  hWin:CURRENT-WINDOW:PARENT = hPnt:PARENT.
  RUN initializeObject IN hWin .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getLibName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLibName Procedure 
FUNCTION getLibName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "Control Calidad".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPopupMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPopupMenu Procedure 
FUNCTION getPopupMenu RETURNS HANDLE
  (phMenu AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hMenu     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hMenuItem AS HANDLE     NO-UNDO.

  IF VALID-HANDLE(phMenu) THEN DO:
    hMenu = phMenu.
    CREATE MENU-ITEM hMenuItem
    ASSIGN PARENT   = hMenu
           SUBTYPE  = "RULE".
  END.
  ELSE DO:
    CREATE MENU hMenu
    ASSIGN POPUP-ONLY = TRUE
           TITLE      = "Popup Menu".
  END.

  /* aprobacion */
  CREATE MENU-ITEM hMenuItem
  ASSIGN PARENT      = hMenu
         NAME        = "m_Aprobar"
         LABEL       = "Aprobar Lote"       
         ACCELERATOR = "A"    
         SENSITIVE   = TRUE
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN openAprobacion IN TARGET-PROCEDURE.
  END TRIGGERS.

  


  RETURN hMenu.
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUsuario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUsuario Procedure 
FUNCTION getUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

