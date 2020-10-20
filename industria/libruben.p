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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

RUN libLogistica.p PERSISTENT SET hLib.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib, SEARCH-SELF).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE pchTree AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOE    AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE piWeek  AS INTEGER    NO-UNDO.

  
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  piWeek  = INTEGER(pcArgs).
  pchTree = phTree.


  pchTree:NODES:CLEAR().

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, piWeek, YEAR(TODAY)).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.

  
  chNode  = pchTree:Nodes:ADD(, , "Semana" + STRING(piWeek), "Semana "  + STRING(piWeek), "semana").  
  chOpen  = pchTree:Nodes:ADD(chNode:KEY, 4, "Pendientes", "Pendientes", "pendientes").
  chClose = pchTree:Nodes:ADD(chNode:KEY, 4, "Cerradas", "Embarques", "cerradas").

  chNode:TAG = STRING(piWeek).


  FOR EACH items_contratos
      WHERE items_contratos.semana_entrega = piWeek
        AND items_contratos.anio           = 2006 /*YEAR(dDesde)*/
        AND (items_contratos.id_articulo <> 51 OR items_contratos.id_articulo <> 58)
      NO-LOCK.

    FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
        
    cKey = "parte" + items_contratos.id_contrato + "-" + STRING(items_contratos.ITEM).
    cTxt = items_contratos.id_contrato + " Parte: " + STRING(items_contratos.ITEM) + 
           " Cliente: " + clientes.razon_social + 
           " Producto: " + productos_terminados.descripcion.

    IF items_contratos.pendiente THEN DO:      
      chNode = pchTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "contrato").
    END.

  END. /*items_contratos*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

