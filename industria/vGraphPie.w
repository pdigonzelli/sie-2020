&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
          ventas           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dLoteUbicacion.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE chGraphPie AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chGraphBar AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE viCurrPoint AS INTEGER INITIAL -1   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dLoteUbicacion.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 11.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 1 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 133.2 BY 11.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dLoteUbicacion.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dLoteUbicacion.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 11.81
         WIDTH              = 133.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 1.24
       COLUMN          = 2
       HEIGHT          = 11.19
       WIDTH           = 63
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 1.24
       COLUMN          = 67
       HEIGHT          = 11.19
       WIDTH           = 66
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame vTableWin OCX.HotHit
PROCEDURE CtrlFrame.Graph.HotHit .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    HitSet
    HitPoint
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-HitSet   AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-HitPoint AS INTEGER NO-UNDO.

DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viSuc   AS INTEGER    NO-UNDO.
DEFINE VARIABLE vcSuc   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
DEFINE VARIABLE vcEntry AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viDat   AS INTEGER    NO-UNDO.
DEFINE VARIABLE vhLub   AS HANDLE     NO-UNDO.

  vhLub = DYNAMIC-FUNCTION('getDataSource').  

/*controlo a ver cual porcion clickeo y vuelve a juntar las porciones*/
  IF viCurrPoint <> -1 THEN 
    chGraphPie:Extra(p-HitPoint) = 0.
  
  /*Separo la porcion clickeada*/
  chGraphPie:Extra(p-HitPoint) = 1.
  
  /*refresco el grafico*/
  chGraphPie:DrawMode = 3.
  
  viCurrPoint = p-HitPoint.
  
  CASE p-HitPoint:
    WHEN 1 THEN DO:
      viSuc = 74.
      vcSuc = "Phoenix".
    END.
    WHEN 2 THEN DO:
      viSuc = 86.
      vcSuc = "Hiwa".
    END.
    WHEN 3 THEN DO:
      viSuc = 426.
      vcSuc = "Hall's".
    END.
    WHEN 4 THEN DO:
      viSuc = 73.
      vcSuc = "Chesterfield".
    END.
  END CASE.
  
  vcLista = DYNAMIC-FUNCTION('getLotesSucursal' IN vhLub, viSuc).

  /*
  vcLeg = ENTRY(1, ENTRY(1, vcLista, CHR(14))).
  MESSAGE vcLeg VIEW-AS ALERT-BOX.
  */
  chGraphBar:NumSets = 1.
  chGraphBar:NumPoints = NUM-ENTRIES(vcLista) - 1.
  chGraphBar:GraphTitle = "Lotes " + vcSuc.

  DO i = 1 TO chGraphBar:NumPoints:
    vcEntry = ENTRY(i, vcLista, CHR(14)).
    viDat   = INTEGER(ENTRY(2, vcEntry, ",")).
    chGraphBar:Label(i) = ENTRY(1, vcEntry, ",") + " (" + STRING(viDat) + ")".
    chGraphBar:Data(i)  = viDat.
  END.

  chGraphBar:VISIBLE = TRUE.
  chGraphBar:DrawMode = 2.
 

  /*
  DO i = 1 TO chGraphBar:NumPoints:
    chGraphBar:COLOR(i) = chGraphPie:COLOR(p-HitPoint).
  END.
  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
  {adm2/support/viewTrg.i}.  
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load vTableWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "vgraphpie.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "vgraphpie.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  chGraphPie = chCtrlFrame:Graph.
  chGraphBar = chCtrlFrame:Graph.
/*
  DEFINE VARIABLE viLav AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viFam AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viMer AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vhLub AS HANDLE     NO-UNDO.

  chGraphPie = chCtrlFrame:Graph.
  chGraphBar = chCtrlFrame-2:Graph.
  vhLub = DYNAMIC-FUNCTION('getDataSource').

  chGraphPie:GraphTitle = "Locacion Lotes".
  chGraphPie:Hot = 1.

  viLav = DYNAMIC-FUNCTION('getCantidadSucursal' IN vhLub, 74).
  viFam = DYNAMIC-FUNCTION('getCantidadSucursal' IN vhLub, 86).
  viMer = DYNAMIC-FUNCTION('getCantidadSucursal' IN vhLub, 426).
  viDes = DYNAMIC-FUNCTION('getCantidadSucursal' IN vhLub, 73).
  
  chGraphPie:Data(1) = viFam.
  chGraphPie:Data(2) = viLav.
  chGraphPie:Data(3) = viMer.
  chGraphPie:Data(4) = viDes.

  chGraphPie:Legend(1) = "Phoe".
  chGraphPie:Legend(2) = "Hiwa".
  chGraphPie:Legend(3) = "Hall".
  chGraphPie:Legend(4) = "Ches".

  chGraphPie:DrawMode = 3.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

