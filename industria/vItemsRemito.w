&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dItemsRemito.i"}.


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

DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
DEFINE VARIABLE rRemito AS ROWID      NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.


/*variables lote a remitir*/
DEFINE VARIABLE iEmpresa    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSucursal   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTipoTambor AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNroMov     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTamDesde   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTamHasta   AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dItemsRemito.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.nro_lote RowObject.id_tipotambor ~
RowObject.id_calidad RowObject.id_envase RowObject.id_articulo 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS btnAdd RECT-25 RECT-27 RECT-28 RECT-29 
&Scoped-Define DISPLAYED-FIELDS RowObject.nro_lote RowObject.desde_lote ~
RowObject.hasta_lote RowObject.cantidad RowObject.id_tipotambor ~
RowObject.id_calidad RowObject.id_envase RowObject.id_articulo 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS optTambor fi_nro_lote Articulo Calidad ~
Envase 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosLote vTableWin 
FUNCTION getDatosLote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "src/adm2/image/buscar.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Agregar" 
     SIZE 5.2 BY 1.19 TOOLTIP "Agregar Parte de Remito".

DEFINE VARIABLE Articulo LIKE RowObject.Articulo
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE Calidad LIKE RowObject.Calidad
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE Envase LIKE RowObject.Envase
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fi_nro_lote AS CHARACTER FORMAT "x(10)" INITIAL ? 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE optTambor AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Produccion Jugo", 1,
"Produccion Aceite", 2,
"Lotes Jugo", 3,
"Sobrante Jugo", 4,
"Arrastre Jugo", 5,
"Lotes Aceite", 6,
"Foldeado", 7,
"Sobrante Aceite", 8,
"Prod. Terceros", 9,
"Cargas", 10,
"Lotes Cascara", 11,
"Produccion Cascara", 12
     SIZE 25 BY 12.38 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.2 BY 1.38.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 12.86.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.2 BY 8.33.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.2 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnAdd AT ROW 1.1 COL 55.4
     RowObject.nro_lote AT ROW 1.19 COL 33.8 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     optTambor AT ROW 1.24 COL 2 NO-LABEL
     fi_nro_lote AT ROW 2.67 COL 40 COLON-ALIGNED HELP
          "Nro. de lote"
     Articulo AT ROW 3.76 COL 35 COLON-ALIGNED
     Calidad AT ROW 4.86 COL 35 COLON-ALIGNED
     Envase AT ROW 5.95 COL 35 COLON-ALIGNED
     RowObject.desde_lote AT ROW 7.05 COL 46.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.4 BY 1
     RowObject.hasta_lote AT ROW 8.14 COL 46.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.4 BY 1
     RowObject.cantidad AT ROW 9.24 COL 46.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.4 BY 1
     RowObject.id_tipotambor AT ROW 11.24 COL 37 COLON-ALIGNED
          LABEL "TTambor"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.id_calidad AT ROW 11.24 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.id_envase AT ROW 12.43 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.id_articulo AT ROW 12.48 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RECT-25 AT ROW 1 COL 28
     RECT-27 AT ROW 1 COL 1
     RECT-28 AT ROW 2.43 COL 28
     RECT-29 AT ROW 10.76 COL 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 60.2 BY 12.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dItemsRemito.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dItemsRemito.i}
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
         HEIGHT             = 12.86
         WIDTH              = 60.2.
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

/* SETTINGS FOR FILL-IN Articulo IN FRAME F-Main
   NO-ENABLE LIKE = Temp-Tables.RowObject. EXP-SIZE                     */
ASSIGN 
       Articulo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Calidad IN FRAME F-Main
   NO-ENABLE LIKE = Temp-Tables.RowObject. EXP-SIZE                     */
ASSIGN 
       Calidad:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.cantidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.desde_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Envase IN FRAME F-Main
   NO-ENABLE LIKE = Temp-Tables.RowObject. EXP-SIZE                     */
ASSIGN 
       Envase:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_nro_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hasta_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tipotambor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.nro_lote IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       RowObject.nro_lote:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET optTambor IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd vTableWin
ON CHOOSE OF btnAdd IN FRAME F-Main /* Agregar */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iDes  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.


  {get ContainerSource hCont}.

  iTip    = INTEGER(optTambor:SCREEN-VALUE).
  iSuc    = INTEGER(DYNAMIC-FUNCTION('getFieldRemito' IN hCont, 'id_sucursal')).
  rRemito = DYNAMIC-FUNCTION('getRowIdRemito' IN hCont).

  IF iTip = 11 THEN DO:   /*cascara*/
    PROPATH = PROPATH + ";m:;src".
    RUN wGetLoteCascaraRemito.w (iSuc, 
                                 OUTPUT iEmpresa, 
                                 OUTPUT iSucursal, 
                                 OUTPUT iTipoTambor, 
                                 OUTPUT iNroMov, 
                                 OUTPUT iCan, 
                                 OUTPUT iLot, 
                                 OUTPUT iAno).
    iDes = 1.
    iHas = iCan.
    rowObject.cantidad:SENSITIVE IN FRAME F-Main = TRUE.
    
    cReturn = STRING(iEmpresa)                + CHR(1) + 
              STRING(iSucursal)               + CHR(1) + 
              STRING(iTipotambor)             + CHR(1) + 
              STRING(iNromov)                 + CHR(1) + 
              STRING(iDes)                    + CHR(1) + 
              STRING(iHas)                    + CHR(1) + 
              STRING(iLot)                    + CHR(1) + 
              STRING(iAno)                    + CHR(1) + 
              "CASCARA DESHIDRATADA DE LIMON" + CHR(1) + 
              "CASCARA"                       + CHR(1) + 
              "BOLSA CASCARA"                 + CHR(1) + 
              STRING(54)                      + CHR(1) +
              STRING(54)                      + CHR(1) + 
              STRING(626).

  END.
  ELSE
    RUN wqOrigenTambores.w (iSuc, iTip, rRemito, "remitos", OUTPUT cReturn).


  IF cReturn <> "" THEN DO:
    /*esto es porque si selecciona al reves, el desde y hasta vienen invertidos de wqOrigenTambores.w*/
    iDes = INTEGER(ENTRY(5, cReturn, CHR(1))).
    iHas = INTEGER(ENTRY(6, cReturn, CHR(1))).
    IF iDes > iHas THEN DO:
      iDes = INTEGER(ENTRY(6, cReturn, CHR(1))).
      iHas = INTEGER(ENTRY(5, cReturn, CHR(1))).
    END.

    cLot = STRING(INTEGER(ENTRY(7,  cReturn, CHR(1))),"9999") + "/" + SUBSTRING(ENTRY(8, cReturn, CHR(1)), 3, 2).

    ASSIGN rowObject.desde_lote:SCREEN-VALUE IN FRAME F-Main  = STRING(iDes)
           rowObject.hasta_lote:SCREEN-VALUE IN FRAME F-Main  = STRING(iHas)
           articulo:SCREEN-VALUE IN FRAME F-Main              = ENTRY(9,  cReturn, CHR(1))
           calidad:SCREEN-VALUE IN FRAME F-Main               = ENTRY(10, cReturn, CHR(1))
           envase:SCREEN-VALUE IN FRAME F-Main                = ENTRY(11, cReturn, CHR(1))
           rowObject.id_articulo:SCREEN-VALUE IN FRAME F-Main = ENTRY(12, cReturn, CHR(1))
           rowObject.id_calidad:SCREEN-VALUE IN FRAME F-Main  = ENTRY(13, cReturn, CHR(1))
           rowObject.id_envase:SCREEN-VALUE IN FRAME F-Main   = ENTRY(14, cReturn, CHR(1))
           fi_nro_lote:SCREEN-VALUE IN FRAME F-Main           = ENTRY(7,  cReturn, CHR(1)) + "/" + ENTRY(8, cReturn, CHR(1))
           /*rowObject.nro_lote:SCREEN-VALUE IN FRAME F-Main    = cLot*/
           rowObject.cantidad:SCREEN-VALUE IN FRAME F-Main    = STRING(iHas - iDes + 1)
           .

    ASSIGN iEmpresa     = INTEGER(ENTRY(1, cReturn, CHR(1)))
           iSucursal    = INTEGER(ENTRY(2, cReturn, CHR(1)))
           iTipoTambor  = INTEGER(ENTRY(3, cReturn, CHR(1)))
           iNroMov      = INTEGER(ENTRY(4, cReturn, CHR(1)))
           iTamDesde    = iDes
           iTamHasta    = iHas.
  END.

  hCont:PRIVATE-DATA = getDatosLote().


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME optTambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL optTambor vTableWin
ON VALUE-CHANGED OF optTambor IN FRAME F-Main
DO:
  rowObject.id_tipotambor:SCREEN-VALUE = SELF:SCREEN-VALUE.
  
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  rowObject.id_tipotambor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = optTambor:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEnable vTableWin 
PROCEDURE customEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plState AS LOGICAL    NO-UNDO.

  optTambor:SENSITIVE IN FRAME F-Main = plState.
  btnAdd:SENSITIVE IN FRAME F-Main    = plState.
  optTambor:SCREEN-VALUE              = IF rowObject.id_tipotambor:SCREEN-VALUE <> "0" THEN rowObject.id_tipotambor:SCREEN-VALUE ELSE "1".
  rowObject.cantidad:SENSITIVE IN FRAME F-Main = plState.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableFields vTableWin 
PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcFieldType AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcFieldType).

  RUN customEnable(FALSE).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  DEFINE VARIABLE iLoop          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iNumEntries    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iDes           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cFieldHandles  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLot           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hTableioSource AS HANDLE    NO-UNDO.
  DEFINE VARIABLE hLib           AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.
  

  ASSIGN cFieldHandles  = DYNAMIC-FUNCTION('getAllFieldHandles')
         iNumEntries    = NUM-ENTRIES(cFieldHandles)
         hTableioSource = DYNAMIC-FUNCTION('getTableIOSource').
  
  DO iLoop = 1 TO iNumEntries:
    ASSIGN hField = WIDGET-HANDLE(ENTRY(iLoop,cFieldHandles)).
    
    IF hField:NAME = "nro_lote" THEN DO:
      cLot = STRING(hField:SCREEN-VALUE, "x(10)").
      fi_nro_lote:SCREEN-VALUE IN FRAME F-Main = cLot.
    END.

    IF hField:NAME = "id_articulo" THEN 
      articulo:SCREEN-VALUE IN FRAME f-Main = DYNAMIC-FUNCTION('getDescArticulo' IN hLib, INTEGER(hField:SCREEN-VALUE)).     
    
    IF hField:NAME = "id_calidad" THEN 
      calidad:SCREEN-VALUE IN FRAME f-Main = DYNAMIC-FUNCTION('getDescCalidad' IN hLib, INTEGER(hField:SCREEN-VALUE)).     

    IF hField:NAME = "id_envase" THEN 
      envase:SCREEN-VALUE IN FRAME f-Main = DYNAMIC-FUNCTION('getDescEnvase' IN hLib, INTEGER(hField:SCREEN-VALUE)).     
    
    IF hField:NAME = "id_tipotambor" AND INTEGER(hField:SCREEN-VALUE) <> 0 THEN 
      optTambor:SCREEN-VALUE IN FRAM F-Main = hField:SCREEN-VALUE.



  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN customEnable(TRUE).

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

  RUN customEnable (FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDatosLoteContainer vTableWin 
PROCEDURE setDatosLoteContainer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.


  {get ContainerSource hCont}.

  IF hCont:FILE-NAME MATCHES "*wRemitos.w" THEN DO:
    hCont:PRIVATE-DATA = getDatosLote().
  END.

  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChanged vTableWin 
PROCEDURE valueChanged :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  optTambor:SCREEN-VALUE IN FRAME F-Main = rowObject.id_tipotambor:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosLote vTableWin 
FUNCTION getDatosLote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = STRING(iEmpresa)     + CHR(1) + 
         STRING(iSucursal)    + CHR(1) + 
         STRING(iTipoTambor)  + CHR(1) + 
         STRING(iNroMov)      + CHR(1) + 
         STRING(iTamDesde)    + CHR(1) + 
         STRING(iTamHasta).
  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

