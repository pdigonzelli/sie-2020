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
       {"drfacturasexteriorlote.i"}.


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "drfacturasexteriorlote.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_empresa RowObject.id_sucursal ~
RowObject.id_tipotambor RowObject.nromov_lote RowObject.desde_tambor ~
RowObject.hasta_tambor 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS btnBuscarLote 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_empresa RowObject.id_sucursal ~
RowObject.id_tipotambor RowObject.nromov_lote RowObject.desde_tambor ~
RowObject.hasta_tambor 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcunidadmedida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBuscarLote 
     IMAGE-UP FILE "src/adm2/image/buscar.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar Lote" 
     SIZE 5 BY 1 TOOLTIP "Buscar Lote".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnBuscarLote AT ROW 1 COL 35
     RowObject.id_empresa AT ROW 1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_sucursal AT ROW 2.14 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_tipotambor AT ROW 3.29 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.nromov_lote AT ROW 3.29 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.desde_tambor AT ROW 5.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.hasta_tambor AT ROW 5.71 COL 51 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 63.4 BY 5.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "drfacturasexteriorlote.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {drfacturasexteriorlote.i}
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
         HEIGHT             = 5.91
         WIDTH              = 63.4.
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnBuscarLote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBuscarLote vTableWin
ON CHOOSE OF btnBuscarLote IN FRAME F-Main /* Buscar Lote */
DO:
  RUN get-lote.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_empresa vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_empresa IN FRAME F-Main /* Cod.Empresa */
DO:
  RUN get-lote.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dcunidadmedida.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcunidadmedidaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcunidadmedida ).
       RUN repositionObject IN h_dcunidadmedida ( 1.24 , 50.00 ) NO-ERROR.
       /* Size in AB:  ( 1.81 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_unidad_ventaDataSourceFilterNumRows8OptionalnoOptionalString':U + '<none>' + 'LabelUnidad VentaSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_unidad_medidaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 4.52 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 42.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dcunidadmedida , 'Data':U , h_dynselect ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.nromov_lote:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

  RUN enableNonRowObjectWidgets(FALSE).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN enableNonRowObjectWidgets(TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableNonRowObjectWidgets vTableWin 
PROCEDURE enableNonRowObjectWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plStatus AS LOGICAL    NO-UNDO.

  btnBuscarLote:SENSITIVE IN FRAME F-Main = plStatus.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-lote vTableWin 
PROCEDURE get-lote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iQty AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  
  
  RUN wGetLoteUbicacion.w (OUTPUT iEmp, 
                           OUTPUT iSuc, 
                           OUTPUT iTip, 
                           OUTPUT iNro, 
                           OUTPUT iQty, 
                           OUTPUT iDes, 
                           OUTPUT iHas).
  IF iNro <> 0 THEN DO:
    ASSIGN rowObject.id_empresa:SCREEN-VALUE IN FRAME F-Main     = STRING(iEmp)
           rowObject.id_sucursal:SCREEN-VALUE IN FRAME F-Main    = STRING(iSuc)
           rowObject.id_tipotambor:SCREEN-VALUE IN FRAME F-Main  = STRING(iTip)
           rowObject.nromov_lote:SCREEN-VALUE IN FRAME F-Main    = STRING(iNro)
           rowObject.desde_tambo:SCREEN-VALUE IN FRAME F-Main    = STRING(iDes)
           rowObject.hasta_tambor:SCREEN-VALUE IN FRAME F-Main   = STRING(iHas).
  END.
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

  RUN enableNonRowObjectWidgets(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

