&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          prog2sap         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dSapClientes.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dSapClientes.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ANRED RowObject.TELF1 ~
RowObject.NAME1 RowObject.TELFX RowObject.NAME2 RowObject.LIFNR ~
RowObject.NAME3 RowObject.STCD1 RowObject.NAME4 RowObject.KNA1-STKZN ~
RowObject.SORT1 RowObject.BRSCH RowObject.SORT2 RowObject.KUKLA ~
RowObject.STRAS RowObject.LZONE RowObject.ORT01 RowObject.FITYP ~
RowObject.PSTLZ RowObject.STCDT RowObject.LAND1 RowObject.REGIO 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.ANRED RowObject.TELF1 ~
RowObject.NAME1 RowObject.TELFX RowObject.NAME2 RowObject.LIFNR ~
RowObject.NAME3 RowObject.STCD1 RowObject.NAME4 RowObject.KNA1-STKZN ~
RowObject.SORT1 RowObject.BRSCH RowObject.SORT2 RowObject.KUKLA ~
RowObject.STRAS RowObject.LZONE RowObject.ORT01 RowObject.FITYP ~
RowObject.PSTLZ RowObject.STCDT RowObject.LAND1 RowObject.REGIO 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.ANRED AT ROW 1.24 COL 3.2
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.TELF1 AT ROW 1.24 COL 60.4
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.NAME1 AT ROW 2.43 COL 3.4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.TELFX AT ROW 2.43 COL 60.2
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.NAME2 AT ROW 3.62 COL 3.4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.LIFNR AT ROW 3.62 COL 60.6
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.NAME3 AT ROW 4.81 COL 3.4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.STCD1 AT ROW 4.81 COL 59.8
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.NAME4 AT ROW 6 COL 3.4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.KNA1-STKZN AT ROW 6 COL 53.4
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.SORT1 AT ROW 7.19 COL 3.6
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.BRSCH AT ROW 7.19 COL 59.4
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.SORT2 AT ROW 8.38 COL 3.6
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.KUKLA AT ROW 8.38 COL 59.8
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.STRAS AT ROW 9.57 COL 3.6
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.LZONE AT ROW 9.57 COL 59.6
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.ORT01 AT ROW 10.76 COL 3.8
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.FITYP AT ROW 10.76 COL 60.8
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.PSTLZ AT ROW 11.95 COL 4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.STCDT AT ROW 11.95 COL 59.6
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.LAND1 AT ROW 13.14 COL 3.8
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     RowObject.REGIO AT ROW 14.33 COL 4
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 108.4 BY 14.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dSapClientes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dSapClientes.i}
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
         HEIGHT             = 14.52
         WIDTH              = 108.4.
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

/* SETTINGS FOR FILL-IN RowObject.ANRED IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.BRSCH IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.FITYP IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.KNA1-STKZN IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.KUKLA IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.LAND1 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.LIFNR IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.LZONE IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.NAME1 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.NAME2 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.NAME3 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.NAME4 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.ORT01 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.PSTLZ IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.REGIO IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.SORT1 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.SORT2 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.STCD1 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.STCDT IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.STRAS IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.TELF1 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.TELFX IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


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

