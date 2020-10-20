&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCabeceraLote NO-UNDO LIKE cabeceralote.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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
DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.



{adm2/support/customColors.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF

&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCabeceraLote

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio articulo calidad cantidad envase fecha id_articulo id_calidad~
 id_empresa id_envase id_lote id_sucursal id_sucursal_ubicacion~
 id_tambor_desde id_tambor_hasta id_tipotambor kilos kilos400 nromov~
 tipo_tambor ubicacion anio_contrato id_contrato id_tipo_contrato item_of~
 kilos_tambor tara_tambor item_oe id_orden_entrega aux
&Scoped-define ENABLED-FIELDS-IN-ttCabeceraLote anio articulo calidad ~
cantidad envase fecha id_articulo id_calidad id_empresa id_envase id_lote ~
id_sucursal id_sucursal_ubicacion id_tambor_desde id_tambor_hasta ~
id_tipotambor kilos kilos400 nromov tipo_tambor ubicacion anio_contrato ~
id_contrato id_tipo_contrato item_of kilos_tambor tara_tambor item_oe ~
id_orden_entrega aux 
&Scoped-Define DATA-FIELDS  anio articulo calidad cantidad envase fecha id_articulo id_calidad~
 id_empresa id_envase id_lote id_sucursal id_sucursal_ubicacion~
 id_tambor_desde id_tambor_hasta id_tipotambor kilos kilos400 nromov~
 tipo_tambor ubicacion anio_contrato id_contrato id_tipo_contrato item_of~
 kilos_tambor tara_tambor item_oe id_orden_entrega aux
&Scoped-define DATA-FIELDS-IN-ttCabeceraLote anio articulo calidad cantidad ~
envase fecha id_articulo id_calidad id_empresa id_envase id_lote ~
id_sucursal id_sucursal_ubicacion id_tambor_desde id_tambor_hasta ~
id_tipotambor kilos kilos400 nromov tipo_tambor ubicacion anio_contrato ~
id_contrato id_tipo_contrato item_of kilos_tambor tara_tambor item_oe ~
id_orden_entrega aux 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttCabeceraLote.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttCabeceraLote NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttCabeceraLote NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttCabeceraLote
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttCabeceraLote


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRows dTables  _DB-REQUIRED
FUNCTION getSelectedRows RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttCabeceraLote SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
   Temp-Tables and Buffers:
      TABLE: ttCabeceraLote T "?" NO-UNDO tablastemp cabeceralote
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "Temp-Tables.ttCabeceraLote"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttCabeceraLote.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
     _FldNameList[2]   > Temp-Tables.ttCabeceraLote.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[3]   > Temp-Tables.ttCabeceraLote.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[4]   > Temp-Tables.ttCabeceraLote.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[5]   > Temp-Tables.ttCabeceraLote.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[6]   > Temp-Tables.ttCabeceraLote.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[7]   > Temp-Tables.ttCabeceraLote.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[8]   > Temp-Tables.ttCabeceraLote.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[9]   > Temp-Tables.ttCabeceraLote.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[10]   > Temp-Tables.ttCabeceraLote.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[11]   > Temp-Tables.ttCabeceraLote.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[12]   > Temp-Tables.ttCabeceraLote.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[13]   > Temp-Tables.ttCabeceraLote.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[14]   > Temp-Tables.ttCabeceraLote.id_tambor_desde
"id_tambor_desde" "id_tambor_desde" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[15]   > Temp-Tables.ttCabeceraLote.id_tambor_hasta
"id_tambor_hasta" "id_tambor_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[16]   > Temp-Tables.ttCabeceraLote.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[17]   > Temp-Tables.ttCabeceraLote.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[18]   > Temp-Tables.ttCabeceraLote.kilos400
"kilos400" "kilos400" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[19]   > Temp-Tables.ttCabeceraLote.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[20]   > Temp-Tables.ttCabeceraLote.tipo_tambor
"tipo_tambor" "tipo_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[21]   > Temp-Tables.ttCabeceraLote.ubicacion
"ubicacion" "ubicacion" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[22]   > Temp-Tables.ttCabeceraLote.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[23]   > Temp-Tables.ttCabeceraLote.id_contrato
"id_contrato" "id_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 no
     _FldNameList[24]   > Temp-Tables.ttCabeceraLote.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 15.2 no
     _FldNameList[25]   > Temp-Tables.ttCabeceraLote.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[26]   > Temp-Tables.ttCabeceraLote.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[27]   > Temp-Tables.ttCabeceraLote.tara_tambor
"tara_tambor" "tara_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 no
     _FldNameList[28]   > Temp-Tables.ttCabeceraLote.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[29]   > Temp-Tables.ttCabeceraLote.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 16.6 no
     _FldNameList[30]   > Temp-Tables.ttCabeceraLote.aux
"aux" "aux" ? ? "character" ? ? ? ? ? ? yes ? no 200 no
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endTransactionValidate dTables  _DB-REQUIRED
PROCEDURE endTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPosUpdate dTables  _DB-REQUIRED
PROCEDURE especialPosUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER  xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreCreate dTables  _DB-REQUIRED
PROCEDURE especialPreCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xCase AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreUpdate dTables  _DB-REQUIRED
PROCEDURE especialPreUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fieldsWithProblems dTables  _DB-REQUIRED
PROCEDURE fieldsWithProblems :
/*------------------------------------------------------------------------------
  Purpose: In this procedure we check all conditions of BUssines Logic that the
  RowObject can give us.Returns a comma separated list in wich each element is a
  pair of field + chr(3)+  color we want to mark as irregular ones
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cList AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTamboresContrato dTables  _DB-REQUIRED
PROCEDURE fillTamboresContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcCon AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAno AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE f400 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH ttCabeceraLote.
    DELETE ttCabeceraLote.
  END.

  IF pcCon = "" THEN RETURN.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_contrato_of     = pcCon
        AND tambores_industria.id_tipocontrato_of = piTip
        AND tambores_industria.anio_of            = piAno
        AND (IF piPte <> 0 THEN tambores_industria.ITEM_of = piPte ELSE TRUE)
      BREAK BY tambores_industria.nromov
      BY tambores_industria.id_tambor.

    ASSIGN iCan = iCan + 1
           fKil = fKil + tambores_industria.kilos_tambor.
    
    IF FIRST-OF(tambores_industria.nromov) THEN iDes = tambores_industria.id_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.

      CREATE ttCabeceraLote.
      BUFFER-COPY tambores_industria TO ttCabeceraLote.

      f400 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor,
                                                     tambores_industria.id_articulo,
                                                     tambores_industria.id_calidad,
                                                     fKil).

      ASSIGN  ttCabeceraLote.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttCabeceraLote.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
              ttCabeceraLote.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
              ttCabeceraLote.ubicacion        = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE" 
              ttCabeceraLote.id_tambor_desde  = iDes
              ttCabeceraLote.id_tambor_hasta  = tambores_industria.id_tambor
              ttCabeceraLote.cantidad         = iCan
              ttCabeceraLote.kilos            = fKil
              ttCabeceraLote.kilos400         = f400
              ttCabeceraLote.id_contrato      = tambores_industria.id_contrato_of

              iCan                            = 0
              fKil                            = 0
              f400                            = 0
              .
    END.

  END.

  openQuery().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTamboresDispo dTables  _DB-REQUIRED
PROCEDURE fillTamboresDispo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE f400 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH ttCabeceraLote.
    DELETE ttCabeceraLote.
  END.

  IF iArt = 0 THEN RETURN .


  FOR EACH tambores_industria
      WHERE tambores_industria.fecha                >= DATE("01/01/2006")
        AND tambores_industria.id_contrato           = ''
        AND tambores_industria.id_locacion_ubicacion = 4
        AND (IF iArt <> 0 THEN tambores_industria.id_articulo = iArt ELSE TRUE)
        AND (IF iCal <> 0 THEN tambores_industria.id_calidad  = iCal ELSE TRUE)
        /*AND (IF iEnv <> 0 THEN tambores_industria.id_envase   = iEnv ELSE TRUE)
        AND (IF iSuc <> 0 THEN tambores_industria.id_sucursal_ubicacion = iSuc ELSE TRUE) */
      BREAK BY tambores_industria.nromov
      BY tambores_industria.id_tambor.

    ASSIGN iCan = iCan + 1
           fKil = fKil + tambores_industria.kilos_tambor.
    
    IF FIRST-OF(tambores_industria.nromov) THEN iDes = tambores_industria.id_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.

      CREATE ttCabeceraLote.
      BUFFER-COPY tambores_industria TO ttCabeceraLote.

      f400 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor,
                                                     tambores_industria.id_articulo,
                                                     tambores_industria.id_calidad,
                                                     fKil).

      ASSIGN  ttCabeceraLote.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttCabeceraLote.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
              ttCabeceraLote.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
              ttCabeceraLote.ubicacion        = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE" 
              ttCabeceraLote.id_tambor_desde  = iDes
              ttCabeceraLote.id_tambor_hasta  = tambores_industria.id_tambor
              ttCabeceraLote.cantidad         = iCan
              ttCabeceraLote.kilos            = fKil
              ttCabeceraLote.kilos400         = f400
              ttCabeceraLote.id_contrato      = tambores_industria.id_contrato_of

              iCan                            = 0
              fKil                            = 0
              f400                            = 0
              .
    END.

  END.

  openQuery().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTamboresOE dTables  _DB-REQUIRED
PROCEDURE fillTamboresOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPt AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE f400 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH ttCabeceraLote.
    DELETE ttCabeceraLote.
  END.

  IF piOE = 0 THEN RETURN.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega  = piOE
        AND tambores_industria.ITEM_oe           = piPt
      BREAK BY tambores_industria.nromov
      BY tambores_industria.id_tambor.

    ASSIGN iCan = iCan + 1
           fKil = fKil + tambores_industria.kilos_tambor.
    
    IF FIRST-OF(tambores_industria.nromov) THEN iDes = tambores_industria.id_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.

      CREATE ttCabeceraLote.
      BUFFER-COPY tambores_industria TO ttCabeceraLote.

      f400 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor,
                                                     tambores_industria.id_articulo,
                                                     tambores_industria.id_calidad,
                                                     fKil).

      ASSIGN  ttCabeceraLote.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttCabeceraLote.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
              ttCabeceraLote.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
              ttCabeceraLote.ubicacion        = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE" 
              ttCabeceraLote.id_tambor_desde  = iDes
              ttCabeceraLote.id_tambor_hasta  = tambores_industria.id_tambor
              ttCabeceraLote.cantidad         = iCan
              ttCabeceraLote.kilos            = fKil
              ttCabeceraLote.kilos400         = f400
              ttCabeceraLote.id_contrato      = tambores_industria.id_contrato_of
              ttCabeceraLote.ITEM_of          = tambores_industria.ITEM_of
              ttCabeceraLote.ITEM_oe          = tambores_industria.ITEM_oe

              iCan                            = 0
              fKil                            = 0
              f400                            = 0
              .
    END.

  END.

  openQuery().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTbsPickUp dTables  _DB-REQUIRED
PROCEDURE fillTbsPickUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piLote   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArtic  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTamDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTamHas AS INTEGER    NO-UNDO.

  
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE f400 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH ttCabeceraLote.
    DELETE ttCabeceraLote.
  END.

  IF piArtic = 581 THEN 
    RUN fillWithWaterPhase (piSucUbi, piTipTam, piSucPro, piLote, piAnio, piArtic, piTamDes, piTamHas).
  ELSE DO:
  
    FOR EACH tambores_industria
        WHERE tambores_industria.fecha                >= DATE("01/01/2005")
          AND tambores_industria.id_locacion_ubicacion = 4
          AND tambores_industria.id_sucursal_ubicacion = piSucUbi
          
          AND (IF piTipTam <> 0 THEN tambores_industria.id_tipotambor = piTipTam ELSE TRUE)
          AND (IF piSucPro <> 0 THEN tambores_industria.id_sucursal = piSucPro ELSE TRUE)
          AND (IF piLote <> 0 THEN tambores_industria.id_lote = piLote ELSE TRUE)
          AND (IF piAnio <> 0 THEN tambores_industria.anio = piAnio ELSE TRUE)
          AND (IF piArtic <> 0 THEN tambores_industria.id_articulo = piArtic ELSE TRUE)
          AND (IF piTamDes <> 0 THEN tambores_industria.id_tambor >= piTamDes ELSE TRUE)
          AND (IF piTamHas <> 0 THEN tambores_industria.id_tambor <= piTamHas ELSE TRUE)
        BREAK BY tambores_industria.nromov
        BY tambores_industria.id_tambor.
  
      ASSIGN iCan = iCan + 1
             fKil = fKil + tambores_industria.kilos_tambor.
      
      IF FIRST-OF(tambores_industria.nromov) THEN iDes = tambores_industria.id_tambor.
  
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.
  
        CREATE ttCabeceraLote.
        BUFFER-COPY tambores_industria TO ttCabeceraLote.
  
        f400 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor,
                                                       tambores_industria.id_articulo,
                                                       tambores_industria.id_calidad,
                                                       fKil).
  
        ASSIGN  ttCabeceraLote.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                ttCabeceraLote.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                ttCabeceraLote.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
                ttCabeceraLote.ubicacion        = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE" 
                ttCabeceraLote.id_tambor_desde  = iDes
                ttCabeceraLote.id_tambor_hasta  = tambores_industria.id_tambor
                ttCabeceraLote.cantidad         = iCan
                ttCabeceraLote.kilos            = fKil
                ttCabeceraLote.kilos400         = f400
                ttCabeceraLote.kilos_tambor     = tambores_industria.kilos_tambor
                ttCabeceraLote.tara_tambor      = tambores_industria.tara
                ttCabeceraLote.id_contrato      = tambores_industria.id_contrato_of
  
                iCan                            = 0
                fKil                            = 0
                f400                            = 0
                .
      END.
  
    END.

  END.

  openQuery().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillWithWaterPhase dTables  _DB-REQUIRED
PROCEDURE fillWithWaterPhase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piLote   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArtic  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTamDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTamHas AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE f400 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH tambores_industria
      WHERE tambores_industria.fecha                >= DATE("01/01/2005")
        AND tambores_industria.id_locacion_ubicacion = 4
        AND tambores_industria.id_sucursal_ubicacion = piSucUbi
        
        AND (IF piTipTam <> 0 THEN tambores_industria.id_tipotambor = piTipTam ELSE TRUE)
        AND (IF piSucPro <> 0 THEN tambores_industria.id_sucursal = piSucPro ELSE TRUE)
        AND (IF piLote <> 0 THEN tambores_industria.id_lote = piLote ELSE TRUE)
        AND (IF piAnio <> 0 THEN tambores_industria.anio = piAnio ELSE TRUE)
        AND (IF piArtic <> 0 THEN tambores_industria.id_articulo = piArtic ELSE TRUE)
        AND (IF piTamDes <> 0 THEN tambores_industria.id_tambor >= piTamDes ELSE TRUE)
        AND (IF piTamHas <> 0 THEN tambores_industria.id_tambor <= piTamHas ELSE TRUE)
      BREAK BY tambores_industria.id_articulo
      BY tambores_industria.id_tambor.

    ASSIGN iCan = iCan + 1
           fKil = fKil + tambores_industria.kilos_tambor
           cAux = cAux + STRING(tambores_industria.nromov) + ","
           .
    
    IF FIRST-OF(tambores_industria.id_articulo) THEN iDes = tambores_industria.id_tambor.

    IF LAST-OF(tambores_industria.id_articulo) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.

      CREATE ttCabeceraLote.
      BUFFER-COPY tambores_industria TO ttCabeceraLote.

      f400 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor,
                                                     tambores_industria.id_articulo,
                                                     tambores_industria.id_calidad,
                                                     fKil).

      ASSIGN  ttCabeceraLote.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttCabeceraLote.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
              ttCabeceraLote.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
              ttCabeceraLote.ubicacion        = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE" 
              ttCabeceraLote.id_tambor_desde  = iDes
              ttCabeceraLote.id_tambor_hasta  = tambores_industria.id_tambor
              ttCabeceraLote.cantidad         = iCan
              ttCabeceraLote.kilos            = fKil
              ttCabeceraLote.kilos400         = f400
              ttCabeceraLote.kilos_tambor     = tambores_industria.kilos_tambor
              ttCabeceraLote.tara_tambor      = tambores_industria.tara
              ttCabeceraLote.id_contrato      = tambores_industria.id_contrato_of
              ttCabeceraLote.aux              = cAux

              iCan                            = 0
              fKil                            = 0
              f400                            = 0
              cAux                            = ""
              .
    END.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getField dTables  _DB-REQUIRED
PROCEDURE getField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xFieldName AS CHARACTER NO-UNDO.

DEFINE VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR hField  AS HANDLE NO-UNDO.

hBuffer = BUFFER RowObject:HANDLE.
IF VALID-HANDLE(hBuffer) THEN
DO:
    hField = hBuffer:BUFFER-FIELD(xFieldName).
    IF VALID-HANDLE(hField) THEN
    DO:
        RETURN hField:BUFFER-VALUE().
    END.
END.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/



  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  {get DataSource xDataSource}.
  IF xDataSource <> ? THEN
  DO:
      {set AutoCommit NO}.
      {set CommitSource xDataSource}.
  END.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postTransactionValidate dTables  _DB-REQUIRED
PROCEDURE postTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postUpdate dTables  _DB-REQUIRED
PROCEDURE postUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer   AS HANDLE NO-UNDO.
DEFINE VAR hField                AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preCreate dTables  _DB-REQUIRED
PROCEDURE preCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preUpdate dTables  _DB-REQUIRED
PROCEDURE preUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLocalVars dTables  _DB-REQUIRED
PROCEDURE setLocalVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnv AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.

  ASSIGN  iArt = piArt
          iCal = piCal
          iEnv = piEnv
          iSuc = piSuc
          .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRows dTables  _DB-REQUIRED
FUNCTION getSelectedRows RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH rowObject.
    cRet = cRet + 
           STRING(rowObject.id_empresa) + CHR(1) + 
           STRING(rowObject.id_sucursal) + CHR(1) + 
           STRING(rowObject.id_tipotambor) + CHR(1) + 
           STRING(rowObject.nromov) + CHR(1) +
           STRING(rowObject.id_tambor_desde) + CHR(1) + 
           STRING(rowObject.id_tambor_hasta) + CHR(1) + 
           STRING(rowObject.id_articulo) + CHR(1) + 
           STRING(rowObject.id_calidad) + CHR(1) + 
           STRING(rowObject.id_envase) + CHR(1) + 
           STRING(rowObject.kilos) + CHR(1) + 
           STRING(rowObject.tara) + CHR(1) + 
           STRING(rowObject.id_lote) + CHR(1) + 
           STRING(rowObject.anio) + CHR(1) + 
           STRING(rowObject.aux) + CHR(1) + 
           STRING(rowObject.id_sucursal_ubicacion) + CHR(1)
           .
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  


  RETURN SUPER( ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

