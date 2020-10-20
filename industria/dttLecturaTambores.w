&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttLecturaTambores NO-UNDO LIKE lecturatambores.


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
&Scoped-define INTERNAL-TABLES ttLecturaTambores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  acidez_w_w anio brix_20_20 fecha id_articulo id_calidad id_camara~
 id_empresa id_empresa_destino id_envase id_lote id_sucursal~
 id_sucursal_camara id_sucursal_destino id_sucursal_remito~
 id_sucursal_ubicacion id_tambor id_tipotambor id_tipotambor_destino~
 id_tipo_movsto item_factura kilos nromov nromov_destino nro_columna_camara~
 nro_fila_camara nro_remito
&Scoped-define ENABLED-FIELDS-IN-ttLecturaTambores acidez_w_w anio ~
brix_20_20 fecha id_articulo id_calidad id_camara id_empresa ~
id_empresa_destino id_envase id_lote id_sucursal id_sucursal_camara ~
id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion id_tambor ~
id_tipotambor id_tipotambor_destino id_tipo_movsto item_factura kilos ~
nromov nromov_destino nro_columna_camara nro_fila_camara nro_remito 
&Scoped-Define DATA-FIELDS  acidez_w_w anio brix_20_20 fecha id_articulo id_calidad id_camara~
 id_empresa id_empresa_destino id_envase id_lote id_sucursal~
 id_sucursal_camara id_sucursal_destino id_sucursal_remito~
 id_sucursal_ubicacion id_tambor id_tipotambor id_tipotambor_destino~
 id_tipo_movsto item_factura kilos nromov nromov_destino nro_columna_camara~
 nro_fila_camara nro_remito Articulo Calidad Envase
&Scoped-define DATA-FIELDS-IN-ttLecturaTambores acidez_w_w anio brix_20_20 ~
fecha id_articulo id_calidad id_camara id_empresa id_empresa_destino ~
id_envase id_lote id_sucursal id_sucursal_camara id_sucursal_destino ~
id_sucursal_remito id_sucursal_ubicacion id_tambor id_tipotambor ~
id_tipotambor_destino id_tipo_movsto item_factura kilos nromov ~
nromov_destino nro_columna_camara nro_fila_camara nro_remito 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttlecturatambores.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttLecturaTambores NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttLecturaTambores NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttLecturaTambores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttLecturaTambores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExisteTambor dTables  _DB-REQUIRED
FUNCTION getExisteTambor RETURNS LOGICAL
  (piNro AS INTEGER,
   piTam AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastRowId dTables  _DB-REQUIRED
FUNCTION getLastRowId RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttLecturaTambores SCROLLING.
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
      TABLE: ttLecturaTambores T "?" NO-UNDO tablastemp lecturatambores
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
     _TblList          = "Temp-Tables.ttLecturaTambores"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttLecturaTambores.acidez_w_w
"acidez_w_w" "acidez_w_w" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[2]   > Temp-Tables.ttLecturaTambores.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
     _FldNameList[3]   > Temp-Tables.ttLecturaTambores.brix_20_20
"brix_20_20" "brix_20_20" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[4]   > Temp-Tables.ttLecturaTambores.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[5]   > Temp-Tables.ttLecturaTambores.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[6]   > Temp-Tables.ttLecturaTambores.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[7]   > Temp-Tables.ttLecturaTambores.id_camara
"id_camara" "id_camara" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[8]   > Temp-Tables.ttLecturaTambores.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[9]   > Temp-Tables.ttLecturaTambores.id_empresa_destino
"id_empresa_destino" "id_empresa_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 18.8 no
     _FldNameList[10]   > Temp-Tables.ttLecturaTambores.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[11]   > Temp-Tables.ttLecturaTambores.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[12]   > Temp-Tables.ttLecturaTambores.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[13]   > Temp-Tables.ttLecturaTambores.id_sucursal_camara
"id_sucursal_camara" "id_sucursal_camara" ? ? "integer" ? ? ? ? ? ? yes ? no 18.8 no
     _FldNameList[14]   > Temp-Tables.ttLecturaTambores.id_sucursal_destino
"id_sucursal_destino" "id_sucursal_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 18.6 no
     _FldNameList[15]   > Temp-Tables.ttLecturaTambores.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 no
     _FldNameList[16]   > Temp-Tables.ttLecturaTambores.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[17]   > Temp-Tables.ttLecturaTambores.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[18]   > Temp-Tables.ttLecturaTambores.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[19]   > Temp-Tables.ttLecturaTambores.id_tipotambor_destino
"id_tipotambor_destino" "id_tipotambor_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 20.6 no
     _FldNameList[20]   > Temp-Tables.ttLecturaTambores.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 14.2 no
     _FldNameList[21]   > Temp-Tables.ttLecturaTambores.item_factura
"item_factura" "item_factura" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[22]   > Temp-Tables.ttLecturaTambores.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[23]   > Temp-Tables.ttLecturaTambores.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[24]   > Temp-Tables.ttLecturaTambores.nromov_destino
"nromov_destino" "nromov_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[25]   > Temp-Tables.ttLecturaTambores.nro_columna_camara
"nro_columna_camara" "nro_columna_camara" ? ? "character" ? ? ? ? ? ? yes ? no 20.4 no
     _FldNameList[26]   > Temp-Tables.ttLecturaTambores.nro_fila_camara
"nro_fila_camara" "nro_fila_camara" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[27]   > Temp-Tables.ttLecturaTambores.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[28]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[29]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[30]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTambor dTables  _DB-REQUIRED
PROCEDURE addTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEtq AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcAct AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcArg AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lCtr    AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  
 
  
  FOR FIRST tambores_industria WHERE tambores_industria.id_etiqueta = piEtq
                               NO-LOCK.

    lCtr = DYNAMIC-FUNCTION('getControlTamborLeido' IN hLibTam, tambores_industria.id_empresa,
                                                                tambores_industria.id_sucursal, 
                                                                tambores_industria.id_tipotambor,
                                                                tambores_industria.nromov, 
                                                                pcAct, 
                                                                pcArg).
    IF NOT lCtr THEN
      RETURN.

    IF getExisteTambor(tambores_industria.nromov, tambores_industria.id_tambor)  THEN
      RETURN.

    cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa,
                                                          tambores_industria.id_sucursal,
                                                          tambores_industria.id_tipotambor,
                                                          tambores_industria.nromov).
    
    CREATE ttLecturaTambores.
    ASSIGN ttLecturaTambores.id_empresa             = tambores_industria.id_empresa
           ttLecturaTambores.id_sucursal            = tambores_industria.id_sucursal
           ttLecturaTambores.id_tipotambor          = tambores_industria.id_tipotambor
           ttLecturaTambores.nromov                 = tambores_industria.nromov
           ttLecturaTambores.id_tambor              = tambores_industria.id_tambor
           ttLecturaTambores.id_articulo            = tambores_industria.id_articulo
           ttLecturaTambores.id_calidad             = tambores_industria.id_calidad
           ttLecturaTambores.id_envase              = tambores_industria.id_envase
           ttLecturaTambores.id_sucursal_ubicacion  = tambores_industria.id_sucursal_ubicacion
           ttLecturaTambores.id_lote                = tambores_industria.id_lote
           ttLecturaTambores.anio                   = tambores_industria.anio
           ttLecturaTambores.kilos                  = tambores_industria.kilos_tambor
           ttLecturaTambores.id_sucursal_remito     = tambores_industria.id_sucursal_remito
           ttLecturaTambores.id_tipo_movsto         = tambores_industria.id_tipo_movsto
           ttLecturaTambores.nro_remito             = tambores_industria.nro_remito
           ttLecturaTambores.ITEM_factura           = tambores_industria.ITEM_factura
           ttLecturaTambores.id_sucursal_camara     = tambores_industria.id_sucursal_camara
           ttLecturaTambores.id_camara              = tambores_industria.id_camara
           ttLecturaTambores.nro_fila_camara        = tambores_industria.nro_fila_camara
           ttLecturaTambores.nro_columna_camara     = tambores_industria.nro_columna_camara
           ttLecturaTambores.brix_20_20             = IF cAnl <> "" AND cAnl <> ? THEN DECIMAL(ENTRY(2, cAnl, CHR(1))) ELSE 0.00
           ttLecturaTambores.acidez_w_w             = IF cAnl <> "" AND cAnl <> ? THEN DECIMAL(ENTRY(3, cAnl, CHR(1))) ELSE 0.00
           .    
    openQuery().

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aux dTables  _DB-REQUIRED
PROCEDURE aux :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPte AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRgo AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buRo FOR rowObject.
  DEFINE BUFFER buRead FOR rowObject.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
  

  /*determino una lista entry con los rangos*/
  FOR EACH buRo 
      BREAK BY buRo.nromov 
            BY buRo.id_tambor.
    
    IF FIRST-OF(buRo.nromov) THEN
      ASSIGN i    = buRo.id_tambor
             j    = buRo.id_tambor
             dKil = 0.

    IF buRo.id_tambor = j THEN 
      ASSIGN j    = j + 1
             dKil = dKil + buRo.kilos
             iCan = iCan + 1
             .
    ELSE DO:
      ASSIGN cRgo = cRgo + STRING(buRo.nromov)        + CHR(1) + 
                           STRING(i)                  + CHR(1) + 
                           STRING(j - 1)              + CHR(1) + 
                           STRING(buRo.id_tipotambor) + CHR(1) + 
                           STRING(buRo.id_articulo)   + CHR(1) +
                           STRING(buRo.id_calidad)    + CHR(1) + 
                           STRING(buRo.id_envase)     + CHR(1) + 
                           STRING(buRo.id_lote)       + CHR(1) + 
                           STRING(buRo.anio)          + CHR(1) + 
                           STRING(buRo.id_empresa)    + CHR(1) +
                           STRING(buRo.id_sucursal)   + CHR(1) + 
                           STRING(buRo.id_tipotambor) + CHR(1) + 
                           STRING(dKil)               + CHR(1) + 
                           STRING(iCan)               + CHR(10)
             i    = buRo.id_tambor
             j    = buRo.id_tambor + 1
             dKil = buRo.kilos
             iCan = 1
             .
    END.

    IF LAST-OF(buRo.nromov) THEN
      ASSIGN cRgo = cRgo + STRING(buRo.nromov)        + CHR(1) + 
                           STRING(i)                  + CHR(1) + 
                           STRING(j - 1)              + CHR(1) + 
                           STRING(buRo.id_tipotambor) + CHR(1) + 
                           STRING(buRo.id_articulo)   + CHR(1) +
                           STRING(buRo.id_calidad)    + CHR(1) + 
                           STRING(buRo.id_envase)     + CHR(1) + 
                           STRING(buRo.id_lote)       + CHR(1) + 
                           STRING(buRo.anio)          + CHR(1) + 
                           STRING(buRo.id_empresa)    + CHR(1) +
                           STRING(buRo.id_sucursal)   + CHR(1) + 
                           STRING(buRo.id_tipotambor) + CHR(1) + 
                           STRING(dKil)               + CHR(1) + 
                           STRING(iCan)               + CHR(10)
             i    = buRo.id_tambor
             j    = buRo.id_tambor
             dKil = 0
             iCan = 0
             .
  END. /*for each buRo*/


  /*recorro la lista y creo las partes del remito*/
  j = NUM-ENTRIES(cRgo, CHR(10)) - 1.
  DO i = 1 TO j:
    /*creo items factura*/
    cAux = ENTRY(i, cRgo, CHR(10)).

    iPte = DYNAMIC-FUNCTION('getNextItemRemito' IN hLib, piSuc, piTip, piNro).
    iDes = INTEGER(ENTRY(2, cAux, CHR(1))).
    iHas = INTEGER(ENTRY(3, cAux, CHR(1))).
    dKil = DECIMAL(ENTRY(13, cAux, CHR(1))).
    iCan = INTEGER(ENTRY(14, cAux, CHR(1))).

    CREATE items_factura.
    ASSIGN items_factura.ITEM           = iPte
           items_factura.id_sucursal    = piSuc
           items_factura.id_tipo_movsto = piTip
           items_factura.nro            = piNro         
           items_factura.id_tipotambor  = INTEGER(ENTRY(4, cAux, CHR(1)))
           items_factura.id_articulo    = INTEGER(ENTRY(5, cAux, CHR(1)))
           items_factura.id_calidad     = INTEGER(ENTRY(6, cAux, CHR(1)))
           items_factura.id_envase      = INTEGER(ENTRY(7, cAux, CHR(1)))
           items_factura.nro_lote       = STRING(ENTRY(8, cAux, CHR(1)), "9999") + "/" + SUBSTRING(ENTRY(9, cAux, CHR(1)), 3, 2)
           items_factura.desde_lote     = iDes
           items_factura.hasta_lote     = iHas
           items_factura.kilos          = dKil
           items_factura.peso           = dKil / iCan
           items_factura.cantidad       = iCan    
           .

    /*creo relacion item_factura tambor*/  /*esto esta feo me parece que se puede optimizar mas este codigo*/

    /*relacion tambor item_factura*/
    RUN setRelTamboresRemito IN hLib (INTEGER(ENTRY(10, cAux, CHR(1))),
                                      INTEGER(ENTRY(11, cAux, CHR(1))),
                                      INTEGER(ENTRY(12, cAux, CHR(1))),
                                      INTEGER(ENTRY(1, cAux, CHR(1))),
                                      iDes,
                                      iHas,
                                      piSuc,
                                      piTip,
                                      piNro,
                                      iPte).
      
  END. /*do i = 1 to j*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cerrarItemRemito dTables  _DB-REQUIRED
PROCEDURE cerrarItemRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPte AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPes AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buRo FOR rowObject.
  DEFINE BUFFER buRead FOR rowObject.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
  

  /*creo un item_factura por cada nromov*/
  FOR EACH buRo 
      BREAK BY buRo.nromov 
            BY buRo.id_tambor .
    
    IF FIRST-OF(buRo.nromov) THEN
      iDes = buRo.id_tambor.

    IF LAST-OF(buRo.nromov) THEN DO:
      iPte = DYNAMIC-FUNCTION('getNextItemRemito' IN hLib, piSuc, piTip, piNro).
      CREATE items_factura.
      ASSIGN items_factura.ITEM           = iPte
             items_factura.id_sucursal    = piSuc
             items_factura.id_tipo_movsto = piTip
             items_factura.nro            = piNro         
             items_factura.id_tipotambor  = buRo.id_tipotambor
             items_factura.id_articulo    = buRo.id_articulo
             items_factura.id_calidad     = buRo.id_calidad
             items_factura.id_envase      = buRo.id_envase
             items_factura.nro_lote       = STRING(buRo.id_lote, "9999") + "/" + SUBSTRING(STRING(buRo.anio), 3, 2)
             items_factura.desde_lote     = iDes
             .

      FOR EACH buRead NO-LOCK 
          WHERE buRead.nromov = buRo.nromov
          BY buRead.id_tambor.

        iHas = buRead.id_tambor.
        dKil = dKil + buRead.kilos.
        dPes = buRead.kilos.
        iCan = iCan + 1.

        /*relacion tambor item_factura*/
        RUN setRelTamboresRemito IN hLib (buRead.id_empresa,
                                          buRead.id_sucursal,
                                          buRead.id_tipotambor,
                                          buRead.nromov,
                                          buRead.id_tambor,
                                          buRead.id_tambor,
                                          piSuc,
                                          piTip,
                                          piNro,
                                          iPte).
      END. /*for each buRead*/

      ASSIGN items_factura.kilos      = dKil
             items_factura.peso       = dKil / iCan
             items_factura.cantidad   = iCan
             items_factura.hasta_lote = iHas
             .

      ASSIGN dKil = 0
             dPes = 0
             iCan = 0
             .

    END.
    
  END. /*for first buRo*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearRowObject dTables  _DB-REQUIRED
PROCEDURE clearRowObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttLecturaTambores.
    DELETE ttLecturaTambores.    
  END.

  openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Articulo = (getArticulo())
         rowObject.Calidad = (getCalidad())
         rowObject.Envase = (getEnvase())
      .

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo NO-LOCK.
    cRet = productos_terminados.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST calidades WHERE calidades.id_calidad = rowObject.id_calidad NO-LOCK.
    cRet = calidades.abreviatura.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase NO-LOCK.
    cRet = envases_prod.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExisteTambor dTables  _DB-REQUIRED
FUNCTION getExisteTambor RETURNS LOGICAL
  (piNro AS INTEGER,
   piTam AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FOR FIRST ttLecturaTambores WHERE ttLecturaTambores.nromov   = piNro
                                AND ttLecturaTambores.id_tambor = piTam
                              NO-LOCK.
    lRet = TRUE.
    
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastRowId dTables  _DB-REQUIRED
FUNCTION getLastRowId RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rRow AS ROWID      NO-UNDO.

  DEFINE BUFFER buRo FOR rowObject.
  FOR LAST buRo NO-LOCK.
    rRow = ROWID(buRo).
       
  END.

  RETURN rRow.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

