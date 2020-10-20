&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
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
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES cargas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Acidez_w_v Acidez_w_w anio Bx_20_20 Bx_correg cantidad_envases_nuevo~
 cantidad_envases_recup cantidad_enzima c_fecha c_hora c_usuario encargado~
 Fecha Hora_comienzo Hora_finalizacion id_articulo id_calidad id_carga~
 id_empresa id_empresa_proceso id_proceso id_quimico id_sucursal~
 id_sucursal_proceso id_tanque id_tipotambor id_tipotambor_proceso litros~
 nromov nromov_proceso observaciones operario_enzima Pulpa Sodio~
 temperatura_carga tiempo_actividad_enzima tipo_enzima volumen_final
&Scoped-define ENABLED-FIELDS-IN-cargas Acidez_w_v Acidez_w_w anio Bx_20_20 ~
Bx_correg cantidad_envases_nuevo cantidad_envases_recup cantidad_enzima ~
c_fecha c_hora c_usuario encargado Fecha Hora_comienzo Hora_finalizacion ~
id_articulo id_calidad id_carga id_empresa id_empresa_proceso id_proceso ~
id_quimico id_sucursal id_sucursal_proceso id_tanque id_tipotambor ~
id_tipotambor_proceso litros nromov nromov_proceso observaciones ~
operario_enzima Pulpa Sodio temperatura_carga tiempo_actividad_enzima ~
tipo_enzima volumen_final 
&Scoped-Define DATA-FIELDS  Acidez_w_v Acidez_w_w anio Bx_20_20 Bx_correg cantidad_envases_nuevo~
 cantidad_envases_recup cantidad_enzima c_fecha c_hora c_usuario encargado~
 Fecha Hora_comienzo Hora_finalizacion id_articulo id_calidad id_carga~
 id_empresa id_empresa_proceso id_proceso id_quimico id_sucursal~
 id_sucursal_proceso id_tanque id_tipotambor id_tipotambor_proceso litros~
 nromov nromov_proceso observaciones operario_enzima Pulpa Sodio~
 temperatura_carga tiempo_actividad_enzima tipo_enzima volumen_final~
 Sucursal Articulo Tanque Quimico SolSolubles Coef Kilos Kilos400
&Scoped-define DATA-FIELDS-IN-cargas Acidez_w_v Acidez_w_w anio Bx_20_20 ~
Bx_correg cantidad_envases_nuevo cantidad_envases_recup cantidad_enzima ~
c_fecha c_hora c_usuario encargado Fecha Hora_comienzo Hora_finalizacion ~
id_articulo id_calidad id_carga id_empresa id_empresa_proceso id_proceso ~
id_quimico id_sucursal id_sucursal_proceso id_tanque id_tipotambor ~
id_tipotambor_proceso litros nromov nromov_proceso observaciones ~
operario_enzima Pulpa Sodio temperatura_carga tiempo_actividad_enzima ~
tipo_enzima volumen_final 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_sucursal id_sucursal_proceso
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dcargas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH cargas ~
      WHERE cargas.Fecha >= date("01/01/2005") NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH cargas ~
      WHERE cargas.Fecha >= date("01/01/2005") NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main cargas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main cargas


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCapacidadTanque dTables  _DB-REQUIRED
FUNCTION getCapacidadTanque RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoef dTables  _DB-REQUIRED
FUNCTION getCoef RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosGrafico dTables  _DB-REQUIRED
FUNCTION getDatosGrafico RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400 dTables  _DB-REQUIRED
FUNCTION getKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextId dTables  _DB-REQUIRED
FUNCTION getNextId RETURNS INTEGER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimico dTables  _DB-REQUIRED
FUNCTION getQuimico RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSolSoluble dTables  _DB-REQUIRED
FUNCTION getSolSoluble RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTanque dTables  _DB-REQUIRED
FUNCTION getTanque RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      cargas SCROLLING.
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
     _TblList          = "general.cargas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.cargas.Fecha >= date(""01/01/2005"")"
     _FldNameList[1]   > general.cargas.Acidez_w_v
"Acidez_w_v" "Acidez_w_v" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[2]   > general.cargas.Acidez_w_w
"Acidez_w_w" "Acidez_w_w" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[3]   > general.cargas.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[4]   > general.cargas.Bx_20_20
"Bx_20_20" "Bx_20_20" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[5]   > general.cargas.Bx_correg
"Bx_correg" "Bx_correg" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[6]   > general.cargas.cantidad_envases_nuevo
"cantidad_envases_nuevo" "cantidad_envases_nuevo" ? ? "integer" ? ? ? ? ? ? yes ? no 24.6 yes
     _FldNameList[7]   > general.cargas.cantidad_envases_recup
"cantidad_envases_recup" "cantidad_envases_recup" ? ? "integer" ? ? ? ? ? ? yes ? no 24 yes
     _FldNameList[8]   > general.cargas.cantidad_enzima
"cantidad_enzima" "cantidad_enzima" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[9]   > general.cargas.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[10]   > general.cargas.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[11]   > general.cargas.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > general.cargas.encargado
"encargado" "encargado" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[13]   > general.cargas.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[14]   > general.cargas.Hora_comienzo
"Hora_comienzo" "Hora_comienzo" ? ? "character" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[15]   > general.cargas.Hora_finalizacion
"Hora_finalizacion" "Hora_finalizacion" ? ? "character" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[16]   > general.cargas.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[17]   > general.cargas.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes
     _FldNameList[18]   > general.cargas.id_carga
"id_carga" "id_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[19]   > general.cargas.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[20]   > general.cargas.id_empresa_proceso
"id_empresa_proceso" "id_empresa_proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[21]   > general.cargas.id_proceso
"id_proceso" "id_proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[22]   > general.cargas.id_quimico
"id_quimico" "id_quimico" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[23]   > general.cargas.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[24]   > general.cargas.id_sucursal_proceso
"id_sucursal_proceso" "id_sucursal_proceso" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[25]   > general.cargas.id_tanque
"id_tanque" "id_tanque" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[26]   > general.cargas.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[27]   > general.cargas.id_tipotambor_proceso
"id_tipotambor_proceso" "id_tipotambor_proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[28]   > general.cargas.litros
"litros" "litros" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[29]   > general.cargas.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[30]   > general.cargas.nromov_proceso
"nromov_proceso" "nromov_proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[31]   > general.cargas.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[32]   > general.cargas.operario_enzima
"operario_enzima" "operario_enzima" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[33]   > general.cargas.Pulpa
"Pulpa" "Pulpa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[34]   > general.cargas.Sodio
"Sodio" "Sodio" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[35]   > general.cargas.temperatura_carga
"temperatura_carga" "temperatura_carga" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[36]   > general.cargas.tiempo_actividad_enzima
"tiempo_actividad_enzima" "tiempo_actividad_enzima" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[37]   > general.cargas.tipo_enzima
"tipo_enzima" "tipo_enzima" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[38]   > general.cargas.volumen_final
"volumen_final" "volumen_final" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[39]   > "_<CALC>"
"getSucursal()" "Sucursal" "Sucursal" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[40]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[41]   > "_<CALC>"
"getTanque()" "Tanque" "Tanque" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no
     _FldNameList[42]   > "_<CALC>"
"getQuimico()" "Quimico" "Quimico" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[43]   > "_<CALC>"
"getSolSoluble()" "SolSolubles" "SolSolubles" "->>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14 no
     _FldNameList[44]   > "_<CALC>"
"getCoef()" "Coef" "Coef" ">,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 8.4 no
     _FldNameList[45]   > "_<CALC>"
"getKilos()" "Kilos" "Kilos" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[46]   > "_<CALC>"
"getKilos400()" "Kilos400" "Kilos400" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
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
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').


  FIND LAST RowObjUpd NO-ERROR.
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 
    IF rowObjUpd.bx_20_20 = ? OR rowObjUpd.acidez_w_w = ? THEN 
      RETURN "Imposible dar de alta una carga sin analisis".
  
    ASSIGN rowObjUpd.id_empresa_proceso     = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa')
           rowObjUpd.id_sucursal_proceso    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
           rowObjUpd.id_tipotambor_proceso  = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor')
           rowObjUpd.nromov_proceso         = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov')
           rowObjUpd.id_proceso             = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_proceso')
           rowObjUpd.id_empresa             = 1
           rowObjUpd.id_sucursal            = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
           rowObjUpd.id_tipotambor          = 10
           rowObjUpd.nromov                 = NEXT-VALUE(nromov)
           rowObjUpd.c_usuario              = USERID("userdb")
           rowObjUpd.c_fecha                = TODAY
           rowObjUpd.c_hora                 = STRING(TIME,"HH:MM:SS").

    cRows = "id_empresa_proceso,id_sucursal_proceso,id_tipotambor_proceso,nromov_proceso,id_proceso," + 
            "id_empresa,id_sucursal,id_tipotambor,nromov," + 
            "c_usuario,c_fecha,c_hora".
    rowObjUpd.changedFields = rowObjUpd.changedFields + "," + cRows.    
    RUN refreshRow IN hSource.
  END.
  
  /*update*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".        
  END.
  
  /*delete*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
    RUN liberarTambores.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bx_20_20Validate dTables  _DB-REQUIRED
PROCEDURE bx_20_20Validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValue AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dBrix AS DECIMAL    NO-UNDO.

  dBrix = DECIMAL(pcValue).

  IF dBrix >= 40 THEN DO: /*pedido por ruben 29/12/2006*/
    RETURN "Error en el valor de analisis. Dicho Valor Destruiria el Ultra-Filtro. Por Favor Corrija!".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createControls dTables  _DB-REQUIRED
PROCEDURE createControls :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

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
         rowObject.Coef = (getCoef())
         rowObject.Kilos = (getKilos())
         rowObject.Kilos400 = (getKilos400())
         rowObject.Quimico = (getQuimico())
         rowObject.SolSolubles = (getSolSoluble())
         rowObject.Sucursal = (getSucursal())
         rowObject.Tanque = (getTanque())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE liberarTambores dTables  _DB-REQUIRED
PROCEDURE liberarTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN removeOrigenes IN hLib (rowObjUpd.id_empresa,
                              rowObjUpd.id_sucursal,
                              rowObjUpd.id_tipotambor,
                              rowObjUpd.nromov).

  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeDrumsFromCarga dTables  _DB-REQUIRED
PROCEDURE removeDrumsFromCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcDrumsList AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r AS ROWID      NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(pcDrumsList, CHR(10)):
    r = TO-ROWID(ENTRY(i, pcDrumsList, CHR(10))).
    FOR FIRST tambores_industria WHERE ROWID(tambores_industria) = r
                                 NO-LOCK.
      RUN setLoteDestinoToTambor IN hLib (tambores_industria.id_empresa,
                                          tambores_industria.id_sucursal,
                                          tambores_industria.id_tipotambor,
                                          tambores_industria.nromov,
                                          tambores_industria.id_tambor,
                                          0,
                                          0,
                                          0,
                                          0,
                                          TRUE).
    END.

  END.
  
  



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
    cRet = productos_terminados.abreviatura.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCapacidadTanque dTables  _DB-REQUIRED
FUNCTION getCapacidadTanque RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST tanque WHERE tanque.id_tanque = rowObject.id_tanque NO-LOCK.
    dRet = tanque.capacidad.
  END.

  IF dRet = 0 THEN 
    dRet = 20000.
  
  RETURN dRet.
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoef dTables  _DB-REQUIRED
FUNCTION getCoef RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL FORMAT ">,>>9.99"   NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR LAST brix WHERE brix.brix <= rowObject.bx_correg BY brix.brix.
    dRet = brix.solido_soluble.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosGrafico dTables  _DB-REQUIRED
FUNCTION getDatosGrafico RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLegend AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cData   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dLitros AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCap    AS DECIMAL    NO-UNDO.


  cRet = DYNAMIC-FUNCTION('getComposicionCarga' IN hLib, rowObject.id_empresa,
                                                         rowObject.id_sucursal,
                                                         rowObject.id_tipotambor,
                                                         rowObject.nromov).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
/*
  dKgs = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, rowObject.acidez_w_w, 
                                                        rowObject.bx_20_20,
                                                        rowObject.litros).
  */
  RETURN dKgs.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400 dTables  _DB-REQUIRED
FUNCTION getKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
/*
  IF rowObject.bx_20_20 <> 0 AND rowObject.acidez_w_w <> 0 THEN 
    dKgs = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, rowObject.bx_20_20, 
                                                      rowObject.acidez_w_w, 
                                                      rowObject.litros, 
                                                      FALSE).
  
  RETURN dKgs.
*/  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextId dTables  _DB-REQUIRED
FUNCTION getNextId RETURNS INTEGER
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iRet AS INTEGER INITIAL 0   NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').

  FOR LAST cargas WHERE cargas.nromov_proceso = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov')
                  NO-LOCK.
    iRet = cargas.id_carga.
  END.
  iRet = iRet + 1.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimico dTables  _DB-REQUIRED
FUNCTION getQuimico RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST quimicos WHERE quimicos.id_quimico = rowObject.id_quimico NO-LOCK.
    cRet = quimicos.nombre.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSolSoluble dTables  _DB-REQUIRED
FUNCTION getSolSoluble RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL FORMAT ">>>,>>>,>>9.99"   NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR LAST brix WHERE brix.brix <= rowObject.bx_correg BY brix.brix.
    dRet = rowObject.litros * brix.solido_soluble.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal NO-LOCK.
    cRet = sucursales.abreviatura.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTanque dTables  _DB-REQUIRED
FUNCTION getTanque RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST tanque WHERE tanque.id_tanque = rowObject.id_tanque NO-LOCK.
    cRet = tanque.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
