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

{adm2/support/customColors.i}

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES produccion_jugo

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  abs520 abs_8_430 Acidez_w_v Acidez_w_w anio benzoato bisulfito Bx_20_20~
 Bx_correg cantidad_1 cantidad_2 cantidad_3 cantidad_envases_nuevo~
 cantidad_envases_recup c_fecha c_hora c_usuario desde_bolsa Fecha~
 hasta_bolsa id_articulo id_calidad id_empresa id_envase_1 id_envase_2~
 id_envase_3 id_produccion id_sucursal id_tipotambor id_turno kilos_1~
 kilos_2 kilos_3 litros nitrogeno nromov pesticida Pulpa pulpa_85 ratio~
 Sodio t_600 unidad_medida vitaminac codigo_prod
&Scoped-define ENABLED-FIELDS-IN-produccion_jugo abs520 abs_8_430 ~
Acidez_w_v Acidez_w_w anio benzoato bisulfito Bx_20_20 Bx_correg cantidad_1 ~
cantidad_2 cantidad_3 cantidad_envases_nuevo cantidad_envases_recup c_fecha ~
c_hora c_usuario desde_bolsa Fecha hasta_bolsa id_articulo id_calidad ~
id_empresa id_envase_1 id_envase_2 id_envase_3 id_produccion id_sucursal ~
id_tipotambor id_turno kilos_1 kilos_2 kilos_3 litros nitrogeno nromov ~
pesticida Pulpa pulpa_85 ratio Sodio t_600 unidad_medida vitaminac ~
codigo_prod 
&Scoped-Define DATA-FIELDS  abs520 Tambores abs_8_430 Acidez_w_v Acidez_w_w anio benzoato bisulfito~
 Bx_20_20 Bx_correg cantidad_1 cantidad_2 cantidad_3 cantidad_envases_nuevo~
 cantidad_envases_recup c_fecha c_hora c_usuario desde_bolsa Fecha~
 hasta_bolsa id_articulo id_calidad id_empresa id_envase_1 id_envase_2~
 id_envase_3 id_produccion id_sucursal id_tipotambor id_turno kilos_1~
 kilos_2 kilos_3 litros nitrogeno nromov pesticida Pulpa pulpa_85 ratio~
 Sodio t_600 unidad_medida vitaminac codigo_prod Articulo Calidad Envase1~
 Envase2 Envase3
&Scoped-define DATA-FIELDS-IN-produccion_jugo abs520 abs_8_430 Acidez_w_v ~
Acidez_w_w anio benzoato bisulfito Bx_20_20 Bx_correg cantidad_1 cantidad_2 ~
cantidad_3 cantidad_envases_nuevo cantidad_envases_recup c_fecha c_hora ~
c_usuario desde_bolsa Fecha hasta_bolsa id_articulo id_calidad id_empresa ~
id_envase_1 id_envase_2 id_envase_3 id_produccion id_sucursal id_tipotambor ~
id_turno kilos_1 kilos_2 kilos_3 litros nitrogeno nromov pesticida Pulpa ~
pulpa_85 ratio Sodio t_600 unidad_medida vitaminac codigo_prod 
&Scoped-Define MANDATORY-FIELDS  id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dproduccionjugo.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH produccion_jugo ~
      WHERE produccion_jugo.anio >= 2003 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH produccion_jugo ~
      WHERE produccion_jugo.anio >= 2003 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main produccion_jugo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main produccion_jugo


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase1 dTables  _DB-REQUIRED
FUNCTION getEnvase1 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase2 dTables  _DB-REQUIRED
FUNCTION getEnvase2 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase3 dTables  _DB-REQUIRED
FUNCTION getEnvase3 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroProduccion dTables  _DB-REQUIRED
FUNCTION getNextNroProduccion RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTambores dTables  _DB-REQUIRED
FUNCTION getTambores RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      produccion_jugo SCROLLING.
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
     _TblList          = "general.produccion_jugo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.produccion_jugo.anio >= 2003"
     _FldNameList[1]   > general.produccion_jugo.abs520
"abs520" "abs520" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[2]   > "_<CALC>"
"getTambores()" "Tambores" "Tambores" ">>>9" "Integer" ? ? ? ? ? ? no ? no 9.4 no
     _FldNameList[3]   > general.produccion_jugo.abs_8_430
"abs_8_430" "abs_8_430" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[4]   > general.produccion_jugo.Acidez_w_v
"Acidez_w_v" "Acidez_w_v" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[5]   > general.produccion_jugo.Acidez_w_w
"Acidez_w_w" "Acidez_w_w" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[6]   > general.produccion_jugo.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[7]   > general.produccion_jugo.benzoato
"benzoato" "benzoato" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[8]   > general.produccion_jugo.bisulfito
"bisulfito" "bisulfito" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.produccion_jugo.Bx_20_20
"Bx_20_20" "Bx_20_20" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[10]   > general.produccion_jugo.Bx_correg
"Bx_correg" "Bx_correg" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[11]   > general.produccion_jugo.cantidad_1
"cantidad_1" "cantidad_1" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[12]   > general.produccion_jugo.cantidad_2
"cantidad_2" "cantidad_2" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[13]   > general.produccion_jugo.cantidad_3
"cantidad_3" "cantidad_3" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[14]   > general.produccion_jugo.cantidad_envases_nuevo
"cantidad_envases_nuevo" "cantidad_envases_nuevo" ? ? "integer" ? ? ? ? ? ? yes ? no 24.6 yes
     _FldNameList[15]   > general.produccion_jugo.cantidad_envases_recup
"cantidad_envases_recup" "cantidad_envases_recup" ? ? "integer" ? ? ? ? ? ? yes ? no 24 yes
     _FldNameList[16]   > general.produccion_jugo.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[17]   > general.produccion_jugo.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[18]   > general.produccion_jugo.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[19]   > general.produccion_jugo.desde_bolsa
"desde_bolsa" "desde_bolsa" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[20]   > general.produccion_jugo.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[21]   > general.produccion_jugo.hasta_bolsa
"hasta_bolsa" "hasta_bolsa" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[22]   > general.produccion_jugo.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[23]   > general.produccion_jugo.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[24]   > general.produccion_jugo.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[25]   > general.produccion_jugo.id_envase_1
"id_envase_1" "id_envase_1" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[26]   > general.produccion_jugo.id_envase_2
"id_envase_2" "id_envase_2" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[27]   > general.produccion_jugo.id_envase_3
"id_envase_3" "id_envase_3" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[28]   > general.produccion_jugo.id_produccion
"id_produccion" "id_produccion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[29]   > general.produccion_jugo.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[30]   > general.produccion_jugo.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[31]   > general.produccion_jugo.id_turno
"id_turno" "id_turno" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[32]   > general.produccion_jugo.kilos_1
"kilos_1" "kilos_1" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[33]   > general.produccion_jugo.kilos_2
"kilos_2" "kilos_2" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[34]   > general.produccion_jugo.kilos_3
"kilos_3" "kilos_3" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[35]   > general.produccion_jugo.litros
"litros" "litros" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[36]   > general.produccion_jugo.nitrogeno
"nitrogeno" "nitrogeno" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[37]   > general.produccion_jugo.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[38]   > general.produccion_jugo.pesticida
"pesticida" "pesticida" ? ? "logical" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[39]   > general.produccion_jugo.Pulpa
"Pulpa" "Pulpa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[40]   > general.produccion_jugo.pulpa_85
"pulpa_85" "pulpa_85" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[41]   > general.produccion_jugo.ratio
"ratio" "ratio" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[42]   > general.produccion_jugo.Sodio
"Sodio" "Sodio" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[43]   > general.produccion_jugo.t_600
"t_600" "t_600" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[44]   > general.produccion_jugo.unidad_medida
"unidad_medida" "unidad_medida" ? ? "integer" ? ? ? ? ? ? yes ? no 14.4 yes
     _FldNameList[45]   > general.produccion_jugo.vitaminac
"vitaminac" "vitaminac" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[46]   > general.produccion_jugo.codigo_prod
"codigo_prod" "codigo_prod" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[47]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no
     _FldNameList[48]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[49]   > "_<CALC>"
"getEnvase1()" "Envase1" "Envase1" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[50]   > "_<CALC>"
"getEnvase2()" "Envase2" "Envase2" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[51]   > "_<CALC>"
"getEnvase3()" "Envase3" "Envase3" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
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
  DEFINE VARIABLE lExiste AS LOGICAL    NO-UNDO.

  FIND LAST RowObjUpd NO-ERROR.

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".

    lExiste = DYNAMIC-FUNCTION('getLoteExistente' IN hLib, rowObject.id_sucursal,
                                                           1,
                                                           rowObject.id_produccion,
                                                           rowObject.anio,
                                                           rowObject.id_articulo).
    IF lExiste THEN
      RETURN "Numero de Produccion ya Existente.".
    
    ASSIGN rowObjUpd.id_empresa    = 1
           rowObjUpd.id_tipotambor = 1
           rowObjUpd.nromov        = NEXT-VALUE(nromov)
           rowObjUpd.c_usuario     = USERID("userdb")
           rowObjUpd.c_fecha       = TODAY  
           rowObjUpd.c_hora        = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_empresa,id_tipotambor,nromov,c_usuario,c_fecha,c_hora".
    
  END.

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
    ASSIGN rowObjUpd.c_usuario  = USERID("userdb")
           rowObjUpd.c_fecha    = TODAY  
           rowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".    

    RUN setArticuloLote IN hLib (rowObjUpd.id_empresa,
                                 rowObjUpd.id_sucursal,
                                 rowObjUpd.id_tipotambor,
                                 rowObjUpd.nromov,
                                 rowObjUpd.id_articulo).

    RUN setCalidadLote IN hLib (rowObjUpd.id_empresa,
                                rowObjUpd.id_sucursal,
                                rowObjUpd.id_tipotambor,
                                rowObjUpd.nromov,
                                rowObjUpd.id_calidad).

  END.

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
    IF DYNAMIC-FUNCTION('getLoteTieneRemitos' IN hLib, rowObjUpd.id_empresa,
                                                       rowObjUpd.id_sucursal, 
                                                       rowObjUpd.id_tipotambor, 
                                                       rowObjUpd.nromov) THEN DO:
      MESSAGE "Imposible borrar una produccion que tiene remitos hechos" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "ADM-ERROR".
    END.

    IF DYNAMIC-FUNCTION('getLoteTieneReprocesos' IN hLib, rowObjUpd.id_empresa,
                                                          rowObjUpd.id_sucursal, 
                                                          rowObjUpd.id_tipotambor, 
                                                          rowObjUpd.nromov) THEN DO:
      MESSAGE "Imposible borrar una produccion cuyos tambores fueron reprocesados en lotes o cargas" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "ADM-ERROR".
    END.

    /*agrego entrada en el spool de mails a depositos*/
    RUN addMailingSpoolEntry IN hLib (rowObjUpd.id_empresa,
                                      rowObjUpd.id_sucursal,
                                      rowObjUpd.id_tipotambor,
                                      rowObjUpd.nromov, 
                                      "delete").

    /*borro tambores*/
    RUN deleteDrumsFromBatch IN hLib (rowObjUpd.id_empresa, 
                                      rowObjUpd.id_sucursal, 
                                      rowObjUpd.id_tipotambor, 
                                      rowObjUpd.nromov, 
                                      TRUE).

  END.

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
         rowObject.Envase1 = (getEnvase1())
         rowObject.Envase2 = (getEnvase2())
         rowObject.Envase3 = (getEnvase3())
         rowObject.Tambores = (getTambores())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fechaValidate dTables  _DB-REQUIRED
PROCEDURE fechaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dFecha AS DATE       NO-UNDO.

  dFecha = DATE(pcValor).

  IF dFecha < TODAY - 1 THEN
    RETURN "Imposible agregar producciones con fechas anteriores".
  ELSE
    RETURN "".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_calidadValidate dTables  _DB-REQUIRED
PROCEDURE id_calidadValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF pcValor = "0" THEN
    RETURN "Debe ingresar un valor para Calidad".
  ELSE 
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_produccionValidate dTables  _DB-REQUIRED
PROCEDURE id_produccionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  RETURN "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_sucursalValidate dTables  _DB-REQUIRED
PROCEDURE id_sucursalValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF pcValor = "0" THEN
    RETURN "Debe ingresar un valor para Sucursal".
  ELSE 
    RETURN "".

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

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

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

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo
                    NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN
    RETURN productos_terminados.descripcion.
  ELSE 
    RETURN "".

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
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST calidades WHERE calidades.id_calidad = rowObject.id_calidad
                    NO-LOCK NO-ERROR.
  IF AVAILABLE calidades THEN
    RETURN calidades.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase1 dTables  _DB-REQUIRED
FUNCTION getEnvase1 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase_1
                    NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase2 dTables  _DB-REQUIRED
FUNCTION getEnvase2 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase_2
                    NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase3 dTables  _DB-REQUIRED
FUNCTION getEnvase3 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase_3
                    NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroProduccion dTables  _DB-REQUIRED
FUNCTION getNextNroProduccion RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LAST produccion_jugo NO-LOCK NO-ERROR.
  IF AVAILABLE produccion_jugo THEN
    RETURN produccion_jugo.id_produccion + 1.
  ELSE 
    RETURN 1.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTambores dTables  _DB-REQUIRED
FUNCTION getTambores RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = rowObject.id_empresa
                                AND tambores_industria.id_sucursal    = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor  = rowObject.id_tipotambor
                                AND tambores_industria.nromov         = rowObject.nromov.
    i = i + 1.    
  END.
  

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

