&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asbroker1                AS HANDLE          NO-UNDO.
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

&glob DATA-LOGIC-PROCEDURE .p

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
&Scoped-Define ENABLED-FIELDS  Acidez_w_v Cantidad Acidez_w_w anio Bx_20_20 Bx_correg cantidad_1~
 cantidad_2 cantidad_3 c_fecha c_hora c_usuario desde_bolsa Fecha~
 hasta_bolsa id_articulo id_calidad id_empresa id_envase_1 id_envase_2~
 id_envase_3 id_produccion id_sucursal id_tipotambor id_turno kilos_1~
 kilos_2 kilos_3 litros nromov Pulpa Sodio unidad_medida
&Scoped-define ENABLED-FIELDS-IN-produccion_jugo Acidez_w_v Acidez_w_w anio ~
Bx_20_20 Bx_correg cantidad_1 cantidad_2 cantidad_3 c_fecha c_hora ~
c_usuario desde_bolsa Fecha hasta_bolsa id_articulo id_calidad id_empresa ~
id_envase_1 id_envase_2 id_envase_3 id_produccion id_sucursal id_tipotambor ~
id_turno kilos_1 kilos_2 kilos_3 litros nromov Pulpa Sodio unidad_medida 
&Scoped-Define DATA-FIELDS  Acidez_w_v Cantidad Acidez_w_w anio Bx_20_20 Bx_correg cantidad_1~
 cantidad_2 cantidad_3 c_fecha c_hora c_usuario desde_bolsa Fecha~
 hasta_bolsa id_articulo id_calidad id_empresa id_envase_1 id_envase_2~
 id_envase_3 id_produccion id_sucursal id_tipotambor id_turno kilos_1~
 kilos_2 kilos_3 litros nromov Pulpa Sodio unidad_medida
&Scoped-define DATA-FIELDS-IN-produccion_jugo Acidez_w_v Acidez_w_w anio ~
Bx_20_20 Bx_correg cantidad_1 cantidad_2 cantidad_3 c_fecha c_hora ~
c_usuario desde_bolsa Fecha hasta_bolsa id_articulo id_calidad id_empresa ~
id_envase_1 id_envase_2 id_envase_3 id_produccion id_sucursal id_tipotambor ~
id_turno kilos_1 kilos_2 kilos_3 litros nromov Pulpa Sodio unidad_medida 
&Scoped-Define MANDATORY-FIELDS  id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "d003ProduccionJugo.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH produccion_jugo ~
      WHERE produccion_jugo.id_tipotambor = 11 NO-LOCK ~
    BY produccion_jugo.anio ~
       BY produccion_jugo.Fecha DESCENDING ~
        BY produccion_jugo.desde_bolsa ~
         BY produccion_jugo.hasta_bolsa INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH produccion_jugo ~
      WHERE produccion_jugo.id_tipotambor = 11 NO-LOCK ~
    BY produccion_jugo.anio ~
       BY produccion_jugo.Fecha DESCENDING ~
        BY produccion_jugo.desde_bolsa ~
         BY produccion_jugo.hasta_bolsa INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main produccion_jugo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main produccion_jugo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
     _OrdList          = "general.produccion_jugo.anio|yes,industria.produccion_jugo.Fecha|no,industria.produccion_jugo.desde_bolsa|yes,industria.produccion_jugo.hasta_bolsa|yes"
     _Where[1]         = "general.produccion_jugo.id_tipotambor = 11"
     _FldNameList[1]   > general.produccion_jugo.Acidez_w_v
"Acidez_w_v" "Acidez_w_v" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[2]   > "_<CALC>"
"RowObject.hasta_bolsa - RowObject.desde_bolsa + 1" "Cantidad" ? ">>>>>>>>9" "Integer" ? ? ? ? ? ? yes ? no 10.8 no
     _FldNameList[3]   > general.produccion_jugo.Acidez_w_w
"Acidez_w_w" "Acidez_w_w" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[4]   > general.produccion_jugo.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[5]   > general.produccion_jugo.Bx_20_20
"Bx_20_20" "Bx_20_20" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[6]   > general.produccion_jugo.Bx_correg
"Bx_correg" "Bx_correg" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[7]   > general.produccion_jugo.cantidad_1
"cantidad_1" "cantidad_1" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[8]   > general.produccion_jugo.cantidad_2
"cantidad_2" "cantidad_2" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[9]   > general.produccion_jugo.cantidad_3
"cantidad_3" "cantidad_3" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[10]   > general.produccion_jugo.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[11]   > general.produccion_jugo.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[12]   > general.produccion_jugo.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[13]   > general.produccion_jugo.desde_bolsa
"desde_bolsa" "desde_bolsa" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[14]   > general.produccion_jugo.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[15]   > general.produccion_jugo.hasta_bolsa
"hasta_bolsa" "hasta_bolsa" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[16]   > general.produccion_jugo.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[17]   > general.produccion_jugo.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[18]   > general.produccion_jugo.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[19]   > general.produccion_jugo.id_envase_1
"id_envase_1" "id_envase_1" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[20]   > general.produccion_jugo.id_envase_2
"id_envase_2" "id_envase_2" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[21]   > general.produccion_jugo.id_envase_3
"id_envase_3" "id_envase_3" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[22]   > general.produccion_jugo.id_produccion
"id_produccion" "id_produccion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[23]   > general.produccion_jugo.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[24]   > general.produccion_jugo.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[25]   > general.produccion_jugo.id_turno
"id_turno" "id_turno" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[26]   > general.produccion_jugo.kilos_1
"kilos_1" "kilos_1" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[27]   > general.produccion_jugo.kilos_2
"kilos_2" "kilos_2" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[28]   > general.produccion_jugo.kilos_3
"kilos_3" "kilos_3" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[29]   > general.produccion_jugo.litros
"litros" "litros" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[30]   > general.produccion_jugo.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[31]   > general.produccion_jugo.Pulpa
"Pulpa" "Pulpa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[32]   > general.produccion_jugo.Sodio
"Sodio" "Sodio" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[33]   > general.produccion_jugo.unidad_medida
"unidad_medida" "unidad_medida" ? ? "integer" ? ? ? ? ? ? yes ? no 14.4 yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Cantidad = (RowObject.hasta_bolsa - RowObject.desde_bolsa + 1)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR i             AS INTEGER   NO-UNDO.
DEFINE VAR vExisten      AS INTEGER   NO-UNDO.
DEFINE VAR vAntes        AS INTEGER   NO-UNDO.
DEFINE VAR vDespues      AS INTEGER   NO-UNDO.
DEFINE VAR vSucursal     AS INTEGER   NO-UNDO.
DEFINE VAR vDesde        AS INTEGER   NO-UNDO.
DEFINE VAR vHasta        AS INTEGER   NO-UNDO.
DEFINE VAR vProduccion   AS INTEGER   NO-UNDO.
DEFINE VAR vTiempo       AS CHARACTER NO-UNDO.
DEFINE VAR iCantidad     AS INTEGER   NO-UNDO.
DEFINE VAR vLastId       AS INTEGER   NO-UNDO.
DEFINE VAR iNroMov       AS INTEGER   NO-UNDO.
DEFINE VAR iFlg          AS INTEGER   NO-UNDO.
DEFINE VAR hUpdateSource AS HANDLE    NO-UNDO.

DEFINE BUFFER buProduccionJugo FOR produccion_jugo PRESELECT.



FIND FIRST RowObjUpd.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A"
                      OR RowObjUpd.RowMod = "C". /*insert*/
  
  vSucursal = RowObjUpd.id_sucursal.
  iNroMov   = NEXT-VALUE(nromov).

  ASSIGN RowObjUpd.id_empresa       = 1
         RowObjUpd.id_tipotambor    = 11
         RowObjUpd.nromov           = iNroMov
         RowObjUpd.id_sucursal      = vSucursal.
  
  /*obtengo el ultimo id de bolsa*/
  
  vLastId = 0.

  FOR EACH buProduccionJugo WHERE buProduccionJugo.id_sucursal = vSucursal 
                              AND buProduccionJugo.id_tipotambor = 11
                            BY buProduccionJugo.hasta_bolsa DESC.
    vLastId = buProduccionJugo.hasta_bolsa.
    LEAVE.
  END.
  
  /*obtengo el dato cantidad del viewer*/
  
  hUpdateSource = DYNAMIC-FUNCTION ('getUpdateSource':U).
  iCantidad = DYNAMIC-FUNCTION('getCantidad' IN hUpdateSource).

  vDesde      = vLastId + 1.
  vHasta      = vLastId + iCantidad. /*(cantidad:SCREEN-VALUE IN FRAME F-Main).*/
  ASSIGN RowObjUpd.desde_bolsa = vDesde
         RowObjUpd.hasta_bolsa = vHasta.
  
  vProduccion = RowObjUpd.id_produccion.
  
  /*registro de movimientos de stock*/
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.

  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN appCascInsertTamboresIndustria.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT vDesde, 
                                                                               INPUT vHasta, 
                                                                               INPUT vSucursal, 
                                                                               INPUT RowObjUpd.kilos_1, 
                                                                               INPUT RowObjUpd.anio, 
                                                                               INPUT RowObjUpd.id_empresa, 
                                                                               INPUT RowObjUpd.id_articulo, 
                                                                               INPUT iNroMov, 
                                                                               INPUT RowObjUpd.id_envase_1, 
                                                                               INPUT RowObjUpd.fecha) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.
  
  RUN y_gstkcre_cas.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT 1,
                                                              INPUT vSucursal,
                                                              INPUT 11,
                                                              INPUT vDesde,
                                                              INPUT vHasta,
                                                              INPUT 18) NO-ERROR.
  
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.

  ret = hAppSrv:DISCONNECT().
  DELETE OBJECT hAppSrv.
  /*fin registro stock*/

END.


FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U":U: /*update*/

END.




FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D":U:  /*delete*/

  iFlg = 0.
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    =  RowObjUpd.id_empresa
                                AND tambores_industria.id_sucursal   =  RowObjUpd.id_sucursal                                
                                AND tambores_industria.id_tipotambor =  11
                                AND tambores_industria.nromov        =  RowObjUpd.nromov
                                AND tambores_industria.id_lote       = 0.
                              /*  AND id_tambor     >= vDesde
                                  AND id_tambor     <= vHasta*/
    iFlg = 1.
    DELETE tambores_industria.
  END.
  IF iFlg = 0 THEN DO:
    MESSAGE "Las bolsas seleccionadas se encuentran asociadas a un lote. Imposible Eliminar." VIEW-AS ALERT-BOX.
  END.

  
  /*
  CREATE SERVER hAppSrv.

  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN y_gstkcre_cas.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT 1,
                                                              INPUT RowObjUpd.id_sucursal,
                                                              INPUT 11,
                                                              INPUT RowObjUpd.desde_bolsa,
                                                              INPUT RowObjUpd.hasta_bolsa,
                                                              INPUT 2).
  
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.
  /*fin registro stock*/
  /*elimino en tambores_industria*/

  
  RUN appCascDeleteTamboresIndustria.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT RowObjUpd.id_empresa, 
                                                                               INPUT RowObjUpd.id_sucursal, 
                                                                               INPUT RowObjUpd.nromov) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.
  
  ret = hAppSrv:DISCONNECT().
  DELETE OBJECT hAppSrv.
  */

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshRow dTables  _DB-REQUIRED
PROCEDURE refreshRow :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

