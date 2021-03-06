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

DEFINE VAR vHLote AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES release_delivery

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  estado fecha_confirmacion_entrega fecha_creacion fecha_entrega fecha_envio~
 fecha_limite_frio id_cliente id_release_delivery numero_release~
 observaciones unidad id_contrato item_contrato id_tipo_contrato~
 anio_contrato id_sucursal_ubicacion Cantidad id_articulo id_calidad~
 id_envase calidad envase id_delivery_carrier c_usuario c_fecha c_hora
&Scoped-define ENABLED-FIELDS-IN-release_delivery estado ~
fecha_confirmacion_entrega fecha_creacion fecha_entrega fecha_envio ~
fecha_limite_frio id_cliente id_release_delivery numero_release ~
observaciones unidad id_contrato item_contrato id_tipo_contrato ~
anio_contrato id_sucursal_ubicacion id_articulo id_calidad id_envase ~
calidad envase id_delivery_carrier c_usuario c_fecha c_hora 
&Scoped-Define DATA-FIELDS  estado fecha_confirmacion_entrega fecha_creacion fecha_entrega fecha_envio~
 fecha_limite_frio id_cliente id_release_delivery numero_release~
 observaciones unidad id_contrato item_contrato id_tipo_contrato~
 anio_contrato id_sucursal_ubicacion Cantidad id_articulo id_calidad~
 id_envase calidad envase id_delivery_carrier c_usuario c_fecha c_hora
&Scoped-define DATA-FIELDS-IN-release_delivery estado ~
fecha_confirmacion_entrega fecha_creacion fecha_entrega fecha_envio ~
fecha_limite_frio id_cliente id_release_delivery numero_release ~
observaciones unidad id_contrato item_contrato id_tipo_contrato ~
anio_contrato id_sucursal_ubicacion id_articulo id_calidad id_envase ~
calidad envase id_delivery_carrier c_usuario c_fecha c_hora 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dreleasedelivery.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH release_delivery NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH release_delivery NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main release_delivery
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main release_delivery


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContrato dTables  _DB-REQUIRED
FUNCTION getContrato RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFechaProceso dTables  _DB-REQUIRED
FUNCTION getFechaProceso RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroRelease dTables  _DB-REQUIRED
FUNCTION getNextNroRelease RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAnioContrato dTables  _DB-REQUIRED
FUNCTION setAnioContrato RETURNS LOGICAL
( INPUT piAnioContrato AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTipoContrato dTables  _DB-REQUIRED
FUNCTION setTipoContrato RETURNS LOGICAL
  ( INPUT piTipoContrato AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      release_delivery SCROLLING.
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
     _TblList          = "general.release_delivery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.release_delivery.estado
"estado" "estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes
     _FldNameList[2]   > general.release_delivery.fecha_confirmacion_entrega
"fecha_confirmacion_entrega" "fecha_confirmacion_entrega" ? ? "date" ? ? ? ? ? ? yes ? no 27 yes
     _FldNameList[3]   > general.release_delivery.fecha_creacion
"fecha_creacion" "fecha_creacion" ? ? "date" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[4]   > general.release_delivery.fecha_entrega
"fecha_entrega" "fecha_entrega" ? ? "date" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[5]   > general.release_delivery.fecha_envio
"fecha_envio" "fecha_envio" ? ? "date" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[6]   > general.release_delivery.fecha_limite_frio
"fecha_limite_frio" "fecha_limite_frio" ? ? "date" ? ? ? ? ? ? yes ? no 15.2 yes
     _FldNameList[7]   > general.release_delivery.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[8]   > general.release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.release_delivery.numero_release
"numero_release" "numero_release" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[10]   > general.release_delivery.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 250 yes
     _FldNameList[11]   > general.release_delivery.unidad
"unidad" "unidad" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[12]   > general.release_delivery.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[13]   > general.release_delivery.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[14]   > general.release_delivery.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[15]   > general.release_delivery.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[16]   > general.release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[17]   > "_<CALC>"
"getCantidad()" "Cantidad" ? "zzz,zz9.99" "Decimal" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[18]   > general.release_delivery.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[19]   > general.release_delivery.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[20]   > general.release_delivery.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[21]   > general.release_delivery.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[22]   > general.release_delivery.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[23]   > general.release_delivery.id_delivery_carrier
"id_delivery_carrier" "id_delivery_carrier" ? ? "integer" ? ? ? ? ? ? yes ? no 14.6 yes
     _FldNameList[24]   > general.release_delivery.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.release_delivery.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[26]   > general.release_delivery.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
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
DEFINE BUFFER Rod FOR RowObjUpd.
    
/*  
    ESTE CODIGO FUE PUESTO AL FINAL EN EL POSTTRANSACTIONVALIDATE

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U".
   FIND ROD WHERE ROD.RowNum = RowObjUpd.RowNum AND RoD.RowMod = "" NO-ERROR.
   IF RowObjUpd.estado <> RoD.estado THEN DO:
       FOR EACH items_release_delivery WHERE items_release_delivery.id_release_delivery = 
                                             RowObjUpd.id_release_delivery NO-LOCK.
           FIND lotes_ubicacion OF items_release_delivery NO-ERROR.
           IF NOT AVAILABLE lotes_ubicacion THEN
               RETURN "No pudo encontrar el lote en esa ubicacion".
           ASSIGN lotes_ubicacion.cantidad_comprometida = lotes_ubicacion.cantidad_comprometida -
                                                          items_release_delivery.cantidad.
       END.
   END.
END.
*/
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
         rowObject.Cantidad = (getCantidad())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject dTables  _DB-REQUIRED
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DELETE PROCEDURE vhLote.
  vhLote = ?.

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
DEFINE BUFFER Rod FOR RowObjUpd.
DEFINE VAR v_suc_origen AS INTEGER.
DEFINE VAR v_usuarios AS CHAR.
DEFINE VAR v_subject AS CHAR.
DEFINE VAR v_body AS CHAR.
DEFINE VAR iCant  AS INTEGER NO-UNDO.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".
  ASSIGN RowObjUpd.c_usuario  = USERID("userdb")
         RowObjUpd.c_fecha    = TODAY  
         RowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
  /** CODIGO AGREGADO POR ADRIAN 03/12/2003 HS 11:30 */
  RowObjUpd.changedFields = RowObjUpd.changedFields + ", " + "c_usuario,c_fecha,c_hora".
  /***************************************************/
  
END.


FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
  FOR EACH items_release_delivery WHERE items_release_delivery.id_release_delivery = RowObjUpd.id_release_delivery .
    /*elimino los items del release y restituyo las cantidades a lotes_ubicacion*/
    FOR FIRST lotes_ubicacion WHERE lotes_ubicacion.id_sucursal_ubicacion   = items_release_delivery.id_sucursal_ubicacion
                                AND lotes_ubicacion.id_empresa              = items_release_delivery.id_empresa
                                AND lotes_ubicacion.id_sucursal             = items_release_delivery.id_sucursal
                                AND lotes_ubicacion.id_tipotambor           = items_release_delivery.id_tipotambor
                                AND lotes_ubicacion.nromov                  = items_release_delivery.nromov.

        iCant = lotes_ubicacion.cantidad_comprometida - items_release_delivery.tambores.
        IF iCant > 0  THEN
          ASSIGN lotes_ubicacion.cantidad_comprometida = iCant.
        ELSE
          ASSIGN lotes_ubicacion.cantidad_comprometida = 0.
    END.
    DELETE items_release_delivery.
  END.
END.


FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U". /* ES UN UPDATE DEL RowObjUpd */  

   FIND ROD WHERE ROD.RowNum = RowObjUpd.RowNum AND RoD.RowMod = "" NO-ERROR.
            /* ME POSICIONO EN EL RELEASE QUE ESTOY PARADO EN EL BROWSER */
   IF RowObjUpd.fecha_confirmacion_entrega <> RoD.fecha_confirmacion_entrega THEN DO:
            /* ME FIJO SI EL CAMPO FECHA ENTREGA SE MODIFICICO */
            /* EN CASO QUE SI, ES PORQUE SE ESTA ENTREGANDO LA MERCADERIA ESA FECHA */
       FOR EACH items_release_delivery WHERE items_release_delivery.id_release_delivery = RowObjUpd.id_release_delivery NO-LOCK.
           /*
           FIND FIRST lotes_ubicacion OF items_release_delivery NO-ERROR.
           IF NOT AVAILABLE lotes_ubicacion THEN
           ELSE DO:
           */
           
           DO:
               RUN confirmacionRelease IN vhLote (items_release_delivery.id_empresa , 
                                                  items_release_delivery.id_sucursal ,
                                                  items_release_delivery.id_tipotambor ,
                                                  items_release_delivery.nromov ,
                                                  items_release_delivery.id_sucursal_ubicacion ,
                                                  182 ,
                                                  TODAY ,
                                                  items_release_delivery.tambores).

               IF RETURN-VALUE <> "" THEN
                    RETURN RETURN-VALUE.


               

           END.
       END.
   END.
END.

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
  ASSIGN RowObjUpd.c_usuario  = USERID("userdb")
         RowObjUpd.c_fecha    = TODAY  
         RowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
  /** CODIGO AGREGADO POR ADRIAN 03/12/2003 HS 11:30 */
  RowObjUpd.changedFields = RowObjUpd.changedFields + ", " + "c_usuario,c_fecha,c_hora".
  /***************************************************/


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fecha_creacionValidate dTables  _DB-REQUIRED
PROCEDURE fecha_creacionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcValorFecha AS CHARACTER NO-UNDO.
  /*by facundo 18/12/2003 hs 11.15 - le quito este control porque estoy haciendo pruebas con yurgen*/
  /*
  IF DATE(pcValorFecha) < TODAY THEN
    RETURN "Error en la fecha de creacion".

  RETURN "".   /* Function return value. */
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fecha_limite_frioValidate dTables  _DB-REQUIRED
PROCEDURE fecha_limite_frioValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcValorFecha AS CHARACTER NO-UNDO.

  FIND CURRENT RowObject NO-ERROR.
  
  IF DATE(pcValorFecha) <= RowObject.fecha_creacion THEN
    RETURN "Error en la fecha de creacion".

  RETURN "".   /* Function return value. */


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



  RUN libLotesUbicacion.p PERSISTENT SET vhLote.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numero_releaseValidate dTables  _DB-REQUIRED
PROCEDURE numero_releaseValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcValorNroRelease AS CHARACTER NO-UNDO.

  IF INTEGER(pcValorNroRelease) > 0 THEN 
    RETURN "".
  ELSE
    RETURN "Error en Nro de Release".

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
  DEFINE VAR iSeq AS INTEGER    NO-UNDO.
  
  DEFINE BUFFER ROD FOR RowObjUpd.
  
  FIND FIRST RowObjUpd NO-ERROR.
  
  FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A"
                        OR RowObjUpd.RowMod = "C".
    iSeq = NEXT-VALUE(id_release_delivery).
    ASSIGN RowObjUpd.id_release_delivery = iSeq.
    RowObjUpd.changedFields = RowObjUpd.changedFields + ", " + "id_release_delivery".
  END.

  


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
 
  DEFINE VARIABLE dCantidad AS DECIMAL    NO-UNDO.

  FIND CURRENT RowObject NO-ERROR.

  FOR EACH items_release_delivery WHERE items_release_delivery.id_release_delivery = RowObject.id_release_delivery
                                  NO-LOCK.
    dCantidad = dCantidad + items_release_delivery.tambores.
  END.
  IF dCantidad < 0 THEN
    dCantidad = 0.

  RETURN dCantidad.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContrato dTables  _DB-REQUIRED
FUNCTION getContrato RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR iContrato  AS CHARACTER    NO-UNDO.

  FIND CURRENT RowObject NO-LOCK.
  iContrato = RowObject.id_contrato.

  RETURN iContrato.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFechaProceso dTables  _DB-REQUIRED
FUNCTION getFechaProceso RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroRelease dTables  _DB-REQUIRED
FUNCTION getNextNroRelease RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LAST RELEASE_delivery NO-LOCK NO-ERROR.
  IF AVAILABLE RELEASE_delivery THEN DO:
    RETURN RELEASE_delivery.numero_release + 1.
  END.
  
  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAnioContrato dTables  _DB-REQUIRED
FUNCTION setAnioContrato RETURNS LOGICAL
( INPUT piAnioContrato AS INTEGER /* parameter-definitions */ ) :
  
  MESSAGE piAnioContrato VIEW-AS ALERT-BOX.


  FIND CURRENT RowObjUpd NO-ERROR.
  ASSIGN RowObjUpd.anio_contrato = piAnioContrato.
  RowObjUpd.changedFields = RowObjUpd.changedFields  + ",anio_contrato".

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTipoContrato dTables  _DB-REQUIRED
FUNCTION setTipoContrato RETURNS LOGICAL
  ( INPUT piTipoContrato AS INTEGER /* parameter-definitions */ ) :
  
  MESSAGE piTipoContrato VIEW-AS ALERT-BOX.

  FIND CURRENT RowObjUpd NO-ERROR.
  ASSIGN RowObjUpd.id_tipo_contrato = piTipoContrato.
  RowObjUpd.changedFields = RowObjUpd.changedFields  + ",id_tipo_contrato".

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

