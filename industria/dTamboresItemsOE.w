&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES tambores_industria sucursales ~
productos_terminados

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Anio estado Fecha id_articulo id_calidad id_contrato_of id_empresa~
 id_envase id_locacion_ubicacion id_lote id_lote_nuevo id_orden_entrega~
 id_sucursal id_tambor id_tipotambor item_oe item_of kilos_tambor~
 abreviatura descripcion anio_of id_tipocontrato_of nromov
&Scoped-define ENABLED-FIELDS-IN-tambores_industria Anio estado Fecha ~
id_articulo id_calidad id_contrato_of id_empresa id_envase ~
id_locacion_ubicacion id_lote id_lote_nuevo id_orden_entrega id_sucursal ~
id_tambor id_tipotambor item_oe item_of kilos_tambor anio_of ~
id_tipocontrato_of nromov 
&Scoped-define ENABLED-FIELDS-IN-sucursales abreviatura 
&Scoped-define ENABLED-FIELDS-IN-productos_terminados descripcion 
&Scoped-Define DATA-FIELDS  Anio estado Fecha id_articulo id_calidad id_contrato_of id_empresa~
 id_envase id_locacion_ubicacion id_lote id_lote_nuevo id_orden_entrega~
 id_sucursal id_tambor id_tipotambor item_oe item_of kilos_tambor~
 abreviatura descripcion anio_of id_tipocontrato_of nromov
&Scoped-define DATA-FIELDS-IN-tambores_industria Anio estado Fecha ~
id_articulo id_calidad id_contrato_of id_empresa id_envase ~
id_locacion_ubicacion id_lote id_lote_nuevo id_orden_entrega id_sucursal ~
id_tambor id_tipotambor item_oe item_of kilos_tambor anio_of ~
id_tipocontrato_of nromov 
&Scoped-define DATA-FIELDS-IN-sucursales abreviatura 
&Scoped-define DATA-FIELDS-IN-productos_terminados descripcion 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_lote id_sucursal id_tambor
&Scoped-Define APPLICATION-SERVICE asindustria
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtamboresitemsoe.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tambores_industria ~
      WHERE tambores_industria.id_sucursal_ubicacion <> 85 ~
and tambores_industria.id_sucursal_ubicacion <> 91 ~
and tambores_industria.id_locacion_ubicacion  = 4 ~
 AND (industria.tambores_industria.id_tipotambor = 3 or tambores_industria.id_tipotambor = 6 or tambores_industria.id_tipotambor = 7)  NO-LOCK, ~
      EACH sucursales OF tambores_industria NO-LOCK, ~
      EACH productos_terminados OF tambores_industria NO-LOCK ~
    BY tambores_industria.Anio ~
       BY tambores_industria.id_lote ~
        BY tambores_industria.id_tambor INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tambores_industria ~
      WHERE tambores_industria.id_sucursal_ubicacion <> 85 ~
and tambores_industria.id_sucursal_ubicacion <> 91 ~
and tambores_industria.id_locacion_ubicacion  = 4 ~
 AND (industria.tambores_industria.id_tipotambor = 3 or tambores_industria.id_tipotambor = 6 or tambores_industria.id_tipotambor = 7)  NO-LOCK, ~
      EACH sucursales OF tambores_industria NO-LOCK, ~
      EACH productos_terminados OF tambores_industria NO-LOCK ~
    BY tambores_industria.Anio ~
       BY tambores_industria.id_lote ~
        BY tambores_industria.id_tambor INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tambores_industria sucursales ~
productos_terminados
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tambores_industria
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main sucursales
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main productos_terminados


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AsignarOE dTables  _DB-REQUIRED
FUNCTION AsignarOE RETURNS CHARACTER
  (INPUT pOE AS INTEGER,
   INPUT pItemOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tambores_industria, 
      sucursales, 
      productos_terminados SCROLLING.
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
   Partition: asindustria
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
     _TblList          = "general.tambores_industria,comercial.sucursales OF general.tambores_industria,general.productos_terminados OF general.tambores_industria"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "general.tambores_industria.Anio|yes,industria.tambores_industria.id_lote|yes,industria.tambores_industria.id_tambor|yes"
     _Where[1]         = "general.tambores_industria.id_sucursal_ubicacion <> 85
and general.tambores_industria.id_sucursal_ubicacion <> 91
and general.tambores_industria.id_locacion_ubicacion  = 4
 AND (industria.tambores_industria.id_tipotambor = 3 or general.tambores_industria.id_tipotambor = 6 or general.tambores_industria.id_tipotambor = 7) "
     _FldNameList[1]   > general.tambores_industria.Anio
"Anio" "Anio" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[2]   > general.tambores_industria.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[3]   > general.tambores_industria.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[4]   > general.tambores_industria.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[5]   > general.tambores_industria.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 7 yes
     _FldNameList[6]   > general.tambores_industria.id_contrato_of
"id_contrato_of" "id_contrato_of" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[7]   > general.tambores_industria.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[8]   > general.tambores_industria.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[9]   > general.tambores_industria.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[10]   > general.tambores_industria.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[11]   > general.tambores_industria.id_lote_nuevo
"id_lote_nuevo" "id_lote_nuevo" ? ? "character" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[12]   > general.tambores_industria.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[13]   > general.tambores_industria.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.6 yes
     _FldNameList[14]   > general.tambores_industria.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? yes ? yes 12 yes
     _FldNameList[15]   > general.tambores_industria.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[16]   > general.tambores_industria.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[17]   > general.tambores_industria.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[18]   > general.tambores_industria.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[19]   > comercial.sucursales.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[20]   > general.productos_terminados.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[21]   > general.tambores_industria.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[22]   > general.tambores_industria.id_tipocontrato_of
"id_tipocontrato_of" "id_tipocontrato_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[23]   > general.tambores_industria.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailing dTables  _DB-REQUIRED
PROCEDURE mailing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPt AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j AS INTEGER    NO-UNDO.

  DEFINE BUFFER buRo FOR rowObject.
  FIND FIRST buRo NO-LOCK NO-ERROR.
  i = buRo.id_tambor.
  FIND LAST buRo NO-LOCK NO-ERROR.
  j = buRo.id_tambor.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  RUN mailingTamboresItemOE IN hLib (buRo.id_empresa,
                                     buRo.id_sucursal,
                                     buRo.id_tipotambor,
                                     buRo.nromov,
                                     i, 
                                     j, 
                                     piOE, 
                                     piPt).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AsignarOE dTables  _DB-REQUIRED
FUNCTION AsignarOE RETURNS CHARACTER
  (INPUT pOE AS INTEGER,
   INPUT pItemOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR i AS INTEGER.
DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
DEFINE VARIABLE k    AS INTEGER    NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.


DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
DELETE OBJECT hLibCom.


FOR EACH rowObject.
  RUN asocTamboresOE IN hLib (rowObject.id_empresa,
                              rowObject.id_sucursal,
                              rowObject.id_tipotambor,
                              rowObject.nromov,
                              rowObject.id_tambor,
                              rowObject.id_tambor,
                              pOE,
                              pItemOE).
END.

RETURN "".


/*

i = 0.
IF pOE = 0 THEN DO: /* ESTA LIBERANDO LOS TAMBORES DE LAS OE */

    MESSAGE "Esta seguro que quiere liberar estos tambores?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                                                             TITLE "Pregunta" UPDATE choice AS LOGICAL.
    CASE choice:
        WHEN TRUE THEN /* Yes */ DO:
            FIND CURRENT rowObject NO-ERROR.
            k = rowObject.id_tambor.
            FOR EACH RowObject.
                FIND FIRST tambores_industria WHERE tambores_industria.nromov = rowObject.nromov
                                                AND tambores_industria.id_tambor = rowObject.id_tambor NO-ERROR.
                IF AVAILABLE tambores_industria THEN DO:

                    ASSIGN tambores_industria.id_orden_entrega   = 0
                           tambores_industria.ITEM_oe            = 0
                           tambores_industria.id_contrato_of     = ""
                           tambores_industria.id_tipocontrato_of = 0
                           tambores_industria.anio_of            = 0
                           tambores_industria.item_of            = 0.
                    i = i + 1.
                END.
                ELSE MESSAGE "No encontro el tambor" VIEW-AS ALERT-BOX.
            END.
            MESSAGE "Se liberaron " i " tambores." VIEW-AS ALERT-BOX.
        END.
        WHEN FALSE THEN /* No */ DO:
            UNDO, RETURN NO-APPLY.
        END.
        OTHERWISE /* Cancel */
            STOP.
    END CASE.
END.
ELSE DO:
                    /* ESTA RELACIONANDO LOS TAMBORES CON LA OE */
    
    FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = pOE
                                     AND items_orden_entrega.ITEM_oe            = pItemOE
                                    NO-LOCK NO-ERROR.
    IF AVAILABLE items_orden_entrega THEN DO:
        FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        IF AVAILABLE items_contratos THEN DO:
            FOR EACH RowObject.
                FIND FIRST tambores_industria WHERE tambores_industria.nromov = rowObject.nromov
                                                AND tambores_industria.id_tambor = rowObject.id_tambor NO-ERROR.
                IF AVAILABLE tambores_industria THEN DO:
                    ASSIGN iEmp = tambores_industria.id_empresa
                           iSuc = tambores_industria.id_sucursal
                           iTip = tambores_industria.id_tipotambor
                           iNro = tambores_industria.nromov
                           j    = tambores_industria.id_tambor.

                    ASSIGN tambores_industria.id_orden_entrega   = pOE
                           tambores_industria.ITEM_oe            = pItemOE
                           tambores_industria.id_contrato_of     = items_orden_entrega.id_contrato
                           tambores_industria.id_tipocontrato_of = items_orden_entrega.id_tipo_contrato
                           tambores_industria.anio_of            = items_orden_entrega.anio
                           tambores_industria.item_of            = items_orden_entrega.ITEM.
                    i = i + 1.
                END.
                ELSE MESSAGE "No encontro el tambor" VIEW-AS ALERT-BOX.
            END.
        END.
        ELSE MESSAGE "No se encontro el contrato" VIEW-AS ALERT-BOX.
    END.
    ELSE MESSAGE "No se encontro la parte de OE " pOE VIEW-AS ALERT-BOX.
    
    MESSAGE "Se han modificado " i " tambores." VIEW-AS ALERT-BOX.

    

END.

RETURN "".   /* Function return value. */
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

