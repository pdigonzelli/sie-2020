&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES datos_jugo_clarificado
&Scoped-define FIRST-EXTERNAL-TABLE datos_jugo_clarificado


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR datos_jugo_clarificado.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS datos_jugo_clarificado.jugo_naranja ~
datos_jugo_clarificado.Agua datos_jugo_clarificado.Bisulfito_sodio ~
datos_jugo_clarificado.Benzoato_sodio datos_jugo_clarificado.SO2_inicial ~
datos_jugo_clarificado.SO2_final datos_jugo_clarificado.Sodio ~
datos_jugo_clarificado.N-NH2_inicial datos_jugo_clarificado.N-NH2_final ~
datos_jugo_clarificado.Benzoato_inicial ~
datos_jugo_clarificado.Benzoato_final ~
datos_jugo_clarificado.Acido_ascorbico_inicial ~
datos_jugo_clarificado.Acido_ascorbico_final 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}jugo_naranja ~{&FP2}jugo_naranja ~{&FP3}~
 ~{&FP1}Agua ~{&FP2}Agua ~{&FP3}~
 ~{&FP1}Bisulfito_sodio ~{&FP2}Bisulfito_sodio ~{&FP3}~
 ~{&FP1}Benzoato_sodio ~{&FP2}Benzoato_sodio ~{&FP3}~
 ~{&FP1}SO2_inicial ~{&FP2}SO2_inicial ~{&FP3}~
 ~{&FP1}SO2_final ~{&FP2}SO2_final ~{&FP3}~
 ~{&FP1}Sodio ~{&FP2}Sodio ~{&FP3}~
 ~{&FP1}N-NH2_inicial ~{&FP2}N-NH2_inicial ~{&FP3}~
 ~{&FP1}N-NH2_final ~{&FP2}N-NH2_final ~{&FP3}~
 ~{&FP1}Benzoato_inicial ~{&FP2}Benzoato_inicial ~{&FP3}~
 ~{&FP1}Benzoato_final ~{&FP2}Benzoato_final ~{&FP3}~
 ~{&FP1}Acido_ascorbico_inicial ~{&FP2}Acido_ascorbico_inicial ~{&FP3}~
 ~{&FP1}Acido_ascorbico_final ~{&FP2}Acido_ascorbico_final ~{&FP3}
&Scoped-define ENABLED-TABLES datos_jugo_clarificado
&Scoped-define FIRST-ENABLED-TABLE datos_jugo_clarificado
&Scoped-Define DISPLAYED-FIELDS datos_jugo_clarificado.jugo_naranja ~
datos_jugo_clarificado.Agua datos_jugo_clarificado.Bisulfito_sodio ~
datos_jugo_clarificado.Benzoato_sodio datos_jugo_clarificado.SO2_inicial ~
datos_jugo_clarificado.SO2_final datos_jugo_clarificado.Sodio ~
datos_jugo_clarificado.N-NH2_inicial datos_jugo_clarificado.N-NH2_final ~
datos_jugo_clarificado.Benzoato_inicial ~
datos_jugo_clarificado.Benzoato_final ~
datos_jugo_clarificado.Acido_ascorbico_inicial ~
datos_jugo_clarificado.Acido_ascorbico_final 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
nromov||y|general.datos_jugo_clarificado.nromov
id_sucursal||y|general.datos_jugo_clarificado.id_sucursal
id_tipotambor||y|general.datos_jugo_clarificado.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nromov,id_sucursal,id_tipotambor"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida V-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     datos_jugo_clarificado.jugo_naranja AT ROW 1.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Agua AT ROW 2.14 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Bisulfito_sodio AT ROW 3.19 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Benzoato_sodio AT ROW 4.24 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.SO2_inicial AT ROW 5.29 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.SO2_final AT ROW 6.33 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Sodio AT ROW 7.48 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.N-NH2_inicial AT ROW 2.05 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.N-NH2_final AT ROW 3.1 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Benzoato_inicial AT ROW 4.14 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Benzoato_final AT ROW 5.19 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     datos_jugo_clarificado.Acido_ascorbico_inicial AT ROW 6.24 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     datos_jugo_clarificado.Acido_ascorbico_final AT ROW 7.29 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.datos_jugo_clarificado
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.62
         WIDTH              = 80.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
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

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "campos" V-table-Win _INLINE
/* Actions: custom/support/cuscampv.p custom/support/cuscampv.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "relaciones" V-table-Win _INLINE
/* Actions: custom/support/keyedit.w custom/support/keyedit.w ? ? ? */
/* campos relacionados con tablas externas 
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "borrado" V-table-Win _INLINE
/* Actions: ? custom/support/cusborfv.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Cabecera" V-table-Win _INLINE
/* Actions: ? custom/support/set-cabecera.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Detalle" V-table-Win _INLINE
/* Actions: ? custom/support/set-detalle.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Items" V-table-Win _INLINE
/* Actions: ? custom/support/set-items.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME datos_jugo_clarificado.Acido_ascorbico_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Acido_ascorbico_final V-table-Win
ON LEAVE OF datos_jugo_clarificado.Acido_ascorbico_final IN FRAME F-Main /* Acido_ascorbico_final */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Acido_ascorbico_inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Acido_ascorbico_inicial V-table-Win
ON LEAVE OF datos_jugo_clarificado.Acido_ascorbico_inicial IN FRAME F-Main /* Acido_ascorbico_inicial */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Agua
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Agua V-table-Win
ON LEAVE OF datos_jugo_clarificado.Agua IN FRAME F-Main /* Agua */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Benzoato_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Benzoato_final V-table-Win
ON LEAVE OF datos_jugo_clarificado.Benzoato_final IN FRAME F-Main /* Benzoato_final */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Benzoato_inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Benzoato_inicial V-table-Win
ON LEAVE OF datos_jugo_clarificado.Benzoato_inicial IN FRAME F-Main /* Benzoato_inicial */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Benzoato_sodio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Benzoato_sodio V-table-Win
ON LEAVE OF datos_jugo_clarificado.Benzoato_sodio IN FRAME F-Main /* Benzoato_sodio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Bisulfito_sodio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Bisulfito_sodio V-table-Win
ON LEAVE OF datos_jugo_clarificado.Bisulfito_sodio IN FRAME F-Main /* Bisulfito_sodio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.jugo_naranja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.jugo_naranja V-table-Win
ON LEAVE OF datos_jugo_clarificado.jugo_naranja IN FRAME F-Main /* jugo_naranja */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.N-NH2_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.N-NH2_final V-table-Win
ON LEAVE OF datos_jugo_clarificado.N-NH2_final IN FRAME F-Main /* N-NH2_final */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.N-NH2_inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.N-NH2_inicial V-table-Win
ON LEAVE OF datos_jugo_clarificado.N-NH2_inicial IN FRAME F-Main /* N-NH2_inicial */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.SO2_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.SO2_final V-table-Win
ON LEAVE OF datos_jugo_clarificado.SO2_final IN FRAME F-Main /* SO2_final */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.SO2_inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.SO2_inicial V-table-Win
ON LEAVE OF datos_jugo_clarificado.SO2_inicial IN FRAME F-Main /* SO2_inicial */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME datos_jugo_clarificado.Sodio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datos_jugo_clarificado.Sodio V-table-Win
ON LEAVE OF datos_jugo_clarificado.Sodio IN FRAME F-Main /* Sodio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
 
 
  /************************ INTERNAL PROCEDURES ********************/
{custom/support/vinternal.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create V-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r as rowid.
define var hcontainer as handle.
define var emp as integer.
define var suc as integer.
define var tip as integer.
define var nro as integer.
define var lot as integer.

run get-container (output hcontainer).

if hcontainer <> ? then
do:
    run dame-rowid in hcontainer (output r).

end.

for each lotes_jugo where rowid(lotes_jugo) = r.
    emp = lotes_jugo.id_empresa.
    suc = lotes_jugo.id_sucursal.
    tip = lotes_jugo.id_tipotambor.
    nro = lotes_jugo.nromov.
    lot = lotes_jugo.id_lote.
end.

assign datos_jugo_clarificado.id_empresa = emp
       datos_jugo_clarificado.id_sucursal = suc
       datos_jugo_clarificado.id_lote = lot
       datos_jugo_clarificado.id_tipotambor = tip
       datos_jugo_clarificado.nromov = nro
       datos_jugo_clarificado.c_usuario = userid("userdb")
       datos_jugo_clarificado.c_fecha = today
       datos_jugo_clarificado.c_hora = string(time,"HH:MM:SS").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete V-table-Win 
PROCEDURE adm-post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update V-table-Win 
PROCEDURE adm-post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create V-table-Win 
PROCEDURE adm-pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete V-table-Win 
PROCEDURE adm-pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "datos_jugo_clarificado"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "datos_jugo_clarificado"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE agregar V-table-Win 
PROCEDURE agregar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define var ch as character no-undo.
    define var i as integer no-undo.
    define var h as handle no-undo.
    run get-link-handle in adm-broker-hdl 
        ( input this-procedure , input 'TABLEIO-SOURCE' , output ch). 
    do i = 1 to num-entries(ch) :
        h = widget-handle(entry(i,ch)).
        if valid-handle(h) then
            run agregar in h.     
    end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_campos V-table-Win 
PROCEDURE deshabilita_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_campos as character.
define var i as integer no-undo.
define var f as handle no-undo.
define var h as handle no-undo.


do i = 1 to num-entries(lista_campos):
    f = frame f-main:first-child.
    h = f:first-tab-item.
    do while valid-handle(h):
        if h:name = entry(i,lista_campos) then
        do:
            h:sensitive = false.
            leave.
        end.    
        h = h:next-tab-item.
    end.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar V-table-Win 
PROCEDURE grabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define var ch as character no-undo.
    define var i as integer no-undo.
    define var h as handle no-undo.
    run get-link-handle in adm-broker-hdl 
        ( input this-procedure , input 'TABLEIO-SOURCE' , output ch). 
    do i = 1 to num-entries(ch) :
        h = widget-handle(entry(i,ch)).
        if valid-handle(h) then
        do:
            run activa in h.
            run grabar in h.
        end.        
    end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion V-table-Win 
PROCEDURE habilitar_relacion :
define var field-group as handle.
define var cur-control as handle.
define var lista_relacion as character no-undo initial "".
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.
do while valid-handle(cur-control): 

    if cur-control:visible and cur-control:type = "fill-in"
    and lookup(cur-control:name,lista_relacion) <> 0 then 
        cur-control:load-mouse-pointer("glove").
    cur-control = cur-control:next-tab-item.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  DEFINE VAR CVINCULADO AS CHARACTER NO-UNDO.
  RUN GET-LINK-HANDLE IN ADM-BROKER-HDL ( THIS-PROCEDURE , 'VINCULADO-SOURCE' , OUTPUT CVINCULADO ).
  HVINCULADO = WIDGET-HANDLE(CVINCULADO).
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run habilitar_relacion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nromov" "datos_jugo_clarificado" "nromov"}
  {src/adm/template/sndkycas.i "id_sucursal" "datos_jugo_clarificado" "id_sucursal"}
  {src/adm/template/sndkycas.i "id_tipotambor" "datos_jugo_clarificado" "id_tipotambor"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "datos_jugo_clarificado"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valida V-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  case nombre:
    when "id_sucursal" then
        if integer(valor) = 0 then 
        do:
            mensaje = "error".
            return false.
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


