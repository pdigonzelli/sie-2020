&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM SmartFrame Template

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
define var alta as logical no-undo initial false.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lotes_jugo

/* Definitions for FRAME F-Main                                         */
&Scoped-define QUERY-STRING-F-Main FOR EACH lotes_jugo NO-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH lotes_jugo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main lotes_jugo
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main lotes_jugo


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS id_contrato_of id_tipocontrato_of anio_of ~
BUTTON-15 BUTTON-14 desde_tambor hasta_tambor BUTTON-1 RECT-16 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS id_contrato_of id_tipocontrato_of anio_of ~
orden_fabricacion nombre_cliente desde_tambor hasta_tambor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_lotes_orden_fabricacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_tambores_lotes_jugo_of AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Borrar asignación" 
     SIZE 31 BY 1.19.

DEFINE BUTTON BUTTON-14 
     LABEL "Transferencia Tambores" 
     SIZE 31 BY 1.14.

DEFINE BUTTON BUTTON-15 
     LABEL "Asignacion Completa del Lote" 
     SIZE 31 BY 1.14.

DEFINE VARIABLE anio_of AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE desde_tambor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Desde Tambor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE hasta_tambor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE id_contrato_of AS CHARACTER FORMAT "X(12)" 
     LABEL "Contract" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE id_tipocontrato_of AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Tipo Contrato" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE nombre_cliente AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE orden_fabricacion AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "OF" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 148 BY 9.52.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 229 BY 50
     BGCOLOR 7 FGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      lotes_jugo SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     id_contrato_of AT ROW 11.24 COL 15 COLON-ALIGNED
     id_tipocontrato_of AT ROW 12.19 COL 21 COLON-ALIGNED
     anio_of AT ROW 13.14 COL 15 COLON-ALIGNED
     orden_fabricacion AT ROW 15.05 COL 15 COLON-ALIGNED
     nombre_cliente AT ROW 16 COL 15 COLON-ALIGNED
     BUTTON-15 AT ROW 17.67 COL 5
     BUTTON-14 AT ROW 18.38 COL 116
     desde_tambor AT ROW 18.62 COL 76 COLON-ALIGNED
     hasta_tambor AT ROW 18.62 COL 98 COLON-ALIGNED
     BUTTON-1 AT ROW 18.86 COL 5
     RECT-16 AT ROW 10.76 COL 3
     RECT-2 AT Y 5 X 445
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.2 BY 20.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 150.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" F-Frame-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" F-Frame-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FILL-IN nombre_cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_fabricacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "general.lotes_jugo"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 F-Frame-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Borrar asignación */
DO:
  define var r as rowid.
  define var v_t as integer initial 0.
  
  run get-rowid1 in h_b_lotes_orden_fabricacion (output r).
  
  find lotes_jugo where rowid(lotes_jugo) = r no-error.
  if available lotes_jugo then
    do:
        assign lotes_jugo.id_contrato_of = ""
               lotes_jugo.id_tipocontrato_of = 0
               lotes_jugo.anio_of = 0.
        
        for each tambores_industria of lotes_jugo.
            assign tambores_industria.id_contrato_of = ""
                   tambores_industria.id_tipocontrato_of = 0
                   tambores_industria.anio_of = 0.   

            v_t = v_t + 1.
            
        end.
        message "Se modifico el Lote de Jugo y los " v_t " tambores." view-as alert-box.
        id_contrato_of:SCREEN-VALUE IN FRAME F-Main = "".
        id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main = "0".
        anio_of:SCREEN-VALUE IN FRAME F-Main = "0".
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 F-Frame-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Transferencia Tambores */
DO:
  DEFINE VAR r AS ROWID.
  DEFINE VAR v_articulo AS INTEGER.
  DEFINE VAR i AS INTEGER.
  DEFINE BUFFER bb_lotes FOR lotes_jugo.
  DEFINE BUFFER bbLoteAnterior FOR lotes_jugo.
  DEFINE VAR v_nromov AS INTEGER.
  DEFINE VAR v_t AS INTEGER.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  
  run get-rowid1 in h_b_lotes_orden_fabricacion (output r).
  
  IF TRIM(id_contrato_of:SCREEN-VALUE IN FRAME F-Main) <> "" THEN DO:

      FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-ERROR.
      IF AVAILABLE lotes_jugo THEN DO:
        ASSIGN iEmp = lotes_jugo.id_empresa
               iSuc = lotes_jugo.id_sucursal
               iTip = lotes_jugo.id_tipotambor
               iNro = lotes_jugo.nromov.
    
        ASSIGN lotes_jugo.id_contrato_of        = id_contrato_of:SCREEN-VALUE IN FRAME F-Main
               lotes_jugo.id_tipocontrato_of    = integer(id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main)
               lotes_jugo.anio_of               = INTEGER(anio_of:SCREEN-VALUE IN FRAME F-Main).
    
        DO i = INTEGER(desde_tambor:SCREEN-VALUE IN FRAME F-Main) TO INTEGER(hasta_tambor:SCREEN-VALUE IN FRAME F-Main).
            FIND FIRST tambores_industria OF lotes_jugo
                                          WHERE tambores_industria.id_tambor = i
                                          NO-ERROR.
            IF AVAILABLE tambores_industria THEN DO:
                ASSIGN tambores_industria.id_contrato_of        = id_contrato_of:SCREEN-VALUE IN FRAME F-Main
                       tambores_industria.id_tipocontrato_of    = integer(id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main)
                       tambores_industria.anio_of               = INTEGER(anio_of:SCREEN-VALUE IN FRAME F-Main).
    
                v_t = v_t + 1.        
            
            END.
            ELSE DO:
                MESSAGE "No se encontro el tambor." VIEW-AS ALERT-BOX.
            END.
        END.
        
        MESSAGE "Se han asociado " v_t " tambores del lote " lotes_jugo.id_lote VIEW-AS ALERT-BOX.
        RUN mi-descriptivos.
        RUN dispatch in h_b_tambores_lotes_jugo_of ('open-query':U).

        DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
        RUN libTamboresIndustria.p PERSISTENT SET hLib.
        RUN mailingAsocTamboresOF IN hLib (iEmp,
                                           iSuc,
                                           iTip,
                                           iNro,
                                           INTEGER(desde_tambor:SCREEN-VALUE IN FRAME F-Main), 
                                           INTEGER(hasta_tambor:SCREEN-VALUE IN FRAME F-Main), 
                                           id_contrato_of:SCREEN-VALUE IN FRAME F-Main).

    
      END.
      ELSE MESSAGE "No hay ningun lote seleccionado" VIEW-AS ALERT-BOX.
  END.
  ELSE MESSAGE "No ha elegido ninguna Orden Fabricacion" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 F-Frame-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Asignacion Completa del Lote */
DO:
  define var r as rowid.
  define var v_articulo as integer.
  define var v_t as integer.
  
  
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  RUN libTamboresIndustria.p PERSISTENT SET hLib.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.

  
  RUN get-rowid1 in h_b_lotes_orden_fabricacion (output r).
  
  IF TRIM(id_contrato_of:SCREEN-VALUE IN FRAME F-Main) <> "" THEN DO:

      FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-ERROR.
      IF AVAILABLE lotes_jugo THEN DO:
        ASSIGN iEmp = lotes_jugo.id_empresa
               iSuc = lotes_jugo.id_sucursal
               iTip = lotes_jugo.id_tipotambor
               iNro = lotes_jugo.nromov.
        
        ASSIGN lotes_jugo.id_contrato_of        = id_contrato_of:SCREEN-VALUE IN FRAME F-Main
               lotes_jugo.id_tipocontrato_of    = integer(id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main)
               lotes_jugo.anio_of               = INTEGER(anio_of:SCREEN-VALUE IN FRAME F-Main).
    
        FOR EACH tambores_industria OF lotes_jugo.
            
                ASSIGN tambores_industria.id_contrato_of        = id_contrato_of:SCREEN-VALUE IN FRAME F-Main
                   tambores_industria.id_tipocontrato_of    = integer(id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main)
                   tambores_industria.anio_of               = INTEGER(anio_of:SCREEN-VALUE IN FRAME F-Main).
    
            v_t = v_t + 1.        
        END.
        
        MESSAGE "Se han modificado todos los tambores(" v_t ") del lote." VIEW-AS ALERT-BOX.
        RUN mi-descriptivos.
        RUN dispatch in h_b_tambores_lotes_jugo_of ('open-query':U).
        
        RUN mailingAsocTamboresOF IN hLib (iEmp,
                                           iSuc,
                                           iTip,
                                           iNro,
                                           1, 
                                           v_t, 
                                           id_contrato_of:SCREEN-VALUE IN FRAME F-Main).

      END.
      ELSE MESSAGE "No hay ningun lote seleccionado" VIEW-AS ALERT-BOX.
  END.
  ELSE MESSAGE "No ha elegido ninguna Orden Fabricacion" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME id_contrato_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL id_contrato_of F-Frame-Win
ON MOUSE-SELECT-DBLCLICK OF id_contrato_of IN FRAME F-Main /* Contract */
DO:
    define var r1 as ROWID.
    define var v_t as integer initial 0.
    
    run wc_contratos.w (output r1).
    FIND contratos WHERE ROWID(contratos) = r1 NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN
    DO:
        id_contrato_of:screen-value = string(contratos.id_contrato).
        id_tipocontrato_of:screen-value = string(contratos.id_tipo_contrato).
        anio_of:screen-value = string(contratos.anio).
        orden_fabricacion:screen-value = string(contratos.orden_fabricacion). 
        
        FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN nombre_cliente:SCREEN-VALUE = clientes.nombre.
    END.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.48 , 91.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.48 , 114.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lotes_orden_fabricacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes_orden_fabricacion ).
       RUN set-position IN h_b_lotes_orden_fabricacion ( 3.86 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_lotes_orden_fabricacion ( 6.71 , 147.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b_tambores_lotes_jugo_of.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_tambores_lotes_jugo_of ).
       RUN set-position IN h_b_tambores_lotes_jugo_of ( 11.24 , 73.00 ) NO-ERROR.
       RUN set-size IN h_b_tambores_lotes_jugo_of ( 6.71 , 74.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_lotes_orden_fabricacion. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_lotes_orden_fabricacion ).

       /* Links to csmartbrowser h_b_tambores_lotes_jugo_of. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_orden_fabricacion , 'Record':U , h_b_tambores_lotes_jugo_of ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-navico ,
             id_contrato_of:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             h_cus-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_lotes_orden_fabricacion ,
             h_cus-misc , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_tambores_lotes_jugo_of ,
             id_contrato_of:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta F-Frame-Win 
PROCEDURE consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_consultas as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_consultas = "" then
    message "No hay consultas disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_consultas,output cresult).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos F-Frame-Win 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer F-Frame-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY id_contrato_of id_tipocontrato_of anio_of orden_fabricacion 
          nombre_cliente desde_tambor hasta_tambor 
      WITH FRAME F-Main.
  ENABLE id_contrato_of id_tipocontrato_of anio_of BUTTON-15 BUTTON-14 
         desde_tambor hasta_tambor BUTTON-1 RECT-16 RECT-2 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta F-Frame-Win 
PROCEDURE get-deshabilitados-paleta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter hprograma as handle no-undo.
define output parameter estados as character no-undo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion F-Frame-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit F-Frame-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
{custom/support/localexit.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {custom/method/ctitulo.i}
  run deshabilita_viewer.
  run habilitar_relacion_viewer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mi-descriptivos F-Frame-Win 
PROCEDURE mi-descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST contratos WHERE contratos.id_contrato = id_contrato_of:SCREEN-VALUE IN FRAME F-Main
                    AND contratos.anio = INTEGER(anio_of:SCREEN-VALUE IN FRAME F-Main)
                    AND contratos.id_tipo_contrato = INTEGER(id_tipocontrato_of:SCREEN-VALUE IN FRAME F-Main) 
                    NO-LOCK NO-ERROR.
IF AVAILABLE contratos THEN DO:
    orden_fabricacion:SCREEN-VALUE IN FRAME F-Main = string(contratos.orden_fabricacion). 
    
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN nombre_cliente:SCREEN-VALUE IN FRAME F-Main = clientes.nombre.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy F-Frame-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create F-Frame-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
MESSAGE "Post Create" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete F-Frame-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update F-Frame-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

/* MESSAGE "Post Update" VIEW-AS ALERT-BOX. */
/*

FIND lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
IF AVAILABLE lotes_jugo THEN
    DO:
        /* MESSAGE "Lote " lotes_jugo.id_lote lotes_jugo.nro_orden_fabricacion VIEW-AS ALERT-BOX. */

        FOR EACH tambores_industria OF lotes_jugo.
            ASSIGN tambores_industria.id_contrato_of     = lotes_jugo.id_contrato_of
                   tambores_industria.id_tipocontrato_of     = lotes_jugo.id_tipocontrato_of
                   tambores_industria.anio_of    = lotes_jugo.anio_of
                   tambores_industria.item_of    = lotes_jugo.item_of
                   tambores_industria.id_envio_of    = lotes_jugo.id_envio_of.
        
        END.    
    END.
  */

alta = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy F-Frame-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create F-Frame-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
alta = true.
MESSAGE "Pre Create" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete F-Frame-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update F-Frame-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */
/* MESSAGE "Pre Update" VIEW-AS ALERT-BOX. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro F-Frame-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "lotes_jugo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

