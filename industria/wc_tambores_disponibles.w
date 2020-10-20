&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

DEFINE TEMP-TABLE tt_tambores
    FIELD id_empresa     AS INTEGER  COLUMN-LABEL "Empresa"
    FIELD id_sucursal     AS INTEGER  COLUMN-LABEL "Sucursal"
    FIELD id_tipotambor     AS INTEGER  COLUMN-LABEL "Tipotambor"
    FIELD nromov     AS INTEGER  COLUMN-LABEL "Nromov"
    FIELD id_lote     AS INTEGER  COLUMN-LABEL "Lote"
    FIELD id_tambor     AS INTEGER  COLUMN-LABEL "Tambor"
    FIELD id_articulo   AS INTEGER  COLUMN-LABEL "Articulo"
    FIELD id_calidad    AS INTEGER  COLUMN-LABEL "Calidad"
    FIELD id_envase     AS INTEGER  COLUMN-LABEL "Envase"
    FIELD kilos_tambor  AS DECIMAL  COLUMN-LABEL "Peso Neto"
    FIELD anio          AS INTEGER  COLUMN-LABEL "Año"
    FIELD elegido       AS LOGICAL  COLUMN-LABEL "Seleccionado" FORMAT "SI/" INITIAL FALSE
    FIELD id_reg        AS ROWID.


/* Local Variable Definitions ---                                       */

DEFINE INPUT PARAMETER p_id_tipotambor AS INTEGER.
DEFINE INPUT PARAMETER p_id_articulo AS INTEGER.
DEFINE INPUT PARAMETER p_id_lote AS INTEGER.
DEFINE INPUT PARAMETER p_anio AS INTEGER.
DEFINE INPUT PARAMETER p_id_sucursal AS INTEGER.
DEFINE INPUT  PARAMETER prRemito AS ROWID      NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_tambores.







/**********EMPIEZA CON PARAMETROS*********/
&SCOPED-DEFINE CON-PARAMETROS YES 
/**********TERMINA CON PARAMETROS*********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_tambores

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt_tambores.id_sucursal tt_tambores.id_lote tt_tambores.anio tt_tambores.id_tambor tt_tambores.id_articulo tt_tambores.id_calidad tt_tambores.kilos_tambor tt_tambores.elegido   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 RUN carga-datos. OPEN QUERY {&SELF-NAME} FOR EACH tt_tambores.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt_tambores
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt_tambores


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 v_desde v_hasta BUTTON-4 BUTTON-2 ~
BUTTON-3 cantidad_elegida aceptar salir bolsas BUTTON-5 RECT-1 RECT-2 ~
RECT-3 
&Scoped-Define DISPLAYED-OBJECTS v_desde v_hasta cantidad_elegida ~
cantidad_tambores FILL-IN-2 bolsas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON aceptar 
     IMAGE-UP FILE "custom/imagen/check":U
     IMAGE-INSENSITIVE FILE "custom/imagen/check":U NO-CONVERT-3D-COLORS
     LABEL "&Aceptar" 
     SIZE 10 BY 1.67 TOOLTIP "Aceptar tarea".

DEFINE BUTTON BUTTON-2 
     LABEL "Elegir Todos" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Liberar Todos" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Aceptar" 
     SIZE 11 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Aceptar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON salir 
     IMAGE-UP FILE "custom/imagen/cross":U
     IMAGE-INSENSITIVE FILE "custom/imagen/cross":U NO-CONVERT-3D-COLORS
     LABEL "&Salir" 
     SIZE 10 BY 1.67 TOOLTIP "Abandonar Tarea".

DEFINE VARIABLE bolsas AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Cantidad Bolsas" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cantidad_elegida AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Tambores Elegidos" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cantidad_tambores AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Total Tambores Disponibles" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "PARA USO EN DESPACHOS DE CASCARA" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE v_desde AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Desde Tambor" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE v_hasta AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 2.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 121 BY 19.52.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt_tambores SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt_tambores.id_sucursal  
    tt_tambores.id_lote
    tt_tambores.anio
    tt_tambores.id_tambor
    tt_tambores.id_articulo
    tt_tambores.id_calidad
    tt_tambores.kilos_tambor
    tt_tambores.elegido
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 117 BY 16.43 ROW-HEIGHT-CHARS .52 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.48 COL 4
     v_desde AT ROW 18.14 COL 17 COLON-ALIGNED
     v_hasta AT ROW 18.14 COL 32 COLON-ALIGNED
     BUTTON-4 AT ROW 18.14 COL 43
     BUTTON-2 AT ROW 18.14 COL 59
     BUTTON-3 AT ROW 18.14 COL 76
     cantidad_elegida AT ROW 18.14 COL 111 COLON-ALIGNED
     cantidad_tambores AT ROW 19.33 COL 111 COLON-ALIGNED
     FILL-IN-2 AT ROW 20.76 COL 1 COLON-ALIGNED NO-LABEL
     aceptar AT ROW 21.48 COL 100
     salir AT ROW 21.48 COL 112
     bolsas AT ROW 21.95 COL 18 COLON-ALIGNED
     BUTTON-5 AT ROW 21.95 COL 34
     RECT-1 AT ROW 21.24 COL 97
     RECT-2 AT ROW 1 COL 2
     RECT-3 AT ROW 20.52 COL 2
     SPACE(67.39) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Consultas - Gessi - Grupo Sauken".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/contenedor.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cantidad_tambores IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
RUN carga-datos.
OPEN QUERY {&SELF-NAME} FOR EACH tt_tambores.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" D-Dialog _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Consultas - Gessi - Grupo Sauken */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aceptar D-Dialog
ON CHOOSE OF aceptar IN FRAME D-Dialog /* Aceptar */
DO:
  APPLY "END-ERROR":U TO frame d-dialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON LEAVE OF BROWSE-2 IN FRAME D-Dialog
DO:
  /*IF NOT tt_tambores.elegido THEN DO:
        tt_tambores.id_lote:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.anio:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_articulo:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_calidad:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.kilos_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.elegido:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
  END.
  ELSE DO: 
      tt_tambores.id_lote:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.anio:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_articulo:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_calidad:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.kilos_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.elegido:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON MOUSE-SELECT-CLICK OF BROWSE-2 IN FRAME D-Dialog
DO:
  IF tt_tambores.elegido THEN DO:
        ASSIGN tt_tambores.elegido = FALSE.
       /* tt_tambores.id_lote:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.anio:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_articulo:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.id_calidad:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.kilos_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        tt_tambores.elegido:BGCOLOR IN BROWSE {&BROWSE-NAME} = 15.
        MESSAGE " se tendrian que cambiar a blanco" VIEW-AS ALERT-BOX.
        */
  END.
  ELSE DO: 
      ASSIGN tt_tambores.elegido = TRUE.
      /*tt_tambores.id_lote:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.anio:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_articulo:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.id_calidad:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.kilos_tambor:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      tt_tambores.elegido:BGCOLOR IN BROWSE {&BROWSE-NAME} = 14.
      MESSAGE " se tendrian que cambiar a amarillo" VIEW-AS ALERT-BOX.
      */
  END.
      
  BROWSE-2:REFRESH().

  RUN cantidad_tambores_elegidos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON SELECTION OF BROWSE-2 IN FRAME D-Dialog
DO:
  MESSAGE "Selection" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON VALUE-CHANGED OF BROWSE-2 IN FRAME D-Dialog
DO:
    /*
  IF tt_tambores.elegido:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "SI" THEN DO:
      tt_tambores.elegido:BGCOLOR IN BROWSE {&BROWSE-NAME} = 5.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 D-Dialog
ON CHOOSE OF BUTTON-2 IN FRAME D-Dialog /* Elegir Todos */
DO:
  FOR EACH tt_tambores.
      ASSIGN tt_tambores.elegido = TRUE.
  END.
  BROWSE-2:REFRESH().
  RUN cantidad_tambores_elegidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 D-Dialog
ON CHOOSE OF BUTTON-3 IN FRAME D-Dialog /* Liberar Todos */
DO:
  FOR EACH tt_tambores.
      ASSIGN tt_tambores.elegido = FALSE.
  END.
  BROWSE-2:REFRESH().
  RUN cantidad_tambores_elegidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 D-Dialog
ON CHOOSE OF BUTTON-4 IN FRAME D-Dialog /* Aceptar */
DO:
  FOR EACH tt_tambores WHERE id_tambor >= INTEGER(v_desde:SCREEN-VALUE IN FRAME D-Dialog)
                         AND id_tambor <= INTEGER(v_hasta:SCREEN-VALUE IN FRAME D-Dialog).
      ASSIGN tt_tambores.elegido = TRUE.
  END.
  BROWSE-2:REFRESH().
  RUN cantidad_tambores_elegidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 D-Dialog
ON CHOOSE OF BUTTON-5 IN FRAME D-Dialog /* Aceptar */
DO:
    DEFINE VAR i AS INTEGER.
    DEFINE VAR v_bolsas AS INTEGER.

    v_bolsas = INTEGER(bolsas:SCREEN-VALUE IN FRAME D-Dialog).

    DO i = 1 TO v_bolsas.
        FIND FIRST tt_tambores WHERE tt_tambores.elegido = FALSE NO-ERROR.
        IF AVAILABLE tt_tambores THEN DO:
            ASSIGN tt_tambores.elegido = TRUE.
        END.
        ELSE MESSAGE "Error, esta tratando de asignar bolsas que no existen"
                    VIEW-AS ALERT-BOX.
        
    END.
  
  BROWSE-2:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL salir D-Dialog
ON CHOOSE OF salir IN FRAME D-Dialog /* Salir */
DO:
  FOR EACH tt_tambores.
      DELETE tt_tambores.
  END.
  APPLY "END-ERROR":U TO frame d-dialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-busca-rowid D-Dialog 
PROCEDURE adm-busca-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

apply "close" to this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros D-Dialog 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cantidad_tambores_elegidos D-Dialog 
PROCEDURE cantidad_tambores_elegidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR I AS INTEGER.

i = 0.
FOR EACH tt_tambores WHERE tt_tambores.elegido.
    i = i + 1.
END.

cantidad_elegida:SCREEN-VALUE IN FRAME D-Dialog = STRING(i).

i = 0.
FOR EACH tt_tambores WHERE NOT tt_tambores.elegido.
    i = i + 1.
END.

cantidad_tambores:SCREEN-VALUE IN FRAME D-Dialog = STRING(i).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-datos D-Dialog 
PROCEDURE carga-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR p_id_emp AS INTEGER.
DEFINE VAR p_id_suc AS INTEGER.
DEFINE VAR p_id_tip AS INTEGER.
DEFINE VAR p_nro AS INTEGER.
DEFINE VAR i AS INTEGER.

DEFINE VARIABLE iOE     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iItemOE AS INTEGER    NO-UNDO.

FOR EACH tt_tambores.
    DELETE tt_tambores.
END.


IF prRemito <> ? THEN DO:
  FIND FIRST remitos WHERE ROWID(remitos) = prRemito NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    IF remitos.mercado = 1 THEN  /*verificar cual es el valor para mercado externo*/
    ASSIGN iOE     = remitos.id_orden_entrega
           iItemOE = remitos.ITEM_oe.
  END.
END.



FIND FIRST tipostambor WHERE tipostambor.id_tipotambor = p_id_tipotambor NO-LOCK NO-ERROR.
IF AVAILABLE tipostambor THEN DO:
    IF tipostambor.tabla = "tambores_industria" THEN DO:
    
        FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor          = p_id_tipotambor
                                      AND tambores_industria.id_articulo            = p_id_articulo
                                      /*AND tambores_industria.id_lote                = 0
                                      AND tambores_industria.anio                   = p_anio */
                                      AND tambores_industria.id_sucursal_ubicacion  = p_id_sucursal
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND tambores_industria.id_estado              <> 10
                                      AND tambores_industria.id_sucursal_remito     <> p_id_sucursal
                                      AND IF iOE <> 0 THEN tambores_industria.id_orden_entrega = iOE ELSE TRUE
                                      AND IF iItemOE <> 0 THEN tambores_industria.item_oe = iItemOE ELSE TRUE
                                      NO-LOCK
                                      BY tambores_industria.id_tambor.
            CREATE tt_tambores.
            BUFFER-COPY tambores_industria TO tt_tambores.
            ASSIGN tt_tambores.id_reg = ROWID(tambores_industria).                              
        END.
    END.
    ELSE DO:
        IF tipostambor.tabla = "produccion_jugo" THEN DO:
            RUN p_busca_tabla_cabecera_prod_jugo.p (INPUT p_id_tipotambor,
                                          INPUT p_id_articulo,
                                          INPUT p_id_lote,
                                          INPUT p_anio,
                                          INPUT p_id_sucursal,
                                          OUTPUT p_id_emp,
                                          OUTPUT p_id_suc,
                                          OUTPUT p_id_tip,
                                          OUTPUT p_nro) tipostambor.tabla.
        END.
        ELSE DO:
            RUN p_busca_tabla_cabeceraf.p (INPUT p_id_tipotambor,
                                          INPUT p_id_articulo,
                                          INPUT p_id_lote,
                                          INPUT p_anio,
                                          INPUT p_id_sucursal, 
                                          OUTPUT p_id_emp,
                                          OUTPUT p_id_suc,
                                          OUTPUT p_id_tip,
                                          OUTPUT p_nro) tipostambor.tabla.
        END.
        
        IF p_id_tipotambor = 11 AND p_nro <> 0 THEN DO:
            FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino     = p_id_emp
                                          AND tambores_industria.id_sucursal_destino    = p_id_suc
                                          AND tambores_industria.id_tipotambor_destino  = p_id_tip
                                          AND tambores_industria.nromov_destino         = p_nro
                                          AND tambores_industria.id_sucursal_ubicacion  = p_id_sucursal
                                          AND tambores_industria.id_locacion_ubicacion  = 4
                                          AND tambores_industria.id_estado              <> 10
                                          AND tambores_industria.id_sucursal_remito     <> p_id_sucursal
                                          NO-LOCK
                                          BY tambores_industria.id_tambor.
                CREATE tt_tambores.
                BUFFER-COPY tambores_industria TO tt_tambores.
                ASSIGN tt_tambores.id_reg = ROWID(tambores_industria).
            END.
        END.
        ELSE DO:
            FOR EACH tambores_industria WHERE tambores_industria.id_empresa             = p_id_emp 
                                          AND tambores_industria.id_sucursal            = p_id_suc 
                                          AND tambores_industria.id_tipotambor          = p_id_tip 
                                          AND tambores_industria.nromov                 = p_nro    
                                          AND tambores_industria.id_sucursal_ubicacion  = p_id_sucursal
                                          AND tambores_industria.id_locacion_ubicacion  = 4
                                          AND tambores_industria.id_estado              <> 10
                                          AND tambores_industria.id_sucursal_remito     <> p_id_sucursal
                                          NO-LOCK
                                          BY tambores_industria.id_tambor.
                CREATE tt_tambores.
                BUFFER-COPY tambores_industria TO tt_tambores.
                ASSIGN tt_tambores.id_reg = ROWID(tambores_industria).
                i = i + 1.
            END.
        END.
    END.
END.
cantidad_tambores:SCREEN-VALUE IN FRAME D-Dialog = STRING(i).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-parametros D-Dialog 
PROCEDURE dame-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pp_id_tipotambor AS INTEGER.
DEFINE OUTPUT PARAMETER pp_id_articulo AS INTEGER.
DEFINE OUTPUT PARAMETER pp_id_lote AS INTEGER.
DEFINE OUTPUT PARAMETER pp_anio AS INTEGER.
DEFINE OUTPUT PARAMETER pp_id_sucursal AS INTEGER.

pp_id_tipotambor = p_id_tipotambor.
pp_id_articulo   = p_id_articulo.
pp_id_lote       = p_id_lote.
pp_anio          = p_anio.
pp_id_sucursal   = p_id_sucursal.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-valor D-Dialog 
PROCEDURE devuelve-valor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter cresult as rowid no-undo.
define var c as character no-undo.
define var h as handle no-undo.
define var r as rowid no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure , input 'CONSULTA-TARGET' , output c).
h=widget-handle(c).

if valid-handle(h) then
do:
    run get-attribute in h (input 'TYPE':U).
    if return-value = 'csmartbrowser' then
    do:
        run devuelve-rowid in h (output r).
        cresult = r.
    end.
    else
        cresult = ?.    
end.        

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY v_desde v_hasta cantidad_elegida cantidad_tambores FILL-IN-2 bolsas 
      WITH FRAME D-Dialog.
  ENABLE BROWSE-2 v_desde v_hasta BUTTON-4 BUTTON-2 BUTTON-3 cantidad_elegida 
         aceptar salir bolsas BUTTON-5 RECT-1 RECT-2 RECT-3 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry D-Dialog 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects D-Dialog 
PROCEDURE local-create-objects :
define var cresult as character no-undo.
define var h as handle no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Links to csmartbrowser h_b-table. */
  run get-link-handle in adm-broker-hdl ( input this-procedure,
                                          input 'Consulta-TARGET',
                                          output cresult).
  h=widget-handle(cresult).
  if valid-handle(h) then
    return.  
  
  run get-link-handle in adm-broker-hdl ( input this-procedure,
                                          input 'CONTAINER-TARGET',
                                          output cresult).
  h=widget-handle(cresult).
  if valid-handle(h) then
  do:                                        
    run get-attribute in h ('TYPE').
    if return-value = "csmartbrowser" then
    do:
        run add-link in adm-broker-hdl (input this-procedure, 'Consulta', h).
    end.
  end.  
    &IF DEFINED(CON-PARAMETROS) &THEN
        run asigna-parametros.
        run contenedor.
        run notify ('open-query').
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit D-Dialog 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt_tambores"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

