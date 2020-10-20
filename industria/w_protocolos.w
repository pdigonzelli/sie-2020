&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
DEF STREAM DataStream.
DEF VAR pv_chWord AS COM-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 ~
BUTTON-36 BUTTON-35 BUTTON-34 BUTTON-32 BUTTON-33 RECT-22 RECT-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_inspecciones_lote_jugo_proto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_items_protocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_lote_aceite_protocolo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_lote_jugo_protocolo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_protocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cfolder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_protocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_protocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_protocolos_1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_protocolo_cascara AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Chemical Characteristic" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Lotes Jugo" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Lotes Aceite" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-32 
     LABEL "Mail c/Prot" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-33 
     LABEL "Mail a BsAs" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-34 
     LABEL "Prot. Cascara" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-35 
     LABEL "Prot. Jugo" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-36 
     LABEL "Prot. Aceite" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Liberar Protocolo" 
     SIZE 17 BY 1.14.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 8.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.24 COL 81
     BUTTON-2 AT ROW 1.24 COL 107
     BUTTON-3 AT ROW 1.24 COL 122
     BUTTON-4 AT ROW 1.24 COL 137
     BUTTON-36 AT ROW 2.43 COL 81
     BUTTON-35 AT ROW 2.43 COL 94
     BUTTON-34 AT ROW 2.43 COL 107
     BUTTON-32 AT ROW 2.43 COL 122
     BUTTON-33 AT ROW 2.43 COL 137
     RECT-22 AT ROW 3.62 COL 59
     RECT-4 AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 23.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Protocolos"
         HEIGHT             = 23.71
         WIDTH              = 155
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 203.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" W-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Protocolos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Protocolos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Chemical Characteristic */
DO:
  DEFINE VAR r AS ROWID.
  DEFINE VAR r_prot AS ROWID.
  DEFINE VAR v_art AS INTEGER INITIAL 0.
  DEFINE VAR v_valor AS DECIMAL.

  RUN GET-rowid1 IN h_b_lote_jugo_protocolo (OUTPUT r).
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
      v_art = lotes_jugo.id_articulo.
  END.
  ELSE DO:
      RUN GET-rowid1 IN h_b_lote_aceite_protocolo (OUTPUT r).
      FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_aceite THEN DO:
          v_art = lotes_aceite.id_articulo.
      END.
      ELSE DO: /* NO ECONTRO EL LOTE DE aceite */
         MESSAGE "No eligio ningun Lote!." VIEW-AS ALERT-BOX. 
      END.
  END.
  IF v_art > 0 THEN DO:
      RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).
      FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
      IF AVAILABLE protocolos THEN DO:
        FOR EACH r_caracteristicas_articulos WHERE r_caracteristicas_articulo.id_articulo = v_art
                                               AND r_caracteristicas_articulo.id_tipo_protocolo = 
                                                   protocolos.id_tipo_protocolo NO-LOCK.
            v_valor = 0.
            FIND FIRST caracteristicas_quimicas OF r_caracteristicas_articulos NO-LOCK NO-ERROR.
            IF AVAILABLE caracteristicas_quimicas THEN DO:
                IF caracteristicas_quimicas.programa <> "" THEN DO:
                    RUN VALUE(caracteristicas_quimicas.programa) (INPUT ROWID(protocolos),
                                                                  INPUT caracteristicas_quimicas.id_caracteristica,
                                                                  OUTPUT v_valor).

                    FIND FIRST items_protocolos WHERE items_protocolos.id_protocolo = protocolos.id_protocolo
                                                  AND items_protocolos.anio         = protocolos.anio
                                                  AND items_protocolos.id_articulo  = protocolos.id_articulo
                                                  AND items_protocolos.id_tipo_protocolo = 
                                                      protocolos.id_tipo_protocolo
                                                  AND items_protocolos.id_caracteristica = 
                                                      r_caracteristicas_articulos.id_caracteristica 
                                                NO-ERROR.
                    IF AVAILABLE items_protocolos THEN DO:
                        DELETE items_protocolos.
                    END.
        
                    CREATE items_protocolos.
                    ASSIGN items_protocolos.id_protocolo       = protocolos.id_protocolo
                           items_protocolos.anio               = protocolos.anio
                           items_protocolos.id_articulo        = protocolos.id_articulo
                           items_protocolos.id_tipo_protocolo  = protocolos.id_tipo_protocolo
                           items_protocolos.id_caracteristica  = r_caracteristicas_articulos.id_caracteristica
                           items_protocolos.orden              = r_caracteristicas_articulos.orden
                           items_protocolos.valor_caracter     = STRING(v_valor)
                           items_protocolos.c_usuario          = USERID("userdb")
                           items_protocolos.c_fecha            = TODAY
                           items_protocolos.c_hora             = STRING(TIME,"HH:MM:SS").
                END.                                              
            END.
        END.
      END.
      v_art = 0.
  END.
  
  /*  RUN GET-rowid1 IN h_b_lote_jugo_protocolo (OUTPUT r).
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
      v_art = lotes_jugo.id_articulo.
  END.
  IF v_art > 0 THEN DO:
      RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).
      FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
      IF AVAILABLE protocolos THEN DO:
        FOR EACH r_caracteristicas_articulos WHERE r_caracteristicas_articulo.id_articulo = v_art
                                               AND r_caracteristicas_articulo.id_tipo_protocolo = 
                                                   protocolos.id_tipo_protocolo NO-LOCK.
            v_valor = 0.
            FIND FIRST caracteristicas_quimicas OF r_caracteristicas_articulos NO-LOCK NO-ERROR.
            IF AVAILABLE caracteristicas_quimicas THEN DO:
                IF caracteristicas_quimicas.programa <> "" THEN DO:
                    RUN VALUE(caracteristicas_quimicas.programa) (INPUT ROWID(protocolos),
                                                                  INPUT caracteristicas_quimicas.id_caracteristica,
                                                                  OUTPUT v_valor).
                END.                                              
            END.
            
            FIND FIRST items_protocolos WHERE items_protocolos.id_protocolo = protocolos.id_protocolo
                                          AND items_protocolos.id_tipo_protocolo = 
                                              protocolos.id_tipo_protocolo
                                          AND items_protocolos.id_caracteristica = 
                                              r_caracteristicas_articulos.id_caracteristica 
                                        NO-ERROR.
            IF AVAILABLE items_protocolos THEN DO:
                DELETE items_protocolos.
            END.

            CREATE items_protocolos.
            ASSIGN items_protocolos.id_protocolo       = protocolos.id_protocolo
                   items_protocolos.id_tipo_protocolo  = protocolos.id_tipo_protocolo
                   items_protocolos.id_caracteristica  = r_caracteristicas_articulos.id_caracteristica
                   items_protocolos.orden              = r_caracteristicas_articulos.orden
                   items_protocolos.valor_caracter     = STRING(v_valor)
                   items_protocolos.c_usuario          = USERID("userdb")
                   items_protocolos.c_fecha            = TODAY
                   items_protocolos.c_hora             = STRING(TIME,"HH:MM:SS").
            RUN dispatch IN h_b_items_protocolos ('open-query':U).
        END.
      END.
      v_art = 0.
  END.
  RUN GET-rowid1 IN h_b_lote_aceite_protocolo (OUTPUT r).
  FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_aceite THEN DO:
      v_art = lotes_aceite.id_articulo.
  END.
  ELSE DO: /* NO ECONTRO EL LOTE DE JUGO */
     MESSAGE "No eligio ningun Lote!." VIEW-AS ALERT-BOX. 
  END.
  IF v_art > 0 THEN DO:
      RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).
      FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
      IF AVAILABLE protocolos THEN DO:
        FOR EACH r_caracteristicas_articulos WHERE r_caracteristicas_articulo.id_articulo = v_art
                                               AND r_caracteristicas_articulo.id_tipo_protocolo = 
                                                   protocolos.id_tipo_protocolo NO-LOCK.
            v_valor = 0.
            FIND FIRST caracteristicas_quimicas OF r_caracteristicas_articulos NO-LOCK NO-ERROR.
            IF AVAILABLE caracteristicas_quimicas THEN DO:
                IF caracteristicas_quimicas.programa <> "" THEN DO:
                    RUN VALUE(caracteristicas_quimicas.programa) (INPUT ROWID(protocolos),
                                                                  INPUT caracteristicas_quimicas.id_caracteristica,
                                                                  OUTPUT v_valor).
                END.                                              
            END.
            
            FIND FIRST items_protocolos WHERE items_protocolos.id_protocolo = protocolos.id_protocolo
                                          AND items_protocolos.id_tipo_protocolo = 
                                              protocolos.id_tipo_protocolo
                                          AND items_protocolos.id_caracteristica = 
                                              r_caracteristicas_articulos.id_caracteristica 
                                        NO-ERROR.
            IF AVAILABLE items_protocolos THEN DO:
                DELETE items_protocolos.
            END.

            CREATE items_protocolos.
            ASSIGN items_protocolos.id_protocolo       = protocolos.id_protocolo
                   items_protocolos.id_tipo_protocolo  = protocolos.id_tipo_protocolo
                   items_protocolos.id_caracteristica  = r_caracteristicas_articulos.id_caracteristica
                   items_protocolos.orden              = r_caracteristicas_articulos.orden
                   items_protocolos.valor_caracter     = STRING(v_valor)
                   items_protocolos.c_usuario          = USERID("userdb")
                   items_protocolos.c_fecha            = TODAY
                   items_protocolos.c_hora             = STRING(TIME,"HH:MM:SS").
            RUN dispatch IN h_b_items_protocolos ('open-query':U).
        END.
      END.
      v_art = 0.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Lotes Jugo */
DO:
  DEFINE VAR r_proto AS ROWID.
  DEFINE VAR r_lote AS ROWID.
  DEFINE VAR v_tambores AS INTEGER.
  DEFINE VAR v_neto AS DECIMAL.
  DEFINE VAR v_bruto AS DECIMAL.
  DEFINE VAR v_gall AS DECIMAL.
  DEFINE VAR v_valor AS DECIMAL.

  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dLit    AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  RUN GET-rowid1 IN h_b_protocolos (OUTPUT r_proto).
  FIND FIRST protocolos WHERE ROWID(protocolos) = r_proto NO-ERROR.
  IF AVAILABLE protocolos THEN DO:
    IF protocolos.id_empresa = 0 THEN DO:
        RUN wc_sel_lotes_jugo_protocolos.w (INPUT protocolos.id_articulo, OUTPUT r_lote).
        FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r_lote NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_jugo THEN DO:
            v_tambores = 0.
            FIND envases_prod OF lotes_jugo NO-LOCK NO-ERROR.
            IF lotes_jugo.id_envase = 527 THEN DO:
                v_neto  = lotes_jugo.peso_neto.
                v_bruto = lotes_jugo.peso_neto + 1.05.                    
            END.
            ELSE DO:
                v_neto  = lotes_jugo.peso_neto.
                v_bruto = lotes_jugo.peso_neto + envases_prod.tara.
            END.
            FOR EACH tambores_industria OF lotes_jugo.
                v_tambores = v_tambores + 1.
            END.
            
            /*calculo de galones by facundo 02/06/2006*/
            FIND LAST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
            dLit   = v_neto / DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLibTam, inspecciones_lote.bx_correg).
            v_gall = ROUND(dLit / 3.785, 2).
            v_gall = DYNAMIC-FUNCTION('getGalonesLote' IN hLibTam, lotes_jugo.id_empresa,
                                                                   lotes_jugo.id_sucursal,
                                                                   lotes_jugo.id_tipotambor,
                                                                   lotes_jugo.nromov).
            
            ASSIGN protocolos.id_empresa        = lotes_jugo.id_empresa
                   protocolos.id_sucursal       = lotes_jugo.id_sucursal
                   protocolos.id_tipotambor     = lotes_jugo.id_tipotambor
                   protocolos.nromov            = lotes_jugo.nromov
                   protocolos.cantidad_tambores = v_tambores
                   protocolos.desde_tambor      = 1
                   protocolos.hasta_tambor      = v_tambores
                   protocolos.peso_neto         = v_neto
                   protocolos.peso_bruto        = v_bruto
                   protocolos.galones_tambores  = v_gall.

            RUN dispatch IN h_b_lote_jugo_protocolo ('open-query':U).
                          
            FOR EACH r_caracteristicas_articulos WHERE r_caracteristicas_articulo.id_articulo = 
                                                       lotes_jugo.id_articulo
                                                   AND r_caracteristicas_articulo.id_tipo_protocolo = 
                                                       protocolos.id_tipo_protocolo NO-LOCK.
                v_valor = 0.
                FIND FIRST caracteristicas_quimicas OF r_caracteristicas_articulos NO-LOCK NO-ERROR.
                IF AVAILABLE caracteristicas_quimicas THEN DO:
                    IF caracteristicas_quimicas.programa <> "" THEN DO:
                        RUN VALUE(caracteristicas_quimicas.programa) (INPUT ROWID(protocolos),
                                                                      INPUT caracteristicas_quimicas.id_caracteristica,
                                                                      OUTPUT v_valor).
                        
                        FIND FIRST items_protocolos WHERE items_protocolos.id_protocolo = protocolos.id_protocolo
                                                      AND items_protocolos.anio         = protocolos.anio
                                                      AND items_protocolos.id_articulo  = protocolos.id_articulo
                                                      AND items_protocolos.id_caracteristica = r_caracteristicas_articulos.id_caracteristica 
                                                      NO-ERROR.
                        IF AVAILABLE items_protocolos THEN DO:
                            ASSIGN items_protocolos.valor_caracter     = STRING(v_valor).
                        END.
                        ELSE DO:
                            CREATE items_protocolos.
                            ASSIGN items_protocolos.id_protocolo       = protocolos.id_protocolo
                                   items_protocolos.anio               = protocolos.anio
                                   items_protocolos.id_articulo        = protocolos.id_articulo
                                   items_protocolos.id_tipo_protocolo  = protocolos.id_tipo_protocolo
                                   items_protocolos.id_caracteristica  = r_caracteristicas_articulos.id_caracteristica
                                   items_protocolos.orden              = r_caracteristicas_articulos.orden
                                   items_protocolos.valor_caracter     = STRING(v_valor)
                                   items_protocolos.c_usuario          = USERID("userdb")
                                   items_protocolos.c_fecha            = TODAY
                                   items_protocolos.c_hora             = STRING(TIME,"HH:MM:SS").
                        END.
                    END.
                END.
            END.
        END. 
    END.
    ELSE DO:
        MESSAGE "El protocolo " protocolos.id_protocolo " ya tiene un lote asociado" 
            VIEW-AS ALERT-BOX.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Lotes Aceite */
DO:
  DEFINE VAR r_proto AS ROWID.
  DEFINE VAR r_lote AS ROWID.
 
  RUN GET-rowid1 IN h_b_protocolos (OUTPUT r_proto).
  FIND FIRST protocolos WHERE ROWID(protocolos) = r_proto NO-ERROR.
  IF AVAILABLE protocolos THEN DO:
    IF protocolos.id_empresa = 0 THEN DO:
        RUN wc_sel_lotes_aceite_protocolos.w (INPUT protocolos.id_articulo, OUTPUT r_lote).
        FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r_lote NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_aceite THEN DO:
            
            ASSIGN protocolos.id_empresa    = lotes_aceite.id_empresa
                   protocolos.id_sucursal   = lotes_aceite.id_sucursal
                   protocolos.id_tipotambor = lotes_aceite.id_tipotambor
                   protocolos.nromov        = lotes_aceite.nromov.
            RUN dispatch IN h_b_lote_aceite_protocolo ('open-query':U).
        END.
    END.
    ELSE DO:
        MESSAGE "El protocolo " protocolos.id_protocolo " ya tiene un lote asociado" 
            VIEW-AS ALERT-BOX.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 W-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-Main /* Mail c/Prot */
DO:
DEFINE VAR r_prot AS ROWID.
DEFINE VAR r_items_muestras AS ROWID.
DEFINE VAR h_con AS HANDLE.

RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
    IF AVAILABLE protocolos THEN DO:
        FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa
                                        AND tambores_industria.id_sucursal = protocolos.id_sucursal
                                        AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                        AND tambores_industria.nromov = protocolos.nromov
                                        NO-LOCK NO-ERROR.
        IF AVAILABLE tambores_industria THEN
            {i_mail_destinatario_protocolos.i}
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 W-Win
ON CHOOSE OF BUTTON-33 IN FRAME F-Main /* Mail a BsAs */
DO:
    DEFINE VAR r as rowid.
    define var v_subject as char.
    define var v_body as char.
    define var v_usuarios as char.
    
    RUN get-rowid1 in h_b_protocolos (output r).
    
    FOR EACH usuarios_listas WHERE usuarios_listas.id_lista = 9 /* Aviso de creacion de protocolos */
                             NO-LOCK.
        v_usuarios = v_usuarios + "," + usuarios_listas.email.            
    END.
                
    IF v_usuarios <> "" THEN v_usuarios = SUBSTRING(v_usuarios,2,LENGTH(v_usuarios) - 1).
        
    FIND FIRST protocolos WHERE ROWID(protocolos) = r NO-LOCK NO-ERROR.
    IF AVAILABLE protocolos THEN DO:
        v_subject = "Protocolo  " + STRING(protocolos.id_protocolo,"9999") + "/" + STRING(protocolos.anio,"99").
        FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa
                                        AND tambores_industria.id_sucursal = protocolos.id_sucursal
                                        AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                        AND tambores_industria.nromov = protocolos.nromov
                                        NO-LOCK.
        IF AVAILABLE tambores_industria THEN DO:
            v_body = "Se ha creado el protocolo " + STRING(protocolos.id_protocolo,"9999") + "/" + STRING(protocolos.anio,"99") +
                     " del lote " + STRING(tambores_industria.id_lote,"9999") + "/" + SUBSTRING(STRING(tambores_industria.anio),3,2).
                                
            IF v_usuarios <> "" THEN DO:
                RUN SendMail.p(INPUT "",                       /* SIEMPRE TIENE QUE IR */
                               INPUT 2,                                                   /* PRIORIDAD */
                               INPUT v_subject,                                           /* SUBJECT */
                               INPUT v_body,                                              /* BODY     */
                               INPUT v_usuarios,                                          /* DEST. SEP COMAS */
                               INPUT ""                                      /* ARCHIVOS ATTACHED SEP POR COMAS */
                              ).
            END.
        
                               
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-34 W-Win
ON CHOOSE OF BUTTON-34 IN FRAME F-Main /* Prot. Cascara */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_cascara.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_cascara.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_cascaraHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_humedad AS CHAR.
DEFINE VAR v_azucar AS CHAR.
DEFINE VAR v_color AS CHAR.

RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST envases_muestras OF protocolos NO-LOCK NO-ERROR.

    FOR EACH items_protocolos OF protocolos NO-LOCK.
        CASE items_protocolos.id_caracteristica:
            WHEN 31 THEN DO:
                /* HUMEDAD */
                v_humedad = general.items_protocolos.valor_caracter.
            END.
            WHEN 32 THEN DO:
                /* AZUCAR */
                v_azucar = general.items_protocolos.valor_caracter.
            END.
            WHEN 33 THEN DO:
                /* COLOR */
                v_color = general.items_protocolos.valor_caracter.
            END.
        END CASE.
    END.
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        STRING(protocolos.id_protocolo,">999")
        STRING(protocolos.anio,">>99")
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        STRING(protocolos.lote,">>99")
        STRING(protocolos.anio_lote,">>99")
        IF protocolos.tipo_cascara THEN "STANDARD" ELSE "FINE"
        IF AVAILABLE envases_muestras THEN UPPER(envases_muestras.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        v_humedad
        v_azucar
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        UPPER(v_color).
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                   /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                   /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                       /* Main Document */
                      lv_cDataFile,                                                       /* File that holds all the data */
                      lv_cMailMergeFile,                                                  /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                                /* Automatically Print New Mail Merge Document */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 W-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-Main /* Prot. Jugo */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_jugo.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_jugo.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_jugoHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_body AS CHAR.
DEFINE VAR v_smell AS CHAR.
DEFINE VAR v_color AS CHAR.
DEFINE VAR v_caract AS CHAR.
DEFINE VAR v_valores AS CHAR.
DEFINE VAR v_code AS CHAR.
DEFINE VAR v_valor_code AS CHAR.
DEFINE VAR v_galones AS CHAR.
DEFINE VAR v_valor_galones AS CHAR.


RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST contratos WHERE contratos.id_contrato        = tambores_industria.id_contrato_of
                           AND contratos.id_tipo_contrato   = tambores_industria.id_tipocontrato_of
                           AND contratos.anio               = tambores_industria.anio_of
                         NO-LOCK NO-ERROR.
    FIND FIRST colores_prot OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST cuerpo_prot WHERE cuerpo_prot.id_cuerpo = protocolos.id_body NO-LOCK NO-ERROR.
    FIND FIRST aromas_sabores_prot WHERE aromas_sabores_prot.id_aroma = protocolos.id_smell NO-LOCK NO-ERROR.
    v_caract = "".
    FOR EACH items_protocolos OF protocolos NO-LOCK.
        FIND FIRST caracteristicas_quimicas OF items_protocolos NO-LOCK NO-ERROR.
        v_caract = v_caract + caracteristicas_quimicas.descripcion + CHR(10).
        v_valores = v_valores + general.items_protocolos.valor_caracter + CHR(10).
    END.
    IF protocolos.con_code THEN DO:
        v_code = "CODE:".
        v_valor_code = protocolos.CODE.
    END.
    IF protocolos.con_pyg THEN DO:
        v_code = "P&G RMS Nro:".
        v_valor_code = protocolos.CODE.
    END.
    IF protocolos.con_galones THEN DO:
        v_galones = "Gallon per drum".
        v_valor_galones = STRING(protocolos.galones_tambores).
    END.
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        IF AVAILABLE lotes_jugo THEN STRING(lotes_jugo.id_lote,">999") + "/" + SUBSTRING(STRING(lotes_jugo.anio,">>99"),3,2) ELSE "NONE"
        IF AVAILABLE lotes_jugo THEN STRING(lotes_jugo.fecha) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE calidades THEN UPPER(calidades.descripcion) ELSE "NONE"
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        v_code
        v_valor_code
        STRING(protocolos.id_protocolo,">999") + "/" + STRING(protocolos.anio,">>99")
        IF AVAILABLE contratos THEN STRING(contratos.orden_fabricacion,">999") + "/" + STRING(SUBSTRING(STRING(contratos.anio),3,2),"99") ELSE "NONE"
        IF AVAILABLE colores_prot THEN colores_prot.descripcion ELSE "NONE"
        IF AVAILABLE cuerpo_prot THEN cuerpo_prot.descripcion ELSE "NONE"
        IF AVAILABLE aromas_sabores_prot THEN aromas_sabores_prot.descripcion ELSE "NONE"
        v_caract
        v_valores
        protocolos.cantidad_tambores
        STRING(industria.protocolos.desde_tambor,"999") + " to " + STRING(industria.protocolos.hasta_tambor,"999")
        general.protocolos.peso_neto
        general.protocolos.peso_bruto_tambor
        v_galones
        v_valor_galones
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        .
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                  /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                  /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                  /* Main Document */
                      lv_cDataFile,                                                  /* File that holds all the data */
                      lv_cMailMergeFile,                                             /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                    /* Automatically Print New Mail Merge Document */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-36 W-Win
ON CHOOSE OF BUTTON-36 IN FRAME F-Main /* Prot. Aceite */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_aceite.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_aceite.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_aceiteHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_body AS CHAR.
DEFINE VAR v_smell AS CHAR.
DEFINE VAR v_color AS CHAR.
DEFINE VAR v_caract AS CHAR.
DEFINE VAR v_valores AS CHAR.
DEFINE VAR v_code AS CHAR.
DEFINE VAR v_valor_code AS CHAR.
DEFINE VAR v_galones AS CHAR.
DEFINE VAR v_valor_galones AS CHAR.


RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST lotes_aceite OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria OF lotes_aceite NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST r_muestras_protocolos WHERE r_muestras_protocolos.id_protocolo = protocolos.id_protocolo
                                       AND r_muestras_protocolos.id_articulo  = protocolos.id_articulo
                                       AND r_muestras_protocolos.anio         = protocolos.anio 
                                     NO-LOCK NO-ERROR.

    IF AVAILABLE r_muestras_protocolos THEN DO:
        FIND FIRST items_muestras OF r_muestras_protocolos NO-LOCK NO-ERROR.
        FIND FIRST envases_muestras OF items_muestras NO-LOCK NO-ERROR.
    END.

    FOR EACH items_protocolos OF protocolos NO-LOCK.
        FIND FIRST caracteristicas_quimicas OF items_protocolos NO-LOCK NO-ERROR.
        v_caract = v_caract + caracteristicas_quimicas.descripcion + CHR(10).
        v_valores = v_valores + general.items_protocolos.valor_caracter + CHR(10).
    END.
    
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        IF AVAILABLE lotes_aceite THEN STRING(lotes_aceite.id_lote,">999") + "/" + SUBSTRING(STRING(lotes_aceite.anio,">>99"),3,2) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE envases_muestras THEN UPPER(envases_muestras.descripcion_ingles) ELSE ""
        IF AVAILABLE envases_muestras THEN STRING(items_muestras.cantidad) + " X " + UPPER(envases_muestras.volume) ELSE ""
        STRING(industria.protocolos.cantidad_tambores,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99")
        STRING(industria.protocolos.peso_neto)
        STRING(industria.protocolos.desde_tambor,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99") +
        " TO " + STRING(industria.protocolos.hasta_tambor,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99")
        STRING(protocolos.id_protocolo,">999") + "/" + STRING(protocolos.anio,"99")
        v_caract
        v_valores
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo_firma) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        .
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                  /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                  /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                  /* Main Document */
                      lv_cDataFile,                                                  /* File that holds all the data */
                      lv_cMailMergeFile,                                             /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                    /* Automatically Print New Mail Merge Document */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Liberar Protocolo */
DO:
  DEFINE VAR r_proto AS ROWID.
  DEFINE VAR r_lote AS ROWID.

  RUN GET-rowid1 IN h_b_protocolos (OUTPUT r_proto).
  FIND FIRST protocolos WHERE ROWID(protocolos) = r_proto NO-ERROR.
  IF AVAILABLE protocolos THEN DO:
    ASSIGN protocolos.id_empresa        = 0
           protocolos.id_sucursal       = 0
           protocolos.id_tipotambor     = 0
           protocolos.nromov            = 0
           protocolos.cantidad_tambores = 0
           protocolos.desde_tambor      = 0
           protocolos.hasta_tambor      = 0
           protocolos.peso_neto         = 0
           protocolos.peso_bruto        = 0
           protocolos.galones           = 0.
  END.
   RUN dispatch IN h_b_lote_jugo_protocolo ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
hide current-window.
run deshabilita-viewer-paginas.
run habilita-relacion-viewer-pagina.
{custom/method/ctitulo.i}
view current-window.
run select-page(1).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza_items_protocolos W-Win 
PROCEDURE actualiza_items_protocolos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN select-page (1).
RUN dispatch IN h_b_items_protocolos ('open-query':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 1.24 , 3.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 37.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 59.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_protocolos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_protocolos ).
       RUN set-position IN h_b_protocolos ( 3.62 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_protocolos ( 8.33 , 54.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_protocolos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_protocolos ).
       RUN set-position IN h_v_protocolos ( 3.86 , 60.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.86 , 92.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/template/cfolder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Chemical Prop|Lote Jugo|Lote Aceite|Packaging|Cascara' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_cfolder ).
       RUN set-position IN h_cfolder ( 12.19 , 2.00 ) NO-ERROR.
       RUN set-size IN h_cfolder ( 12.38 , 152.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_protocolos. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_protocolos ).

       /* Links to SmartViewer h_v_protocolos. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_v_protocolos ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_protocolos ).

       /* Links to SmartFolder h_cfolder. */
       RUN add-link IN adm-broker-hdl ( h_cfolder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_protocolos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_protocolos ).
       RUN set-position IN h_b_items_protocolos ( 13.86 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_items_protocolos ( 10.24 , 84.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 14.57 , 92.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_protocolos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_protocolos ).
       RUN set-position IN h_v_items_protocolos ( 17.19 , 90.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.00 , 61.00 ) */

       /* Links to csmartbrowser h_b_items_protocolos. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_b_items_protocolos ).

       /* Links to SmartViewer h_v_items_protocolos. */
       RUN add-link IN adm-broker-hdl ( h_b_items_protocolos , 'Record':U , h_v_items_protocolos ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_v_items_protocolos ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lote_jugo_protocolo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lote_jugo_protocolo ).
       RUN set-position IN h_b_lote_jugo_protocolo ( 14.10 , 7.00 ) NO-ERROR.
       RUN set-size IN h_b_lote_jugo_protocolo ( 4.52 , 71.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_inspecciones_lote_jugo_protocolo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_inspecciones_lote_jugo_proto ).
       RUN set-position IN h_b_inspecciones_lote_jugo_proto ( 18.86 , 7.00 ) NO-ERROR.
       RUN set-size IN h_b_inspecciones_lote_jugo_proto ( 5.00 , 144.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_lote_jugo_protocolo. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_b_lote_jugo_protocolo ).

       /* Links to csmartbrowser h_b_inspecciones_lote_jugo_proto. */
       RUN add-link IN adm-broker-hdl ( h_b_lote_jugo_protocolo , 'Record':U , h_b_inspecciones_lote_jugo_proto ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lote_aceite_protocolo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lote_aceite_protocolo ).
       RUN set-position IN h_b_lote_aceite_protocolo ( 15.05 , 22.00 ) NO-ERROR.
       RUN set-size IN h_b_lote_aceite_protocolo ( 6.71 , 66.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_lote_aceite_protocolo. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_b_lote_aceite_protocolo ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-3 ).
       RUN set-position IN h_cus-updsav-3 ( 14.33 , 26.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-3 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_protocolos_1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_protocolos_1 ).
       RUN set-position IN h_v_protocolos_1 ( 16.95 , 38.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.91 , 49.00 ) */

       /* Links to SmartViewer h_v_protocolos_1. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_v_protocolos_1 ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-3 , 'TableIO':U , h_v_protocolos_1 ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-4 ).
       RUN set-position IN h_cus-updsav-4 ( 15.29 , 19.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-4 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_protocolo_cascara.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_protocolo_cascara ).
       RUN set-position IN h_v_protocolo_cascara ( 17.91 , 31.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.91 , 65.00 ) */

       /* Links to SmartViewer h_v_protocolo_cascara. */
       RUN add-link IN adm-broker-hdl ( h_b_protocolos , 'Record':U , h_v_protocolo_cascara ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-4 , 'TableIO':U , h_v_protocolo_cascara ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 5 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros W-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-viewer-paginas W-Win 
PROCEDURE deshabilita-viewer-paginas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run deshabilita_viewer.
    end.
end.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-36 BUTTON-35 BUTTON-34 
         BUTTON-32 BUTTON-33 RECT-22 RECT-4 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-protocolos W-Win 
PROCEDURE get-rowid-protocolos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_r AS ROWID.
RUN get-rowid1 IN h_b_protocolos (OUTPUT p_r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-relacion-viewer-pagina W-Win 
PROCEDURE habilita-relacion-viewer-pagina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run habilitar_relacion_viewer.
    end.
end.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion W-Win 
PROCEDURE impresion :
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.
define var v_oe as integer.
define var v_filtro as character.
define var v_fecha as character.
DEFINE VAR v_o_f AS CHAR.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    
FIND FIRST protocolos WHERE ROWID(protocolos) = r NO-ERROR.

{i_impresion_protocolos_2.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
/*
  run get-attribute ('current-page').
  MESSAGE "La pagina actual es " RETURN-VALUE VIEW-AS ALERT-BOX.
 */ 
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MailMerge W-Win 
PROCEDURE MailMerge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip_cDocument          AS CHAR.
DEF INPUT PARAMETER ip_cDataFile     AS CHAR.
DEF INPUT PARAMETER ip_cMailMergeDoc AS CHAR.
DEF INPUT PARAMETER iplPrint        AS LOGICAL.
 
DEFINE VAR lv_chDocument AS COM-HANDLE NO-UNDO.   

pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* Close all open documents */

ASSIGN  FILE-INFO:FILE-NAME = SEARCH(ip_cDocument)
        ip_cDocument              = FILE-INFO:FULL-PATHNAME
        FILE-INFO:FILE-NAME = SEARCH(ip_cDataFile)
        ip_cDataFile         = FILE-INFO:FULL-PATHNAME.

 ASSIGN lv_chDocument = pv_chWord:Documents:Open(ip_cDocument,NO,YES,YES,,,no,,) NO-ERROR.      /* Try & Open main Document */
 
 IF NOT VALID-HANDLE(lv_chDocument) THEN RETURN "ERROR".                                 /* Bummer */
 
 lv_chDocument:MailMerge:OpenDataSource(ip_cDataFile) NO-ERROR.                         /* Try & Open Data Source */
 
 IF ERROR-STATUS:ERROR THEN                                                         /* Bummer */
 DO: 
  RELEASE OBJECT lv_chDocument.
  RETURN "ERROR".
 END.

 ASSIGN lv_chDocument:MailMerge:Destination = 0.                                       /* Merge to New Document */
  
 lv_chDocument:MailMerge:Execute() NO-ERROR.                                           /* Perform the Merge */

 lv_chDocument:Close(NO).                                                              /* Close main document */
 
 RELEASE OBJECT lv_chDocument.                                                         /* Release document object */
 
 ASSIGN lv_chDocument = pv_chWord:Documents:Item(1) NO-ERROR.                             /* Get new mail merge document */
  
 IF NOT VALID-HANDLE(lv_chDocument) OR ERROR-STATUS:ERROR THEN RETURN "ERROR".              /* Bummer */
  
 lv_chDocument:SaveAs(ip_cMailMergeDoc).                                                /* Save new mail merge document */
 
 IF iplPrint THEN lv_chDocument:PrintOut(YES).                                         /* print new document if required */

 RELEASE OBJECT lv_chDocument NO-ERROR.                                                /* Release document object */
 
 /* pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* close all documents */ */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create W-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update W-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create W-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
alta = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update W-Win 
PROCEDURE pre-update :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

