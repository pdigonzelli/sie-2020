&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

define var i as integer no-undo.
define var nombre-item as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE csmartbrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Target,TableIO-Target,Consulta-Target,Navigation-Target,record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES muestras items_muestras envases_muestras ~
couriers contactos_muestras r_muestras_protocolos protocolos ~
aromas_sabores_prot cuerpo_prot colores_prot tipos_protocolos ~
items_protocolos contramarcas lotes_aceite lotes_jugo ~
Caracteristicas_quimicas

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table muestras.id_muestra ~
muestras.anio_muestra muestras.fecha muestras.id_estado ~
muestras.id_destinatario contactos_muestras.nombre ~
contactos_muestras.razon_social items_muestras.item_muestra ~
items_muestras.anio_muestra items_muestras.caracteristicas ~
items_muestras.id_envase envases_muestras.abreviatura ~
envases_muestras.descripcion envases_muestras.descripcion_ingles ~
envases_muestras.volumen items_muestras.cantidad ~
items_muestras.fecha_enviado_tuc items_muestras.fecha_enviado_bue ~
items_muestras.nro_guia_tuc items_muestras.nro_guia_bue ~
items_muestras.id_courier_bue couriers.descripcion couriers.abreviatura ~
protocolos.id_protocolo protocolos.anio protocolos.abraviatura ~
protocolos.descripcion protocolos.contramarca protocolos.fecha ~
protocolos.id_empresa protocolos.id_sucursal protocolos.id_tipotambor ~
protocolos.nromov lotes_aceite.id_lote lotes_jugo.id_lote protocolos.estado ~
protocolos.id_tipo_protocolo tipos_protocolos.abreviatura ~
tipos_protocolos.descripcion protocolos.aprobado ~
protocolos.fecha_aprobacion protocolos.cantidad_tambores ~
items_protocolos.id_articulo items_protocolos.id_caracteristica ~
items_protocolos.anio items_protocolos.valor ~
items_protocolos.valor_caracter items_protocolos.orden ~
contramarcas.descripcion contramarcas.abreviatura ~
Caracteristicas_quimicas.id_caracteristica ~
Caracteristicas_quimicas.descripcion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH muestras WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH items_muestras OF muestras NO-LOCK, ~
      EACH envases_muestras OF items_muestras NO-LOCK, ~
      EACH couriers WHERE TRUE /* Join to items_muestras incomplete */ NO-LOCK, ~
      EACH contactos_muestras WHERE TRUE /* Join to muestras incomplete */ NO-LOCK, ~
      EACH r_muestras_protocolos OF muestras NO-LOCK, ~
      EACH protocolos OF envases_muestras NO-LOCK, ~
      EACH aromas_sabores_prot WHERE TRUE /* Join to protocolos incomplete */ NO-LOCK, ~
      EACH cuerpo_prot WHERE TRUE /* Join to protocolos incomplete */ NO-LOCK, ~
      EACH colores_prot OF protocolos NO-LOCK, ~
      EACH tipos_protocolos OF protocolos NO-LOCK, ~
      EACH items_protocolos OF protocolos NO-LOCK, ~
      EACH contramarcas OF protocolos NO-LOCK, ~
      EACH lotes_aceite OF envases_muestras NO-LOCK, ~
      EACH lotes_jugo OF envases_muestras NO-LOCK, ~
      EACH Caracteristicas_quimicas OF items_protocolos NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table muestras items_muestras ~
envases_muestras couriers contactos_muestras r_muestras_protocolos ~
protocolos aromas_sabores_prot cuerpo_prot colores_prot tipos_protocolos ~
items_protocolos contramarcas lotes_aceite lotes_jugo ~
Caracteristicas_quimicas
&Scoped-define FIRST-TABLE-IN-QUERY-br_table muestras
&Scoped-define SECOND-TABLE-IN-QUERY-br_table items_muestras
&Scoped-define THIRD-TABLE-IN-QUERY-br_table envases_muestras
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table couriers
&Scoped-define FIFTH-TABLE-IN-QUERY-br_table contactos_muestras
&Scoped-define SIXTH-TABLE-IN-QUERY-br_table r_muestras_protocolos
&Scoped-define SEVENTH-TABLE-IN-QUERY-br_table protocolos
&Scoped-define EIGHTH-TABLE-IN-QUERY-br_table aromas_sabores_prot
&Scoped-define NINTH-TABLE-IN-QUERY-br_table cuerpo_prot
&Scoped-define TENTH-TABLE-IN-QUERY-br_table colores_prot


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
id_articulo|y|y|general.muestras.id_articulo
id_estado||y|general.muestras.id_estado
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "id_articulo",
     Keys-Supplied = "id_articulo,id_estado"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Menues" B-table-Win _INLINE
/* Actions: ? custom/support/cusbmen.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" B-table-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Cabecera" B-table-Win _INLINE
/* Actions: ? custom/support/set-cabecera.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Detalle" B-table-Win _INLINE
/* Actions: ? custom/support/set-detalle.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Items" B-table-Win _INLINE
/* Actions: ? custom/support/set-items.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Modalidad de Insercion Browser" B-table-Win _INLINE
/* Actions: ? custom/support/bmodins.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Relaciones" B-table-Win _INLINE
/* Actions: ? custom/support/keyeditb.w ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida B-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      muestras, 
      items_muestras, 
      envases_muestras, 
      couriers, 
      contactos_muestras, 
      r_muestras_protocolos, 
      protocolos, 
      aromas_sabores_prot, 
      cuerpo_prot, 
      colores_prot, 
      tipos_protocolos, 
      items_protocolos, 
      contramarcas, 
      lotes_aceite, 
      lotes_jugo, 
      Caracteristicas_quimicas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      muestras.id_muestra FORMAT ">>>>>9":U
      muestras.anio_muestra FORMAT ">>>9":U
      muestras.fecha FORMAT "99/99/99":U
      muestras.id_estado FORMAT ">>>>9":U
      muestras.id_destinatario FORMAT ">>>>>9":U
      contactos_muestras.nombre FORMAT "X(40)":U
      contactos_muestras.razon_social FORMAT "X(50)":U
      items_muestras.item_muestra FORMAT ">>9":U
      items_muestras.anio_muestra FORMAT ">>>9":U
      items_muestras.caracteristicas FORMAT "X(200)":U
      items_muestras.id_envase FORMAT ">>9":U
      envases_muestras.abreviatura FORMAT "X(12)":U
      envases_muestras.descripcion FORMAT "X(40)":U
      envases_muestras.descripcion_ingles FORMAT "X(30)":U
      envases_muestras.volumen FORMAT "X(8)":U
      items_muestras.cantidad FORMAT ">>9":U
      items_muestras.fecha_enviado_tuc FORMAT "99/99/99":U
      items_muestras.fecha_enviado_bue FORMAT "99/99/99":U
      items_muestras.nro_guia_tuc FORMAT "X(40)":U
      items_muestras.nro_guia_bue FORMAT "X(40)":U
      items_muestras.id_courier_bue FORMAT ">>9":U
      couriers.descripcion FORMAT "X(30)":U
      couriers.abreviatura FORMAT "X(12)":U
      protocolos.id_protocolo FORMAT "->>>>>9":U
      protocolos.anio FORMAT ">>>9":U
      protocolos.abraviatura FORMAT "X(12)":U
      protocolos.descripcion FORMAT "X(30)":U
      protocolos.contramarca FORMAT "X(30)":U
      protocolos.fecha FORMAT "99/99/99":U
      protocolos.id_empresa FORMAT ">>9":U
      protocolos.id_sucursal FORMAT ">>9":U
      protocolos.id_tipotambor FORMAT "->,>>>,>>9":U
      protocolos.nromov FORMAT "->,>>>,>>9":U
      lotes_aceite.id_lote FORMAT "->,>>>,>>9":U
      lotes_jugo.id_lote FORMAT "->,>>>,>>9":U
      protocolos.estado FORMAT ">>9":U
      protocolos.id_tipo_protocolo FORMAT ">>9":U
      tipos_protocolos.abreviatura FORMAT "X(12)":U
      tipos_protocolos.descripcion FORMAT "X(30)":U
      protocolos.aprobado FORMAT "si/no":U
      protocolos.fecha_aprobacion FORMAT "99/99/99":U
      protocolos.cantidad_tambores FORMAT ">>>>9":U
      items_protocolos.id_articulo FORMAT ">>>>>9":U
      items_protocolos.id_caracteristica FORMAT ">>9":U
      items_protocolos.anio FORMAT ">>>9":U
      items_protocolos.valor FORMAT "->>,>>9.9999":U
      items_protocolos.valor_caracter FORMAT "X(100)":U
      items_protocolos.orden FORMAT ">>9":U
      contramarcas.descripcion FORMAT "X(30)":U
      contramarcas.abreviatura FORMAT "X(12)":U
      Caracteristicas_quimicas.id_caracteristica FORMAT ">>9":U
      Caracteristicas_quimicas.descripcion FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 117 BY 19.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: csmartbrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.05
         WIDTH              = 117.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cbrowser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "general.muestras,general.items_muestras OF general.muestras,industria.envases_muestras OF general.items_muestras,industria.couriers WHERE general.items_muestras ...,industria.contactos_muestras WHERE general.muestras ...,industria.r_muestras_protocolos OF general.muestras,industria.protocolos OF general.envases_muestras,industria.aromas_sabores_prot WHERE general.protocolos ...,industria.cuerpo_prot WHERE general.protocolos ...,industria.colores_prot OF general.protocolos,industria.tipos_protocolos OF general.protocolos,general.items_protocolos OF general.protocolos,industria.contramarcas OF general.protocolos,industria.lotes_aceite OF general.envases_muestras,industria.lotes_jugo OF general.envases_muestras,industria.Caracteristicas_quimicas OF general.items_protocolos"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   = general.muestras.id_muestra
     _FldNameList[2]   = general.muestras.anio_muestra
     _FldNameList[3]   = general.muestras.fecha
     _FldNameList[4]   = general.muestras.id_estado
     _FldNameList[5]   = general.muestras.id_destinatario
     _FldNameList[6]   = general.contactos_muestras.nombre
     _FldNameList[7]   = general.contactos_muestras.razon_social
     _FldNameList[8]   = general.items_muestras.item_muestra
     _FldNameList[9]   = general.items_muestras.anio_muestra
     _FldNameList[10]   = general.items_muestras.caracteristicas
     _FldNameList[11]   = general.items_muestras.id_envase
     _FldNameList[12]   = general.envases_muestras.abreviatura
     _FldNameList[13]   = general.envases_muestras.descripcion
     _FldNameList[14]   = general.envases_muestras.descripcion_ingles
     _FldNameList[15]   = general.envases_muestras.volumen
     _FldNameList[16]   = general.items_muestras.cantidad
     _FldNameList[17]   = general.items_muestras.fecha_enviado_tuc
     _FldNameList[18]   = general.items_muestras.fecha_enviado_bue
     _FldNameList[19]   = general.items_muestras.nro_guia_tuc
     _FldNameList[20]   = general.items_muestras.nro_guia_bue
     _FldNameList[21]   = general.items_muestras.id_courier_bue
     _FldNameList[22]   = general.couriers.descripcion
     _FldNameList[23]   = general.couriers.abreviatura
     _FldNameList[24]   = general.protocolos.id_protocolo
     _FldNameList[25]   = general.protocolos.anio
     _FldNameList[26]   = general.protocolos.abraviatura
     _FldNameList[27]   = general.protocolos.descripcion
     _FldNameList[28]   = general.protocolos.contramarca
     _FldNameList[29]   = general.protocolos.fecha
     _FldNameList[30]   = general.protocolos.id_empresa
     _FldNameList[31]   = general.protocolos.id_sucursal
     _FldNameList[32]   = general.protocolos.id_tipotambor
     _FldNameList[33]   = general.protocolos.nromov
     _FldNameList[34]   = general.lotes_aceite.id_lote
     _FldNameList[35]   = general.lotes_jugo.id_lote
     _FldNameList[36]   = general.protocolos.estado
     _FldNameList[37]   = general.protocolos.id_tipo_protocolo
     _FldNameList[38]   = general.tipos_protocolos.abreviatura
     _FldNameList[39]   = general.tipos_protocolos.descripcion
     _FldNameList[40]   = general.protocolos.aprobado
     _FldNameList[41]   = general.protocolos.fecha_aprobacion
     _FldNameList[42]   = general.protocolos.cantidad_tambores
     _FldNameList[43]   = general.items_protocolos.id_articulo
     _FldNameList[44]   = general.items_protocolos.id_caracteristica
     _FldNameList[45]   = general.items_protocolos.anio
     _FldNameList[46]   = general.items_protocolos.valor
     _FldNameList[47]   = general.items_protocolos.valor_caracter
     _FldNameList[48]   = general.items_protocolos.orden
     _FldNameList[49]   = general.contramarcas.descripcion
     _FldNameList[50]   = general.contramarcas.abreviatura
     _FldNameList[51]   = general.Caracteristicas_quimicas.id_caracteristica
     _FldNameList[52]   = general.Caracteristicas_quimicas.descripcion
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON F3 OF br_table IN FRAME F-Main
DO:
  run buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
  define var r as rowid no-undo.
  define var c as character no-undo.
  define var h as handle no-undo.
  run get-link-handle in adm-broker-hdl ( input this-procedure , input 'Consulta-SOURCE' , output c).
  h=widget-handle(c).
  if valid-handle(h) then
    run dispatch in h ('busca-rowid').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
     /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
   /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF



/******** EMPIEZA MENU ************/
define sub-menu Ordena 
  menu-item muestras-id_muestra
label 'id_muestra( muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=muestras.id_muestra').
                  run set-attribute-list('titulo=Cod Muestra').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by muestras.id_muestra
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item muestras-anio_muestra
label 'anio_muestra( muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=muestras.anio_muestra').
                  run set-attribute-list('titulo=A�o').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by muestras.anio_muestra
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item muestras-fecha
label 'fecha( muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=muestras.fecha').
                  run set-attribute-list('titulo=Fecha').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by muestras.fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item muestras-id_estado
label 'id_estado( muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=muestras.id_estado').
                  run set-attribute-list('titulo=Cod Estado').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by muestras.id_estado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item muestras-id_destinatario
label 'id_destinatario( muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=muestras.id_destinatario').
                  run set-attribute-list('titulo=Cod Destinatario').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by muestras.id_destinatario
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item contactos_muestras-nombre
label 'nombre( contactos_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=contactos_muestras.nombre').
                  run set-attribute-list('titulo=Nombre').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by contactos_muestras.nombre
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item contactos_muestras-razon_social
label 'razon_social( contactos_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=contactos_muestras.razon_social').
                  run set-attribute-list('titulo=Razon Social').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by contactos_muestras.razon_social
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-item_muestra
label 'item_muestra( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.item_muestra').
                  run set-attribute-list('titulo=Parte').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.item_muestra
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-anio_muestra
label 'anio_muestra( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.anio_muestra').
                  run set-attribute-list('titulo=A�o').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.anio_muestra
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-caracteristicas
label 'caracteristicas( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.caracteristicas').
                  run set-attribute-list('titulo=Caracteristicas').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.caracteristicas
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-id_envase
label 'id_envase( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.id_envase').
                  run set-attribute-list('titulo=CodEnvase').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.id_envase
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envases_muestras-abreviatura
label 'abreviatura( envases_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envases_muestras.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by envases_muestras.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envases_muestras-descripcion
label 'descripcion( envases_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envases_muestras.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by envases_muestras.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envases_muestras-descripcion_ingles
label 'descripcion_ingles( envases_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envases_muestras.descripcion_ingles').
                  run set-attribute-list('titulo=Descripcion Ingles').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by envases_muestras.descripcion_ingles
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envases_muestras-volumen
label 'volumen( envases_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envases_muestras.volumen').
                  run set-attribute-list('titulo=Volumen').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by envases_muestras.volumen
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-cantidad
label 'cantidad( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.cantidad').
                  run set-attribute-list('titulo=Cantidad').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.cantidad
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-fecha_enviado_tuc
label 'fecha_enviado_tuc( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.fecha_enviado_tuc').
                  run set-attribute-list('titulo=Fecho Envio').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.fecha_enviado_tuc
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-fecha_enviado_bue
label 'fecha_enviado_bue( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.fecha_enviado_bue').
                  run set-attribute-list('titulo=Fecha Enviado').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.fecha_enviado_bue
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-nro_guia_tuc
label 'nro_guia_tuc( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.nro_guia_tuc').
                  run set-attribute-list('titulo=Nro Guia').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.nro_guia_tuc
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-nro_guia_bue
label 'nro_guia_bue( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.nro_guia_bue').
                  run set-attribute-list('titulo=Nro Guia').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.nro_guia_bue
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_muestras-id_courier_bue
label 'id_courier_bue( items_muestras ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_muestras.id_courier_bue').
                  run set-attribute-list('titulo=CodCourier').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_muestras.id_courier_bue
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item couriers-descripcion
label 'descripcion( couriers ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=couriers.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by couriers.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item couriers-abreviatura
label 'abreviatura( couriers ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=couriers.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by couriers.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-id_protocolo
label 'id_protocolo( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_protocolo').
                  run set-attribute-list('titulo=CodProtocolo').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_protocolo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-anio
label 'anio( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.anio').
                  run set-attribute-list('titulo=A�o').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.anio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-abraviatura
label 'abraviatura( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.abraviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.abraviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-descripcion
label 'descripcion( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-contramarca
label 'contramarca( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.contramarca').
                  run set-attribute-list('titulo=Contramarca').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.contramarca
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-fecha
label 'fecha( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.fecha').
                  run set-attribute-list('titulo=Fecha').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-id_empresa
label 'id_empresa( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_empresa').
                  run set-attribute-list('titulo=Empresa').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_empresa
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-id_sucursal
label 'id_sucursal( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_sucursal').
                  run set-attribute-list('titulo=Sucursal').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_sucursal
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-id_tipotambor
label 'id_tipotambor( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_tipotambor').
                  run set-attribute-list('titulo=Tipotambor').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_tipotambor
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-nromov
label 'nromov( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.nromov').
                  run set-attribute-list('titulo=nromov').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.nromov
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item lotes_aceite-id_lote
label 'id_lote( lotes_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=lotes_aceite.id_lote').
                  run set-attribute-list('titulo=Lote').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by lotes_aceite.id_lote
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item lotes_jugo-id_lote
label 'id_lote( lotes_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=lotes_jugo.id_lote').
                  run set-attribute-list('titulo=id_batch').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by lotes_jugo.id_lote
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-estado
label 'estado( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.estado').
                  run set-attribute-list('titulo=Estado').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.estado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-id_tipo_protocolo
label 'id_tipo_protocolo( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_tipo_protocolo').
                  run set-attribute-list('titulo=Tipo protocolo').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_tipo_protocolo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tipos_protocolos-abreviatura
label 'abreviatura( tipos_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tipos_protocolos.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by tipos_protocolos.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tipos_protocolos-descripcion
label 'descripcion( tipos_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tipos_protocolos.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by tipos_protocolos.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-aprobado
label 'aprobado( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.aprobado').
                  run set-attribute-list('titulo=Aprobado').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.aprobado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-fecha_aprobacion
label 'fecha_aprobacion( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.fecha_aprobacion').
                  run set-attribute-list('titulo=Fecha Aprobacion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.fecha_aprobacion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-cantidad_tambores
label 'cantidad_tambores( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.cantidad_tambores').
                  run set-attribute-list('titulo=Cantidad Tambores').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.cantidad_tambores
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-id_articulo
label 'id_articulo( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.id_articulo').
                  run set-attribute-list('titulo=CodArticulo').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.id_articulo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-id_caracteristica
label 'id_caracteristica( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.id_caracteristica').
                  run set-attribute-list('titulo=CodCaracteristica').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.id_caracteristica
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-anio
label 'anio( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.anio').
                  run set-attribute-list('titulo=A�o').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.anio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-valor
label 'valor( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.valor').
                  run set-attribute-list('titulo=Valor').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.valor
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-valor_caracter
label 'valor_caracter( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.valor_caracter').
                  run set-attribute-list('titulo=Value').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.valor_caracter
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item items_protocolos-orden
label 'orden( items_protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=items_protocolos.orden').
                  run set-attribute-list('titulo=Orden').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by items_protocolos.orden
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item contramarcas-descripcion
label 'descripcion( contramarcas ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=contramarcas.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by contramarcas.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item contramarcas-abreviatura
label 'abreviatura( contramarcas ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=contramarcas.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by contramarcas.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Caracteristicas_quimicas-id_caracteristica
label 'id_caracteristica( Caracteristicas_quimicas ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Caracteristicas_quimicas.id_caracteristica').
                  run set-attribute-list('titulo=CodCaracteristica').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by Caracteristicas_quimicas.id_caracteristica
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Caracteristicas_quimicas-descripcion
label 'descripcion( Caracteristicas_quimicas ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Caracteristicas_quimicas.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by Caracteristicas_quimicas.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by muestras.id_muestra
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=muestras.id_muestra').
run set-attribute-list('titulo=Cod Muestra').
/********** TERMINA MENU ***********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'id_articulo':U THEN DO:
       &Scope KEY-PHRASE muestras.id_articulo eq INTEGER(key-value)
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* id_articulo */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create B-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete B-table-Win 
PROCEDURE adm-post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update B-table-Win 
PROCEDURE adm-post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create B-table-Win 
PROCEDURE adm-pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete B-table-Win 
PROCEDURE adm-pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-update B-table-Win 
PROCEDURE adm-pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros B-table-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista-parametros as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca-registro B-table-Win 
PROCEDURE busca-registro :
define input parameter p as character no-undo.
define input parameter valor as character no-undo.
case p:
  WHEN 'muestras.id_muestra' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_muestra = integer(valor)  no-lock no-error .
  WHEN 'muestras.anio_muestra' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  anio_muestra = integer(valor)  no-lock no-error .
  WHEN 'muestras.fecha' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha = date(valor)  no-lock no-error .
  WHEN 'muestras.id_estado' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_estado = integer(valor)  no-lock no-error .
  WHEN 'muestras.id_destinatario' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_destinatario = integer(valor)  no-lock no-error .
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscar B-table-Win 
PROCEDURE buscar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
define var valor as character.
define var r as rowid no-undo.
define var p as character no-undo.
define var q as character no-undo.

run get-attribute('busca').
p= return-value.

if p <> "yes" Then
return.

run get-attribute('orden').
p= return-value.

run get-attribute('titulo').
q= return-value.

{&BROWSE-NAME}:set-repositioned-row(3,"always") in frame {&FRAME-NAME}.

run custom/support/fbusca.w  ( input p , input q, output valor) .

if valor <> "" then
do:
    run busca-registro (input p , input valor).
    if available {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} then
    do:
        r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
        reposition {&BROWSE-NAME} to rowid r.
        apply 'value-changed' to self.
    end.
end.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid B-table-Win 
PROCEDURE devuelve-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.
r=?.
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
get current {&BROWSE-NAME}.
if available ({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) then
    r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
else
    r=?.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
{custom/support/crow-available.i}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_articulo" "muestras" "id_articulo"}
  {src/adm/template/sndkycas.i "id_estado" "muestras" "id_estado"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "muestras"}
  {src/adm/template/snd-list.i "items_muestras"}
  {src/adm/template/snd-list.i "envases_muestras"}
  {src/adm/template/snd-list.i "couriers"}
  {src/adm/template/snd-list.i "contactos_muestras"}
  {src/adm/template/snd-list.i "r_muestras_protocolos"}
  {src/adm/template/snd-list.i "protocolos"}
  {src/adm/template/snd-list.i "aromas_sabores_prot"}
  {src/adm/template/snd-list.i "cuerpo_prot"}
  {src/adm/template/snd-list.i "colores_prot"}
  {src/adm/template/snd-list.i "tipos_protocolos"}
  {src/adm/template/snd-list.i "items_protocolos"}
  {src/adm/template/snd-list.i "contramarcas"}
  {src/adm/template/snd-list.i "lotes_aceite"}
  {src/adm/template/snd-list.i "lotes_jugo"}
  {src/adm/template/snd-list.i "Caracteristicas_quimicas"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valida B-table-Win 
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

