&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.


DEFINE TEMP-TABLE tt-oe 
    RCODE-INFORMATION
    FIELD id_orden_entrega              AS INTEGER  COLUMN-LABEL "OE"
    FIELD anio_oe                       AS INTEGER  COLUMN-LABEL "A�o"
    FIELD semana                        AS INTEGER  COLUMN-LABEL "Semana"
    FIELD id_despachante                AS INTEGER  COLUMN-LABEL "Cod.Despachante"
    FIELD despachante                   AS CHAR     COLUMN-LABEL "Despachante"
    FIELD id_vapor                      LIKE vapores.id_vapor COLUMN-LABEL "Cod.Vapor"
    FIELD vapor                         AS CHARACT  COLUMN-LABEL "Vapor"
    FIELD fecha_salida                  AS DATE     COLUMN-LABEL "Fecha Salida"
    FIELD fecha_arribo                  AS DATE     COLUMN-LABEL "Fecha Arribo"
    FIELD id_agencia                    AS INTEGER  COLUMN-LABEL "Cod.Agencia"
    FIELD agencia                       AS CHAR        COLUMN-LABEL "Agencia"
    FIELD id_destino                    AS INTEGER  COLUMN-LABEL "Cod.Destino"
    FIELD destino                       AS CHAR        COLUMN-LABEL "Destino"
    FIELD ITEM_oe                       AS INTEGER  COLUMN-LABEL "Parte OE"
    FIELD id_articulo                   LIKE productos_terminados.id_articulo COLUMN-LABEL "Cod.Producto"
    FIELD articulo                      LIKE productos_terminados.descripcion COLUMN-LABEL "Producto"
    FIELD id_envase                     LIKE envases_prod.id_envase COLUMN-LABEL "Cod.Envase"
    FIELD envase                        LIKE envases_prod.descripcion COLUMN-LABEL "Envase"
    FIELD id_calidad                    LIKE calidades.id_calidad COLUMN-LABEL "Cod.Calidad"
    FIELD calidad                       LIKE calidades.descripcion COLUMN-LABEL "Calidad"
    FIELD id_cliente                    LIKE subd_vtas.id_cliente COLUMN-LABEL "Cod.Cliente"
    FIELD razon_social                  LIKE clientes.razon_social COLUMN-LABEL "Cliente"
    FIELD id_contrato                   AS CHARACT  COLUMN-LABEL "Contrato"
    FIELD id_tipo_contrato              AS integer  COLUMN-LABEL "T.Contrato"
    FIELD anio                          AS integer  COLUMN-LABEL "A�o"
    FIELD item                          AS integer  COLUMN-LABEL "Parte"
    FIELD cantidad_tambores             AS integer  COLUMN-LABEL "Tambores"
    FIELD kgs_netos_tambores            AS decimal  COLUMN-LABEL "Kgs Netos"                DECIMALS 4
    FIELD kgs_brutos_tambores           AS decimal  COLUMN-LABEL "Kgs Brutos"               DECIMALS 4
    FIELD id_condicion_venta            AS integer  COLUMN-LABEL "C.Cond.Vta."
    FIELD condicion_venta               AS CHAR     COLUMN-LABEL "Cond. Venta"
    FIELD precio_x_galon                AS decimal  COLUMN-LABEL "Precio x Galon u$s"       DECIMALS 4
    FIELD total_galones                 AS decimal  COLUMN-LABEL "T. Galones"               DECIMALS 4
    FIELD total_factura                 AS decimal  COLUMN-LABEL "Total factura"            DECIMALS 4
    FIELD contenedores                  AS decimal  COLUMN-LABEL "Contenedores"             DECIMALS 2
    FIELD fob_ton                       AS decimal  COLUMN-LABEL "Fob Total"                DECIMALS 4
    FIELD grados_brix                   AS decimal  COLUMN-LABEL "Grados Brix"              DECIMALS 4
    FIELD id_tipo_contenedor            AS integer  COLUMN-LABEL "Cod.Tipo Cont."
    FIELD tipo_contenedor               AS CHAR     COLUMN-LABEL "Tipo Cont."
    FIELD plazo                         AS integer  COLUMN-LABEL "Plazo"
    FIELD id_tipo_plazo                 AS integer  COLUMN-LABEL "Cod.TipoPlazo"
    FIELD tipo_plazo                    AS CHAR     COLUMN-LABEL "Tipo Plazo"
    FIELD id_instrumento_pago           AS integer  COLUMN-LABEL "Cod.Inst.Pago"
    FIELD instrumento_pago              AS CHAR     COLUMN-LABEL "Instrumento Pago"
    FIELD fob_unitario                  AS decimal  COLUMN-LABEL "Fob Unitario"
    FIELD id_tipo_venta                 AS integer  COLUMN-LABEL "Cod.Tipo Venta"
    FIELD tipo_venta                    AS CHAR     COLUMN-LABEL "Tipo Venta"
   /* FIELD id_tipo_orden_entrega         AS integer  COLUMN-LABEL "Cod.Tipo OE"
    FIELD tipo_orden_entrega            AS CHAR     COLUMN-LABEL "Tipo OE" */
    FIELD cantidad_pallets              AS integer  COLUMN-LABEL "Cantidad Pallets"
    FIELD cajas_x_pallets               AS integer  COLUMN-LABEL "Cajas por Pallets"
    FIELD id_marca                      AS integer  COLUMN-LABEL "Cod.Marca"
    FIELD marca                         AS CHAR     COLUMN-LABEL "Marca"
    FIELD importe_comisiones            AS decimal  COLUMN-LABEL "Comision"                 DECIMALS 4
    FIELD coeficiente                   AS decimal  COLUMN-LABEL "Coeficiente"              DECIMALS 6
    FIELD valor_aduana_derechos         AS decimal  COLUMN-LABEL "Valor derechos aduana"    DECIMALS 4
    FIELD valor_aduana_reintegro        AS decimal  COLUMN-LABEL "Valor Reintegro"          DECIMALS 4
    FIELD importe_derechos_exportacion  AS decimal  COLUMN-LABEL "Importe Derechos"         DECIMALS 4
    FIELD importe_reintegro_fijo        AS decimal  COLUMN-LABEL "Importe Reintegro"        DECIMALS 4
    FIELD flete                         AS decimal  COLUMN-LABEL "Flete"                    DECIMALS 6
    FIELD seguro                        AS decimal  COLUMN-LABEL "Seguro"                   DECIMALS 6
    FIELD thc                           AS decimal  COLUMN-LABEL "THC"                      DECIMALS 6
    FIELD entri                         AS decimal  COLUMN-LABEL "Entry"                    DECIMALS 6
    FIELD varios                        AS decimal  COLUMN-LABEL "Varios"                   DECIMALS 4
    FIELD toll                          AS decimal  COLUMN-LABEL "Toll"                     DECIMALS 4
    FIELD handling                      AS decimal  COLUMN-LABEL "Handling"                 DECIMALS 4
    FIELD inland                        AS decimal  COLUMN-LABEL "InLand"                   DECIMALS 4
    FIELD bunker                        AS decimal  COLUMN-LABEL "Bunker"                   DECIMALS 4
    FIELD bl                            AS decimal  COLUMN-LABEL "BL"                       DECIMALS 4
    FIELD thcdestino                    AS decimal  COLUMN-LABEL "THC Destino"              DECIMALS 4
    FIELD inbalancesurcharge            AS decimal  COLUMN-LABEL "Inbalance Surcharge"      DECIMALS 4
    FIELD dcr                           AS decimal  COLUMN-LABEL "DCR"                      DECIMALS 4
    FIELD ebaf                          AS decimal  COLUMN-LABEL "EBAF"                     DECIMALS 4
    FIELD ams                           AS decimal  COLUMN-LABEL "AMS"                      DECIMALS 4.
    

DEFINE TEMP-TABLE tt-oe-resumen 
    RCODE-INFORMATION
    FIELD id_orden_entrega              AS INTEGER  COLUMN-LABEL "OE"
    FIELD id_despachante                AS INTEGER  COLUMN-LABEL "Cod.Despachante"
    FIELD despachante                   AS CHAR     COLUMN-LABEL "Despachante"
    FIELD id_vapor                      LIKE vapores.id_vapor COLUMN-LABEL "Cod.Vapor"
    FIELD vapor                         AS CHARACT  COLUMN-LABEL "Vapor"
    FIELD id_articulo                   LIKE productos_terminados.id_articulo COLUMN-LABEL "Cod.Producto"
    FIELD articulo                      LIKE productos_terminados.descripcion COLUMN-LABEL "Producto"
    FIELD id_calidad                    LIKE calidades.id_calidad COLUMN-LABEL "Cod.Calidad"
    FIELD calidad                       LIKE calidades.descripcion COLUMN-LABEL "Calidad".

DEFINE VARIABLE vfechadesde AS DATE       NO-UNDO.
DEFINE VARIABLE vfechahasta AS DATE       NO-UNDO.
DEFINE VARIABLE vcliente    AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-desde fi-hasta BUTTON-1 BUTTON-2 ~
BUTTON-21 
&Scoped-Define DISPLAYED-OBJECTS fi-desde fi-hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Query_Constructor LABEL "Query Constructor".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "&Exportar" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "&Cancelar" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-21 
     LABEL "Reporte Palito" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE fi-desde AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi-desde AT ROW 3.38 COL 18 COLON-ALIGNED
     fi-hasta AT ROW 4.57 COL 18 COLON-ALIGNED
     BUTTON-1 AT ROW 4.57 COL 51
     BUTTON-2 AT ROW 6 COL 51
     BUTTON-21 AT ROW 3.14 COL 51
     "  Filtro" VIEW-AS TEXT
          SIZE 43 BY .71 AT ROW 1.95 COL 5
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.6 BY 10.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Exportacion a Excell OE"
         HEIGHT             = 10.76
         WIDTH              = 76.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Exportacion a Excell OE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Exportacion a Excell OE */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Exportar */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   
   RUN creaTT.
   RUN generateExcel.p (INPUT TABLE TT-oe,
                        INPUT " Exportacion de OE",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Cancelar */
DO:
  APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 wWin
ON CHOOSE OF BUTTON-21 IN FRAME fMain /* Reporte Palito */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   
   RUN creaTT-resumen.
   RUN generateExcel.p (INPUT TABLE TT-oe-resumen,
                        INPUT " Exportacion de OE",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME m_Query_Constructor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Query_Constructor wWin
ON CHOOSE OF MENU-ITEM m_Query_Constructor /* Query Constructor */
DO:
    DEFINE VAR xSDOS        AS CHARACTER    NO-UNDO.
    DEFINE VAR i            AS INTEGER      NO-UNDO.
    DEFINE VAR iPage        AS INTEGER      NO-UNDO.
    DEFINE VAR iActualPage  AS INTEGER      NO-UNDO.
    DEFINE VAR xDataSource  AS CHARACTER    NO-UNDO.
    DEFINE VAR hDataSource  AS HANDLE       NO-UNDO.

    xSDOS = DYNAMIC-FUNCTION ('getSDO').
    {get CurrentPage iActualPage}.

    DO i = 1 TO NUM-ENTRIES(xSDOS):
        qh = WIDGET-HANDLE(ENTRY(i,xSDOS)).
        {get ObjectPage iPage qh}.
        {get DataSource xDataSource qh}.
        hDataSource = WIDGET-HANDLE(xDataSource).
        IF ( iPage = iActualPage OR iPage = 0 ) AND NOT valid-handle(hDataSource)THEN
            RUN adm2/support/wquery.w ( INPUT qh ).
        ELSE
            MESSAGE 'No puede ejecutar consulta en el detalle' VIEW-AS ALERT-BOX WARNING.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT wWin 
PROCEDURE creaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR v_flete AS DECIMAL.
DEFINE VAR v_seguro AS DECIMAL.
DEFINE VAR v_vs_ddp AS DECIMAL.
DEFINE VAR v_entri AS DECIMAL.
DEFINE VAR v_varios AS DECIMAL.
DEFINE VAR v_thc AS DECIMAL.
DEFINE VAR v_toll AS DECIMAL.
DEFINE VAR v_handling AS DECIMAL.
DEFINE VAR v_inland AS DECIMAL.
DEFINE VAR v_bunker AS DECIMAL.
DEFINE VAR v_bl AS DECIMAL.
DEFINE VAR v_thcdestino AS DECIMAL.
DEFINE VAR v_inbalancesurcharge AS DECIMAL.
DEFINE VAR v_dcr AS DECIMAL.
DEFINE VAR v_ebaf AS DECIMAL.
DEFINE VAR v_ams AS DECIMAL.


FOR EACH tt-oe.
    DELETE tt-oe.
END.

FOR EACH orden_entrega WHERE orden_entrega.fecha >= vfechadesde
                         AND orden_entrega.fecha <= vfechahasta
                         AND orden_entrega.id_orden_entrega > 1000
                       NO-LOCK.

    FIND FIRST agencias     WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
    FIND FIRST tipos_plazo  OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST destinos     OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
    
    FOR EACH gastos_orden_entrega OF orden_entrega NO-LOCK.
        CASE gastos_orden_entrega.id_gasto:
            WHEN 3 THEN DO: /* DUTY DDP (ENTRY) */
                v_entri = gastos_orden_entrega.importe.
            END.
            WHEN 5 THEN DO: /*FLETE*/
                v_flete = gastos_orden_entrega.importe.    
            END.
            WHEN 9 THEN DO: /* THC */
                ASSIGN v_thc = gastos_orden_entrega.importe.
            END.
            WHEN 11 THEN DO: /*SEGURO*/
                v_seguro = gastos_orden_entrega.importe.    
            END.
            WHEN 12 THEN DO: /* VARIOS */
                ASSIGN v_varios = gastos_orden_entrega.importe.    
            END.
            WHEN 14 THEN DO: /* TOLL */
                ASSIGN v_toll = gastos_orden_entrega.importe.    
            END.
            WHEN 15 THEN DO: /* HANDLING */
                ASSIGN v_handling = gastos_orden_entrega.importe.    
            END.
            WHEN 16 THEN DO: /* INLAND */
                ASSIGN v_inland = gastos_orden_entrega.importe.    
            END.
            WHEN 17 THEN DO: /* BUNKER */
                ASSIGN v_bunker = gastos_orden_entrega.importe.    
            END.
            WHEN 19 THEN DO: /* BL */
                ASSIGN v_bl = gastos_orden_entrega.importe.    
            END.
            WHEN 20 THEN DO: /* THC DESTINO */
                ASSIGN v_thcdestino = gastos_orden_entrega.importe.    
            END.
            WHEN 26 THEN DO: /* INBALANCE SURCHARGE */
                ASSIGN v_inbalancesurcharge = gastos_orden_entrega.importe.    
            END.
            WHEN 27 THEN DO: /* DCR */
                ASSIGN v_dcr = gastos_orden_entrega.importe.    
            END.
            WHEN 31 THEN DO: /* EBAF */
                ASSIGN v_ebaf = gastos_orden_entrega.importe.    
            END.
            WHEN 32 THEN DO: /* AMS */
                ASSIGN v_ams = gastos_orden_entrega.importe.    
            END.
        END CASE.    
    END.
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
        
        FIND FIRST clausulas WHERE clausulas.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            FIND FIRST variedades WHERE variedades.id_variedad = items_orden_entrega.id_calidad NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
        END.
        FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST marcas_prod OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST tipos_plazo OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST instrumentos_pagos WHERE instrumentos_pagos.id_instrumento_pago = 
                                            contratos.id_instrumento_pago NO-LOCK NO-ERROR.

        CREATE tt-oe.
        ASSIGN 
            tt-oe.id_orden_entrega  = orden_entrega.id_orden_entrega
            tt-oe.anio_oe           = YEAR(orden_entrega.fecha_embarque)
            tt-oe.semana            = orden_entrega.semana_embarque
            tt-oe.id_despachante    = orden_entrega.id_despachante
            tt-oe.despachante       = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
            tt-oe.id_vapor          = orden_entrega.id_vapor
            tt-oe.vapor             = IF AVAILABLE vapores THEN vapores.abreviatura ELSE "NONE"
            tt-oe.fecha_salida      = orden_entrega.fecha_embarque
            tt-oe.id_agencia        = orden_entrega.id_agencia
            tt-oe.agencia           = IF AVAILABLE agencias THEN agencias.abreviatura ELSE "NONE"
            tt-oe.id_destino        = orden_entrega.id_destino
            tt-oe.destino           = IF AVAILABLE destinos THEN destinos.abreviatura ELSE "NONE"
            tt-oe.ITEM_oe           = items_orden_entrega.ITEM_oe
            tt-oe.id_articulo       = items_orden_entrega.id_articulo
            tt-oe.articulo          = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
            tt-oe.id_envase         = items_orden_entrega.id_envase
            tt-oe.envase            = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE".
        
        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            ASSIGN tt-oe.calidad             = IF AVAILABLE variedades THEN variedades.abreviatura ELSE "NONE"
                   tt-oe.id_calidad          = items_orden_entrega.id_calidad.
        END.
        ELSE DO:
            ASSIGN tt-oe.calidad             = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
                   tt-oe.id_calidad          = items_orden_entrega.id_calidad.
        END.
        
        ASSIGN 
            tt-oe.id_cliente          = items_orden_entrega.id_cliente
            tt-oe.razon_social        = IF AVAILABLE clientes THEN clientes.nombre ELSE "NONE"
            tt-oe.id_contrato         = items_orden_entrega.id_contrato
            tt-oe.id_tipo_contrato    = items_orden_entrega.id_tipo_contrato
            tt-oe.anio                = items_orden_entrega.anio
            tt-oe.ITEM                = items_orden_entrega.ITEM
            tt-oe.cantidad_tambores   = items_orden_entrega.cantidad_tambores
            tt-oe.kgs_netos_tambores  = items_orden_entrega.kgs_netos_tambores
            tt-oe.kgs_brutos_tambores = items_orden_entrega.kgs_brutos_tambores
            tt-oe.id_condicion_venta  = items_orden_entrega.id_condicion_venta
            tt-oe.condicion_venta     = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "NONE"
            tt-oe.precio_x_galon      = items_orden_entrega.total_factura / items_orden_entrega.TOTAL_galones
            tt-oe.TOTAL_galones       = items_orden_entrega.TOTAL_galones
            tt-oe.total_factura       = items_orden_entrega.total_factura
            tt-oe.contenedores        = items_orden_entrega.contenedores
            tt-oe.id_tipo_contenedor  = items_orden_entrega.id_tipo_contenedor
            tt-oe.tipo_contenedor     = IF AVAILABLE tipo_contenedor THEN tipo_contenedor.descripcion ELSE "NONE"
            tt-oe.grados_brix         = items_orden_entrega.grados_brix

            tt-oe.plazo               = orden_entrega.plazo
            tt-oe.id_tipo_plazo       = orden_entrega.id_tipo_plazo
            tt-oe.tipo_plazo          = IF AVAILABLE tipos_plazo THEN tipos_plazo.descripcion ELSE "NONE"

            tt-oe.id_instrumento_pago = IF AVAILABLE contratos THEN contratos.id_instrumento_pago ELSE items_orden_entrega.id_instrumento_pago
            tt-oe.instrumento_pago    = IF AVAILABLE instrumentos_pagos THEN instrumentos_pagos.descripcion ELSE "NONE"
            
            tt-oe.fob_ton             = items_orden_entrega.fob_ton
            tt-oe.fob_unitario        = items_orden_entrega.fob_unitario
            tt-oe.id_tipo_venta       = items_orden_entrega.id_tipo_venta
            tt-oe.tipo_venta          = IF AVAILABLE tipo_venta THEN tipo_venta.descripcion ELSE "NONE"
            tt-oe.cantidad_pallets    = items_orden_entrega.cantidad_pallets
            tt-oe.cajas_x_pallets     = items_orden_entrega.cajas_x_pallets

            tt-oe.id_marca            = items_orden_entrega.id_marca
            tt-oe.marca               = IF AVAILABLE marcas_prod THEN marcas_prod.abreviatura ELSE "NONE"
            
            tt-oe.coeficiente                  = items_orden_entrega.coeficiente
            tt-oe.valor_aduana_derechos        = items_orden_entrega.valor_aduana_derecho 
            tt-oe.valor_aduana_reintegro       = items_orden_entrega.valor_aduana_reintegro 
            tt-oe.importe_derechos_exportacion = items_orden_entrega.importe_derecho 
            tt-oe.importe_reintegro_fijo       = items_orden_entrega.importe_reintegro 
            tt-oe.importe_comisiones           = items_orden_entrega.importe_comision

            tt-oe.flete                 = v_flete
            tt-oe.seguro                = v_seguro
            tt-oe.thc                   = v_thc
            tt-oe.entri                 = v_entri
            tt-oe.varios                = v_varios
            tt-oe.toll                  = v_toll
            tt-oe.handling              = v_handling
            tt-oe.inland                = v_inland
            tt-oe.bunker                = v_bunker
            tt-oe.bl                    = v_bl
            tt-oe.thcdestino            = v_thcdestino
            tt-oe.inbalancesurcharge    = v_inbalancesurcharge
            tt-oe.dcr                   = v_dcr
            tt-oe.ebaf                  = v_ebaf
            tt-oe.ams                   = v_ams
            .
    END.
    v_flete = 0.
    v_seguro = 0.
    v_entri = 0.    
    v_thc = 0.
    v_toll = 0.
    v_handling = 0.
    v_inland = 0.
    v_bunker = 0.
    v_bl = 0.
    v_thcdestino = 0.
    v_inbalancesurcharge = 0.
    v_dcr = 0.
    v_ebaf = 0.
    v_ams = 0.
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT-resumen wWin 
PROCEDURE creaTT-resumen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-oe-resumen.
    DELETE tt-oe-resumen.
END.

FOR EACH orden_entrega WHERE orden_entrega.fecha >= vfechadesde
                         AND orden_entrega.fecha <= vfechahasta
                         AND orden_entrega.id_orden_entrega > 1000
                       NO-LOCK.

    FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
    
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
        
        FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            FIND FIRST variedades WHERE variedades.id_variedad = items_orden_entrega.id_calidad NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
        END.
        
        CREATE tt-oe-resumen.
        ASSIGN 
            tt-oe-resumen.id_orden_entrega  = orden_entrega.id_orden_entrega
            tt-oe-resumen.id_despachante    = orden_entrega.id_despachante
            tt-oe-resumen.despachante       = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
            tt-oe-resumen.id_vapor          = orden_entrega.id_vapor
            tt-oe-resumen.vapor             = IF AVAILABLE vapores THEN vapores.abreviatura ELSE "NONE"
            tt-oe-resumen.id_articulo       = items_orden_entrega.id_articulo
            tt-oe-resumen.articulo          = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE".
            
        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            ASSIGN tt-oe-resumen.calidad             = IF AVAILABLE variedades THEN variedades.abreviatura ELSE "NONE"
                   tt-oe-resumen.id_calidad          = items_orden_entrega.id_calidad.
        END.
        ELSE DO:
            ASSIGN tt-oe-resumen.calidad             = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
                   tt-oe-resumen.id_calidad          = items_orden_entrega.id_calidad.
        END.
        
    END.
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fi-desde fi-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fi-desde fi-hasta BUTTON-1 BUTTON-2 BUTTON-21 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCobranza wWin 
PROCEDURE getCobranza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCobranzaCuenta wWin 
PROCEDURE getCobranzaCuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getGastos wWin 
PROCEDURE getGastos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLiq wWin 
PROCEDURE getLiq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQuery wWin 
PROCEDURE getQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER queryText AS CHARACTER NO-UNDO.
    queryText = queryText1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSort wWin 
PROCEDURE getSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER querySort AS CHARACTER NO-UNDO.
    querySort = querySort1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSort wWin 
PROCEDURE setSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xSort AS CHARACTER NO-UNDO.

querySort1 = xSort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
