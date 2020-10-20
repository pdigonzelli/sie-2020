&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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


DEFINE TEMP-TABLE tt-factura 
    RCODE-INFORMATION
    FIELD zona         AS CHAR COLUMN-LABEL "Zona"
    FIELD destino      AS CHAR COLUMN-LABEL "Destino"
    FIELD condicion    AS CHAR COLUMN-LABEL "Condicion"
    FIELD clausula     AS CHAR COLUMN-LABEL "Clausula"
    FIELD id_viaje     LIKE viajes.id_viaje  COLUMN-LABEL "Cod.Viaje"
    FIELD id_vapor     LIKE vapores.id_vapor COLUMN-LABEL "Cod.Vapor"
    FIELD vapor        AS CHARACTER COLUMN-LABEL "Vapor"
    FIELD fecha_salida AS DATE COLUMN-LABEL "Fecha Salida"
    FIELD fecha_arribo AS DATE COLUMN-LABEL "Fecha Arribo"
    FIELD id_articulo  LIKE productos_terminados.id_articulo COLUMN-LABEL "Cod.Producto"
    FIELD articulo     LIKE productos_terminados.descripcion COLUMN-LABEL "Producto"
    FIELD id_variedad  LIKE variedades.id_variedad COLUMN-LABEL "Cod.Variedad"
    FIELD variedad     LIKE variedades.descripcion COLUMN-LABEL "Variedades"
    FIELD id_envase    LIKE envases_prod.id_envase COLUMN-LABEL "Cod.Envase"
    FIELD envase       LIKE envases_prod.descripcion COLUMN-LABEL "Envase"
    FIELD id_calidad   LIKE calidades.id_calidad COLUMN-LABEL "Cod.Calidad"
    FIELD calidad      LIKE calidades.descripcion COLUMN-LABEL "Calidad"
    FIELD id_marca     LIKE marcas_prod.id_marca COLUMN-LABEL "Cod.Marca"
    FIELD marca        LIKE marcas_prod.descripcion COLUMN-LABEL "Marca"
    FIELD calibre      AS CHAR COLUMN-LABEL "Calibre"
    FIELD nro_sami     AS CHAR COLUMN-LABEL "Nro."
    FIELD fecha_sami   AS DATE COLUMN-LABEL "Fecha Factura"
    FIELD cliente_sami AS INTEGER COLUMN-LABEL "Cod.Cliente"
    FIELD razon_sami   AS CHAR COLUMN-LABEL "Cliente"
    FIELD bultos_sami  AS DECIMAL COLUMN-LABEL "Bultos"
    FIELD importe_sami AS DECIMAL COLUMN-LABEL "Importe"
    FIELD nro_agl      AS CHAR COLUMN-LABEL "Nro."
    FIELD fecha_agl    AS DATE COLUMN-LABEL "Fecha Factura"
    FIELD cliente_agl  AS INTEGER COLUMN-LABEL "Cod.Cliente"
    FIELD razon_agl    AS CHAR COLUMN-LABEL "Cliente"
    FIELD bultos_agl   AS DECIMAL COLUMN-LABEL "Bultos"
    FIELD importe_agl  AS DECIMAL COLUMN-LABEL "Importe"
    FIELD nro_comp     AS CHAR COLUMN-LABEL "Nro."
    FIELD fecha_comp   LIKE subd_vtas.fecha_comp COLUMN-LABEL "Fecha Factura"
    FIELD id_cliente   LIKE subd_vtas.id_cliente COLUMN-LABEL "Cod.Cliente"
    FIELD razon_social LIKE clientes.razon_social COLUMN-LABEL "Cliente"
    FIELD bultos       AS DECIMAL COLUMN-LABEL "Bultos"
    FIELD importe      AS DECIMAL COLUMN-LABEL "Importe"
    FIELD factpall     AS DECIMAL COLUMN-LABEL "Facturado"
    FIELD provpall     AS DECIMAL COLUMN-LABEL "Liq.Provisorio"
    FIELD defpall      AS DECIMAL COLUMN-LABEL "Liq.Definitivo"
    FIELD stockpall    AS DECIMAL COLUMN-LABEL "Stock"
    FIELD sailingpall  AS DECIMAL COLUMN-LABEL "Sailing"
    FIELD montofobfact AS DECIMAL COLUMN-LABEL "Monto Facturado"
    FIELD montofobprov AS DECIMAL COLUMN-LABEL "Monto Provisorio"
    FIELD montofobdef  AS DECIMAL COLUMN-LABEL "Monto Definitivo"
    FIELD montofobobjetivo AS DECIMAL COLUMN-LABEL "Monto Objetivo"
    FIELD montostock   AS DECIMAL COLUMN-LABEL "Monto Stock"
    FIELD montosailing AS DECIMAL COLUMN-LABEL "Monto Sailing"
    FIELD cobranza    AS DECIMAL COLUMN-LABEL "Cobranza"
    FIELD cobranzacuenta AS DECIMAL COLUMN-LABEL "Cobranza Cuenta".

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
&Scoped-Define ENABLED-OBJECTS fi-cliente fi-desde fi-hasta BUTTON-1 ~
BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS fi-cliente fi-desde fi-hasta f-descripcion 

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

DEFINE VARIABLE f-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-cliente AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
     fi-cliente AT ROW 6 COL 18 COLON-ALIGNED
     fi-desde AT ROW 3.38 COL 18 COLON-ALIGNED
     fi-hasta AT ROW 4.57 COL 18 COLON-ALIGNED
     BUTTON-1 AT ROW 1.95 COL 51
     BUTTON-2 AT ROW 3.38 COL 51
     f-descripcion AT ROW 6 COL 32 COLON-ALIGNED NO-LABEL
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
         TITLE              = "Informe Acumulado de Gestion Comercial FF"
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

/* SETTINGS FOR FILL-IN f-descripcion IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Informe Acumulado de Gestion Comercial FF */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Informe Acumulado de Gestion Comercial FF */
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
   vcliente    = INTEGER(fi-cliente:SCREEN-VALUE).
   
   RUN creaTT.
   RUN generateExcel.p (INPUT TABLE TT-factura,
                        INPUT " Informe Budget",
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


&Scoped-define SELF-NAME fi-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente wWin
ON LEAVE OF fi-cliente IN FRAME fMain /* Cliente */
DO:
   find clientes where clientes.id_cliente = integer(self:screen-value in frame {&FRAME-NAME}) no-lock no-error.
  if available clientes then
   do:
    f-descripcion:screen-value in frame {&FRAME-NAME} = clientes.razon_social.
   end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente wWin
ON MOUSE-SELECT-DBLCLICK OF fi-cliente IN FRAME fMain /* Cliente */
DO:
    define var r as rowid.

    run ..\cons\c_clientes02.w (output r).
    if r <> ? then
     do:
       find clientes where rowid(clientes) = r no-lock no-error.
       if available clientes then
        do:
         fi-cliente:screen-value in frame {&FRAME-NAME} = string(clientes.id_cliente).
         f-descripcion:screen-value in frame {&FRAME-NAME} = clientes.razon_social.
        end. 
     end.

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
  {adm2/support/changePage.i}.  
  
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
 DEFINE BUFFER b_sami FOR ventas.subd_vtas.
 DEFINE BUFFER b_samidef FOR ventas.subd_vtas.
 DEFINE BUFFER b_agl  FOR ventas.subd_vtas.
 DEFINE BUFFER b_rpack FOR r_items_venta_pack_list.
 DEFINE BUFFER b_itmsamidef FOR ventas.items_venta.
 DEFINE BUFFER b_itmagl     FOR ventas.items_venta.
 DEFINE BUFFER b_cli_sami   FOR clientes.
 DEFINE BUFFER b_cli_agl    FOR clientes.
 DEFINE BUFFER b_clientes   FOR clientes.

 DEFINE VARIABLE vtipo AS CHARACTER  NO-UNDO.

 DEFINE VARIABLE vbult     AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vimpo     AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vbultsami AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vimposami AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vbultagl  AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vimpoagl  AS DECIMAL    NO-UNDO.

 DEFINE VARIABLE varribo   AS DATE       NO-UNDO.

 DEFINE VARIABLE vbultprov   AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vbultdef    AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vbultstock  AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vbultsailing AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vpalletprov AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vpalletdef  AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vsaldobult  AS INTEGER    NO-UNDO.
 DEFINE VARIABLE vsaldopall  AS INTEGER    NO-UNDO.

 DEFINE VARIABLE vmontofobfact AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vventabruta   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vgastos       AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vdefinitivo   AS LOGICAL    NO-UNDO.

 DEFINE VARIABLE vmontobrutoprov AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vmontobrutodef  AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vgastosest      AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vgastosdef      AS DECIMAL    NO-UNDO.

 DEFINE VARIABLE vmontofobprov   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vmontofobdef    AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vcobranza       AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vcobranzacuenta AS DECIMAL    NO-UNDO.

 FOR EACH tt-factura.
     DELETE tt-factura.
 END.

 FOR EACH ITEM_viajes ,
       FIRST viajes WHERE viajes.id_viaje      = ITEM_viajes.id_viaje AND
                          viajes.fecha_salida <= vfechahasta AND
                          viajes.id_cliente = vcliente,
       EACH r_item_viajes_pack_list OF ITEM_viajes,
       EACH r_items_venta_pack_list 
                WHERE r_items_venta_pack_list.id_packing_list = r_item_viajes_pack_list.id_packing_list AND
                      r_items_venta_pack_list.id_sucursal     = r_item_viajes_pack_list.id_sucursal AND
                      r_items_venta_pack_list.ITEM_pack       = r_item_viajes_pack_list.ITEM_pack ,
       FIRST items_venta OF r_items_venta_pack_list WHERE items_venta.id_punto_venta <> 9999 
       NO-LOCK BREAK BY r_items_venta_pack_list.nromov
                     BY ITEM_viajes.id_viaje
                     BY ITEM_viajes.id_articulo
                     BY ITEM_viajes.id_variedad
                     BY ITEM_viajes.id_envase
                     BY ITEM_viajes.id_calidad
                     BY ITEM_viajes.id_marca
                     BY ITEM_viajes.calibre:

    /* Facturas en firma o proformas de SAMI->Cliente Final */
    FIND b_sami WHERE b_sami.id_punto_venta = items_venta.id_punto_venta AND
                      b_sami.nromov         = items_venta.nromov NO-LOCK NO-ERROR.
    
    /* Busco la Relacion del packing con los otros comprobantes - Facturas Definitivas */
    FIND FIRST b_rpack WHERE b_rpack.id_packing_list = r_item_viajes_pack_list.id_packing_list AND
                       b_rpack.id_sucursal           = r_item_viajes_pack_list.id_sucursal     AND
                       b_rpack.ITEM_pack             = r_item_viajes_pack_list.ITEM_pack       AND
                       b_rpack.id_punto_venta        = items_venta.id_punto_venta AND
                       b_rpack.nromov                <> items_venta.nromov AND
                       b_rpack.ITEM                  = items_venta.ITEM  NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_rpack THEN DO:
    
        FIND b_samidef WHERE b_samidef.id_punto_venta = b_rpack.id_punto_venta AND
                             b_samidef.nromov         = b_rpack.nromov NO-LOCK NO-ERROR.
        FIND b_itmsamidef OF b_rpack NO-LOCK NO-ERROR.

        FIND b_clientes WHERE b_clientes.id_cliente = b_samidef.id_cliente NO-LOCK NO-ERROR.
    
    END.

    /* Obtengo el tipo de los comprobantes de SAMI->Cliente Final
       P: Proforma
       PD: Proforma con Definitiva 
       D: Definitiva */
    RUN getTipoFactura.p (INPUT ROWID(b_sami),
                          OUTPUT vtipo).
    
    /* Acumulo valores */
    vbult = vbult + IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 0.
    vimpo = vimpo + (IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 0) * (IF AVAILABLE b_itmsamidef THEN b_itmsamidef.precio_origen ELSE 0) +
                    (IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 0) * (IF AVAILABLE b_itmsamidef THEN b_itmsamidef.gastos ELSE 0).
    vbultsami = vbultsami + IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 
                            r_items_venta_pack_list.cantidad. /* Tomo las cantidades definitivas */
    vimposami = vimposami + (IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 
                             r_items_venta_pack_list.cantidad) * items_venta.precio_origen + 
                            (IF AVAILABLE b_rpack THEN b_rpack.cantidad ELSE 
                             r_items_venta_pack_list.cantidad) * items_venta.gastos.

    
   
    IF LAST-OF(ITEM_viajes.calibre) THEN DO:
    
            IF vtipo = "PD" OR vtipo = "P" OR vtipo = "N" THEN DO:
                
                FIND FIRST VAPORES WHERE VAPORES.ID_VAPOR = viajes.id_vapor NO-LOCK NO-ERROR. 
                FIND FIRST PRODUCTOS_TERMINADOS WHERE PRODUCTOS_TERMINADOS.ID_ARTICULO = ITEM_viajes.id_articulo NO-LOCK NO-ERROR.
                FIND FIRST ENVASES_PROD WHERE ENVASES_PROD.ID_ENVASE = ITEM_viajes.id_envase NO-LOCK NO-ERROR.
                FIND FIRST VARIEDADES WHERE VARIEDADES.ID_VARIEDAD = ITEM_viajes.id_variedad NO-LOCK NO-ERROR. 
                FIND FIRST CALIDADES WHERE CALIDADES.ID_CALIDAD = ITEM_viajes.id_calidad NO-LOCK NO-ERROR.
                FIND FIRST MARCAS_PROD WHERE MARCAS_PROD.ID_MARCA = ITEM_viajes.id_marca NO-LOCK NO-ERROR.
                FIND FIRST DESTINOS WHERE DESTINOS.ID_DESTINO = viajes.id_destino NO-LOCK NO-ERROR.
                FIND FIRST ZONAS_clientes WHERE ZONAS_CLIENTES.ID_ZONA = destinos.id_zona NO-LOCK NO-ERROR.
                FIND FIRST TIPO_VENTA WHERE TIPO_VENTA.ID_TIPO_VENTA = viajes.id_tipo_venta NO-LOCK NO-ERROR.
                FIND FIRST CLAUSULAS  WHERE CLAUSULAS.ID_CLAUSULA = viajes.id_clausula  NO-LOCK NO-ERROR.
                FIND FIRST b_cli_sami WHERE b_cli_sami.id_cliente = b_sami.id_cliente NO-LOCK NO-ERROR.
                

                FIND FIRST items_packing_list 
                                WHERE items_packing_list.id_packing_list = r_items_venta_pack_list.id_packing_list AND  
                                      items_packing_list.id_sucursal     = r_items_venta_pack_list.id_sucursal     AND
                                      items_packing_list.ITEM            = r_items_venta_pack_list.ITEM_pack       NO-LOCK NO-ERROR.
                FIND FIRST r_pallets_envase 
                                WHERE r_pallets_envase.id_envase      = items_packing_list.id_envase AND
                                      r_pallets_envase.id_tipo_pallet = items_packing_list.id_tipo_pallet NO-LOCK NO-ERROR.            

                RUN getLiq (OUTPUT vbultprov,
                            OUTPUT vbultdef,
                            OUTPUT vpalletprov,
                            OUTPUT vpalletdef,
                            OUTPUT vventabruta).

                RUN getGastos (INPUT vbultprov,
                               OUTPUT vgastos,
                               OUTPUT vdefinitivo). 

                RUN getCobranza (OUTPUT vcobranza).
                
                vbultstock   = vbultstock   + (IF varribo <= vfechahasta THEN (vbultsami  - (vbultprov + vbultdef)) ELSE 0).
                vbultsailing  = vbultsailing + (IF varribo > vfechahasta  THEN (vbultsami  - (vbultprov + vbultdef)) ELSE 0).

                vmontofobfact = vmontofobfact + (item_viajes.tot_u$s_fact - item_viajes.tot_flete).           

                vmontobrutoprov = IF NOT vdefinitivo THEN vventabruta ELSE 0.
                vmontobrutodef  = IF vdefinitivo THEN vventabruta ELSE 0.

                vgastosest = IF NOT vdefinitivo THEN vgastos ELSE 0.
                vgastosdef = IF vdefinitivo THEN vgastos ELSE 0.
    
                vmontofobprov = IF NOT vdefinitivo THEN vventabruta - vgastos ELSE 0. 
                vmontofobdef  = IF vdefinitivo THEN vventabruta - vgastos ELSE 0.

                varribo = viajes.fecha_salida + destinos.sailing.
                
                find last fobs where fobs.id_cliente  = viajes.id_cliente       and
                                     fobs.id_vapor    = viajes.id_vapor         and
                                     fobs.id_viaje    = viajes.id_viaje         AND
                                     fobs.id_articulo = item_viajes.id_articulo and
                                     fobs.id_envase   = item_viajes.id_envase   and
                                     fobs.id_variedad = item_viajes.id_variedad AND 
                                     fobs.id_campana  = viajes.id_campana  use-index fecha no-lock no-error.


                IF LAST-OF(ITEM_viajes.ID_viaje) THEN DO:             
                    RUN getCobranzaCuenta (OUTPUT vcobranzacuenta).             
                 end.     


                CREATE tt-factura.
                ASSIGN 
                       /* Datos Basicos */
                       tt-factura.id_viaje     = ITEM_viajes.id_viaje
                       tt-factura.id_vapor     = viajes.id_vapor
                       tt-factura.vapor        = IF AVAILABLE vapores THEN vapores.descripcion ELSE "SinInfo"
                       tt-factura.fecha_salida = viajes.fecha_salida
                       tt-factura.fecha_arribo = viajes.fecha_salida + destinos.sailing
                       tt-factura.id_articulo  = ITEM_viajes.id_articulo
                       tt-factura.articulo     = productos_terminados.descripcion
                       tt-factura.id_variedad  = ITEM_viajes.id_variedad
                       tt-factura.variedad     = variedades.descripcion
                       tt-factura.id_envase    = ITEM_viajes.id_envase
                       tt-factura.envase       = envases_prod.descripcion
                       tt-factura.id_calidad   = ITEM_viajes.id_calidad
                       tt-factura.calidad      = calidades.descripcion
                       tt-factura.id_marca     = ITEM_viajes.id_marca
                       tt-factura.marca        = marcas_prod.descripcion
                       tt-factura.calibre      = ITEM_viajes.calibre
        
                       /* Info SAMI->Cliente Final */
                       tt-factura.bultos_sami  = vbultsami
                       tt-factura.importe_sami = vimposami
                       tt-factura.nro_sami     = string(b_sami.id_punto_venta,"9999") + "-" + IF b_sami.nro_proforma = 0 THEN string(b_sami.nro_comp,"99999999") ELSE STRING(b_sami.nro_proforma,"99999999")
                       tt-factura.cliente_sami = b_sami.id_cliente
                       tt-factura.razon_sami   = b_cli_sami.razon_social
                       tt-factura.fecha_sami   = b_sami.fecha_comp
        
                       /* Info DEFINITIVAS */
                       tt-factura.bultos       = vbult
                       tt-factura.importe      = vimpo               
                       tt-factura.nro_comp     = IF AVAILABLE b_samidef THEN string(b_samidef.id_punto_venta,"9999") + "-" + IF b_samidef.nro_proforma = 0 THEN string(b_samidef.nro_comp,"99999999") ELSE STRING(b_samidef.nro_proforma,"99999999") ELSE ""
                       tt-factura.fecha_comp   = IF AVAILABLE b_samidef THEN b_samidef.fecha_comp ELSE ?
                       tt-factura.id_cliente   = IF AVAILABLE b_samidef THEN b_samidef.id_cliente ELSE 0
                       tt-factura.razon_social = IF AVAILABLE b_clientes THEN b_clientes.razon_social ELSE ""

                       /* Datos de Gestion */ 
                       tt-factura.zona         = zonas_clientes.descripcion
                       tt-factura.destino      = destinos.descripcion
                       tt-factura.condicion    = tipo_venta.descripcion
                       tt-factura.clausula     = clausulas.descripcion
                       tt-factura.factpall     = vbultsami / r_pallets_envase.pallets
                       tt-factura.provpall     = IF viajes.id_tipo_venta = 3 OR (viajes.id_tipo_venta = 1 AND vfechahasta > varribo) THEN vpalletprov ELSE 0 
                       tt-factura.defpall      = vpalletdef
                       tt-factura.stockpall    = IF varribo <= vfechahasta THEN (vbultsami / r_pallets_envase.pallets) - (vpalletprov + vpalletdef) ELSE 0
                       tt-factura.sailingpall  = IF varribo > vfechahasta  THEN (vbultsami / r_pallets_envase.pallets) - (vpalletprov + vpalletdef) ELSE 0
                       tt-factura.montofobfact = vmontofobfact
                       tt-factura.montofobprov = vmontofobprov
                       tt-factura.montofobdef  = vmontofobdef
                       tt-factura.montofobobjetivo = IF AVAILABLE fobs THEN (fobs.fob_objetivo_uss * vbultsami) ELSE 0
                       tt-factura.montostock   = IF tt-factura.stockpall <> 0 THEN (IF AVAILABLE fobs THEN (fobs.fob_stock_uss * vbultstock ) ELSE 0) ELSE 0
                       tt-factura.montoSailing = IF tt-factura.sailing   <> 0 THEN (IF AVAILABLE fobs THEN (fobs.fob_stock_uss * vbultsailing ) ELSE 0) ELSE 0
                       tt-factura.cobranza     = vcobranza
                       tt-factura.cobranzacuenta = vcobranzacuenta.
                 

                       
                vbult         = 0.
                vimpo         = 0.
                vbultsami     = 0.
                vbultstock    = 0.
                vbultsailing  = 0.
                vimposami     = 0.
                vmontofobfact = 0.
                vmontofobprov = 0.
                vmontofobdef  = 0.
            END.


            FIND FIRST r_items_venta_despacho OF items_venta NO-LOCK NO-ERROR.
            IF AVAILABLE r_items_venta_despacho THEN DO:
                FIND b_agl WHERE b_agl.id_punto_venta = r_items_venta_despacho.id_punto_venta_despacho AND
                                 b_agl.nromov         = r_items_venta_despacho.nromov_despacho NO-LOCK NO-ERROR.
                IF AVAILABLE b_agl AND b_agl.impreso THEN DO:
                
                        FIND FIRST b_cli_agl  WHERE b_cli_agl.id_cliente  = NO-LOCK NO-ERROR.
                        FIND b_itmagl WHERE b_itmagl.id_punto_venta = r_items_venta_despacho.id_punto_venta_despacho AND
                                            b_itmagl.nromov         = r_items_venta_despacho.nromov_despacho AND
                                            b_itmagl.ITEM           = r_items_venta_despacho.ITEM_despacho NO-LOCK NO-ERROR.
                
                     
                
                        ASSIGN 
                               tt-factura.nro_agl     = string(b_agl.id_punto_venta,"9999") + "-" + IF b_agl.nro_proforma = 0 THEN string(b_agl.nro_comp,"99999999") ELSE STRING(b_agl.nro_proforma,"99999999")
                               tt-factura.fecha_agl   = b_agl.fecha_comp
                               tt-factura.cliente_agl = b_agl.id_cliente
                               tt-factura.razon_agl   = b_cli_agl.razon_social
                               tt-factura.bultos_agl  = IF AVAILABLE r_items_venta_despacho THEN r_items_venta_despacho.cantidad ELSE 0
                               tt-factura.importe_agl = (IF AVAILABLE r_items_venta_despacho THEN r_items_venta_despacho.cantidad ELSE 0) * (IF AVAILABLE b_itmagl THEN b_itmagl.precio_origen ELSE 0) +
                                                        (IF AVAILABLE r_items_venta_despacho THEN r_items_venta_despacho.cantidad ELSE 0) * (IF AVAILABLE b_itmagl THEN b_itmagl.gastos ELSE 0).
                END. /* if available b_agl and b_agl.impreso then */
            END.

    END. /* if last-of(item_viajes.calibre) then */

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
  DISPLAY fi-cliente fi-desde fi-hasta f-descripcion 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fi-cliente fi-desde fi-hasta BUTTON-1 BUTTON-2 
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
DEFINE OUTPUT PARAMETER pcobranza AS DECIMAL    NO-UNDO.

for each r_cobranzas_viajes where r_cobranzas_viajes.id_viaje      = item_viajes.id_viaje and
                                  r_cobranzas_viajes.id_articulo   = item_viajes.id_articulo and
                                  r_cobranzas_viajes.id_variedad   = item_viajes.id_variedad no-lock .

            find cobranzas_me where cobranzas_me.id_tipocomp  = r_cobranzas_viajes.id_tipocomp and
                                    cobranzas_me.id_operacion = r_cobranzas_viajes.id_operacion and
                                    cobranzas_me.id_sucursal  = r_cobranzas_viajes.id_sucursal  and
                                    cobranzas_me.nromov       = r_cobranzas_viajes.nromov no-lock no-error.                                          
            if available cobranzas_me then 
              pcobranza = pcobranza + r_cobranzas_viajes.importe_pago . 
end.  


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
DEFINE OUTPUT PARAMETER pcobranzacuenta AS DECIMAL    NO-UNDO.

for each cobranzas_me where cobranzas_me.id_cliente   = viajes.id_cliente and
                            cobranzas_me.id_campana   = viajes.id_campana AND
                            cobranzas_me.id_operacion = 310               AND
                            cobranzas_me.estado       = true              and
                            cobranzas_me.fecha_comp  >= vfechadesde       and
                            cobranzas_me.fecha_comp  <= vfechahasta       and
                            cobranzas_me.saldo        <> 0     no-lock:

   for each r_cobranzas_viajes where r_cobranzas_viajes.id_tipocomp    = cobranzas_me.id_tipocomp  and
                                     r_cobranzas_viajes.id_operacion   = cobranzas_me.id_operacion and 
                                     r_cobranzas_viajes.nromov         = cobranzas_me.nromov       and
                                     r_cobranzas_viajes.id_sucursal    = cobranzas_me.id_sucursal  no-lock:
         pcobranzacuenta = pcobranzacuenta + cobranzas_me.saldo.
     
  end.         
END.

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

DEFINE INPUT  PARAMETER pbultosliq  AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER pgastos     AS DECIMAL.
DEFINE OUTPUT PARAMETER pdefinitivo AS LOGICAL INITIAL FALSE.

DEFINE VARIABLE vitmgastos AS DECIMAL    NO-UNDO.

FIND FIRST liquidaciones_definitivas 
       WHERE liquidaciones_definitivas.id_viaje    = ITEM_viajes.id_viaje  AND
             liquidaciones_definitivas.nro_liquidacion = ITEM_viajes.ITEM      AND
             liquidaciones_definitivas.fecha       >= vfechadesde and
             liquidaciones_definitivas.fecha       <= vfechahasta NO-LOCK NO-ERROR.
IF AVAILABLE liquidaciones_definitivas THEN DO:

    FOR EACH ITEM_liq_definitivas OF liquidaciones_definitivas NO-LOCK:
        vitmgastos = vitmgastos + ITEM_liq_definitivas.monto_origen.
    END.

    pgastos = liquidaciones_definitivas.comision + liquidaciones_definitivas.flete + vitmgastos. 
    pdefinitivo = TRUE.
END.
ELSE DO:
    FIND viajes WHERE viajes.id_viaje = ITEM_viajes.id_viaje NO-LOCK NO-ERROR.
    

    for each gastos_consig where gastos_consig.id_cliente  = viajes.id_cliente and
                                 gastos_consig.id_articulo = ITEM_viajes.id_articulo no-lock:
        for each item_gastos_consig of gastos_consig no-lock:                                       
           vitmgastos = vitmgastos + item_gastos_consig.gasto_caja_std. 
        end.
          
      pgastos = gastos_consig.comision  +  gastos_consig.flete + vitmgastos.
    end.   
    
    pgastos = pgastos * (pbultosliq / r_pallets_envase.pallets).
    pdefinitivo = FALSE.
END.

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
DEFINE OUTPUT PARAMETER pbultprov   AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER pbultdef    AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER ppalletprov AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER ppalletdef  AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER pventabruta AS DECIMAL    NO-UNDO.

FOR EACH item_liq_provisorias 
             where item_liq_provisorias.id_viaje    = item_viajes.id_viaje    and
                   ITEM_liq_provisorias.ITEM        = ITEM_viajes.ITEM,
             first liquidaciones_provisorias of item_liq_provisorias 
                   where liquidaciones_provisorias.fecha  >= vfechadesde AND
                         liquidaciones_provisorias.fecha  <= vfechahasta 
             BREAK BY item_liq_provisorias.id_articulo
                   by item_liq_provisorias.id_variedad
                   by item_liq_provisorias.id_envase
                   BY ITEM_liq_provisorias.id_calidad
                   BY ITEM_liq_provisorias.id_marca 
                   BY ITEM_liq_provisorias.calibre:

     FIND liquidaciones_definitivas 
                 where liquidaciones_definitivas.id_viaje        = item_viajes.id_viaje and
                       liquidaciones_definitivas.nro_liquidacion = item_viajes.ITEM     and
                       liquidaciones_definitivas.fecha >= vfechadesde AND
                       liquidaciones_definitivas.fecha <= vfechahasta no-lock no-error.
     if available liquidaciones_definitivas THEN DO:
             ppalletdef = ppalletdef + (item_liq_provisorias.bultos / r_pallets_envase.pallets).
             pbultdef   = pbultdef + item_liq_provisorias.bultos.
          end.
     else DO:
             ppalletprov = ppalletprov + (item_liq_provisorias.bultos / r_pallets_envase.pallets).
             pbultprov   = pbultprov + item_liq_provisorias.bultos.
     END.

     pventabruta = pventabruta +  item_liq_provisorias.monto_origen.
END.

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

