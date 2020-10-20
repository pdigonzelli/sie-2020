&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE POSICIONES
    FIELD SERIE AS INTEGER
    FIELD PDOCVENTAS AS CHARACTER FORMAT 'X(15)'
    FIELD PPOSICIONDOCVENTAS AS CHARACTER FORMAT '999999'
    FIELD PMATERIAL AS CHARACTER FORMAT 'X(18)'
    FIELD PENVASE AS CHARACTER FORMAT 'X(18)'
    FIELD PCALIBRESTD AS CHARACTER FORMAT 'X(15)'
    FIELD PCALIBREEQU AS CHARACTER FORMAT 'X(15)'
    FIELD PCANTIDAD AS INTEGER
    FIELD PUNIDAD AS CHARACTER FORMAT 'X(3)'
    FIELD PORDER AS CHARACTER FORMAT 'X(12)'.

DEFINE INPUT PARAMETER SORDEN AS CHARACTER FORMAT 'X(12)' NO-UNDO.
DEFINE INPUT PARAMETER SDOCUMENTOVENTA AS CHARACTER FORMAT 'X(10)' NO-UNDO.
DEFINE INPUT PARAMETER SSOLICITANTE AS CHARACTER FORMAT 'X(10)' NO-UNDO.
DEFINE INPUT PARAMETER SNOMBRE AS CHARACTER FORMAT 'X(30)' NO-UNDO.
DEFINE INPUT PARAMETER SPOSICIONDOCUMENTOVENTA AS CHARACTER FORMAT '999999' NO-UNDO.
DEFINE INPUT PARAMETER SMATERIAL AS CHARACTER FORMAT 'X(18)' NO-UNDO.
DEFINE INPUT PARAMETER SESPECIE AS CHARACTER FORMAT 'X(14)' NO-UNDO.
DEFINE INPUT PARAMETER SVARIEDAD AS CHARACTER FORMAT 'X(14)' NO-UNDO.
DEFINE INPUT PARAMETER SCANTIDAD AS CHARACTER FORMAT '99999999' NO-UNDO.
DEFINE INPUT PARAMETER SUNIDAD AS CHARACTER FORMAT 'X(3)' NO-UNDO.
DEFINE INPUT PARAMETER SCALIDAD AS CHARACTER FORMAT 'X(4)' NO-UNDO.
DEFINE INPUT PARAMETER SCATEGORIA AS CHARACTER FORMAT 'X(20)' NO-UNDO.
DEFINE INPUT PARAMETER STIPOPALLET AS CHARACTER FORMAT 'X(20)' NO-UNDO.
DEFINE INPUT PARAMETER STIPOESQUINERO AS CHARACTER FORMAT 'X(20)' NO-UNDO.
DEFINE INPUT PARAMETER SPUERTOLLEGADA AS CHARACTER FORMAT '99999' NO-UNDO.
DEFINE INPUT PARAMETER SDESCRIPCIONPUERTOLELGADA AS CHARACTER FORMAT 'X(40)' NO-UNDO.
DEFINE INPUT PARAMETER SDESTINO AS CHARACTER FORMAT '99999' NO-UNDO.
DEFINE INPUT PARAMETER SDESCRIPCIONDESTINO AS CHARACTER FORMAT 'X(40)' NO-UNDO.
DEFINE INPUT PARAMETER SFECHADOCUMENTO AS CHARA FORMAT '99/99/9999'NO-UNDO.
DEFINE INPUT PARAMETER SVAPOR AS CHARACTER FORMAT '9999' NO-UNDO.
DEFINE INPUT PARAMETER SDESCRIPCIONVAPOR AS CHARACTER FORMAT 'X(40)' NO-UNDO.
DEFINE INPUT PARAMETER SLUGARDESCARGA AS CHARACTER FORMAT '99999' NO-UNDO.
DEFINE INPUT PARAMETER SPUERTOORIGEN AS CHARACTER FORMAT '99999' NO-UNDO.
DEFINE INPUT PARAMETER SDESCRIPCIONPUERTOORIGEN AS CHARACTER FORMAT 'X(40)' NO-UNDO.
DEFINE INPUT PARAMETER SMERCADO AS CHARACTER FORMAT 'X(120)' NO-UNDO.
DEFINE INPUT PARAMETER SCONTRAMARCA AS CHARACTER FORMAT 'X(20)' NO-UNDO.
DEFINE INPUT PARAMETER SAGENCIA AS CHARACTER FORMAT '99999' NO-UNDO.
DEFINE INPUT PARAMETER SDESCRIPCIONAGENCIA AS CHARACTER FORMAT 'X(40)' NO-UNDO.
DEFINE INPUT PARAMETER SMARCA AS CHARACTER FORMAT 'X(20)' NO-UNDO.


DEFINE INPUT PARAMETER TABLE FOR POSICIONES.

DEFINE VAR ILUG AS INTEGER NO-UNDO.
DEFINE VAR IPUERTO AS INTEGER NO-UNDO.
DEFINE VAR ICLIENTE AS INTEGER NO-UNDO.
DEFINE VAR IORDEN AS INTEGER NO-UNDO.
DEFINE VAR CDESTINO AS CHARACTER NO-UNDO.
/*DEFINE VAR CTIPOESQUINERO AS CHARACTER NO-UNDO.*/
DEFINE VAR CESPECIE AS CHARACTER NO-UNDO.



DEFINE VAR FLAG-ESQUINERO AS LOGICAL NO-UNDO.
DEFINE VAR I AS INTEGER NO-UNDO.
DEFINE VAR iITEM AS INTEGER NO-UNDO.

DEFINE VAR ICALIDAD AS INTEGER NO-UNDO.
DEFINE VAR CVARIEDAD AS CHARACTER NO-UNDO.
DEFINE VAR CENVASE AS CHARACTER NO-UNDO.
DEFINE VAR CMARCA AS CHARACTER NO-UNDO.
DEFINE VAR IPALETIZADO AS INTEGER NO-UNDO.
DEFINE VAR CTIPOPALET AS CHARACTER NO-UNDO.
DEFINE VAR CCATEGORIA AS CHARACTER NO-UNDO.
DEFINE VAR ITRATAMIENTO AS INTEGER NO-UNDO.
DEFINE VAR CVAPOR AS CHARACTER NO-UNDO.
DEFINE VAR RESPUESTA AS CHARACTER NO-UNDO.
DEFINE VAR CTIPOPROCESO AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
cVapor   = SVAPOR.
iLug     = INTEGER(SLUGARDESCARGA).
iPuerto  = INTEGER(SPUERTOLLEGADA).
cDestino = SDESTINO.
iOrden   = INTEGER(SORDEN).
iCliente = INT64(SSOLICITANTE).    
ctipoProceso = STIPOESQUINERO.
RESPUESTA =  'OK'.

MESSAGE 'INICIO ORDPROD'.
MESSAGE 'CLIENTE' STRING(ICLIENTE).
FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
IF NOT AVAILABLE clientes_ventas  THEN
DO:
  RESPUESTA = "Cliente no definido para la orden. No se procesara".
  RETURN RESPUESTA.
END.

FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa      = 1 AND
                                  pedidos_packing.id_punto_emisor = 1 AND
                                  pedidos_packing.id_orden        = iOrden  NO-ERROR.

IF AVAILABLE pedidos_packing AND pedidos_packing.id_vapor_usa <> 0 THEN  NEXT.


IF NOT AVAILABLE pedidos_packing  THEN DO:
    CREATE pedidos_packing.
    ASSIGN 
        pedidos_packing.id_orden        = iOrden
        pedidos_packing.id_empresa      = 1
        pedidos_packing.id_punto_emisor = 1.
END.

MESSAGE 'VAPOR-LUGAR-PUERTO-DESTINO-CLIENTE' SVAPOR ILUG IPUERTO CDESTINO ICLIENTE.

IF SVAPOR <> ? THEN FIND FIRST vapores WHERE vapores.id_vapor_sap = sVapor NO-LOCK NO-ERROR.
IF ILUG <> ? THEN FIND FIRST lugar_descarga WHERE INTEGER(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
IF IPUERTO <> ? THEN FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
FIND FIRST destinos WHERE INTEGER(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.


MESSAGE 'MATERIAL' SMATERIAL .

ASSIGN pedidos_packing.union_europea    = IF SMERCADO  = 'UE' THEN TRUE ELSE FALSE 
       pedidos_packing.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
       pedidos_packing.id_puerto_sal    = IF AVAILABLE lugar_descarga THEN lugar_descarga.id_lugdes ELSE 999
       pedidos_packing.id_pedido_sap    = SDOCUMENTOVENTA
       pedidos_packing.id_destino_final = IF AVAILABLE destinos THEN destinos.id_destino ELSE 999
       pedidos_packing.id_cliente       = IF AVAILABLE clientes_ventas THEN clientes_ventas.id_cliente ELSE 999                                                            
       pedidos_packing.id_mercado       = IF AVAILABLE clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
       pedidos_packing.fecha            = DATE(SFECHADOCUMENTO)  
       pedidos_packing.c_usuario        = 'Webservice Sap'
       pedidos_packing.contramarca      = '' /* SCONTRAMARCA */ 
       pedidos_packing.china            = IF SMERCADO = 'CHINA' THEN TRUE ELSE FALSE
       pedidos_packing.ano              = YEAR(pedidos_packing.fecha)
       pedidos_packing.OF_sap           = STRING(iOrden,'99999999999')
       pedidos_packing.id_cliente_remito = 999996
       pedidos_packing.id_puerto_ent    = pedidos_packing.id_destino_final
       pedidos_packing.id_unidad_medida = SUNIDAD
       pedidos_packing.id_orden         = iOrden.
       RUN semanaAno IN THIS-PROCEDURE (pedidos_packing.fecha , OUTPUT pedidos_packing.semana).
 ASSIGN  pedidos_packing.c_hora = STRING(NOW)
         pedidos_packing.c_fecha = TODAY
         pedidos_packing.posicion_pedido_sap = SPOSICIONDOCUMENTOVENTA. 

MESSAGE 'VARIOS' .

MESSAGE 'CANTIDAD CABECERA' SCANTIDAD.
        
i = 0.
FOR EACH POSICIONES:
  i = i + 1.
  iItem = i.
  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa      = 1 AND
                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                          Items_pedidos_packing.id_orden        = iOrden  AND
                                          items_pedidos_packing.ITEM            = i NO-ERROR.
                                          

  FIND FIRST pallets OF items_pedidos_packing NO-LOCK NO-ERROR.
  IF AVAILABLE pallets THEN 
  DO:
      MESSAGE 'ENCUENTRA PALLETS'.
      items_pedidos_packing.cant_pallets      = POSICIONES.PCANTIDAD.
      NEXT.
  END.

  MESSAGE '#1'.

  IF NOT AVAILABLE items_pedidos_packing THEN DO:
      CREATE    items_pedidos_packing.
      ASSIGN    items_pedidos_packing.id_empresa      = 1 
                items_pedidos_packing.id_punto_emisor = 1
                items_pedidos_packing.id_orden        = iOrden
                items_pedidos_packing.ITEM            = iItem.
  END.

  MESSAGE '#2' SCALIDAD.

  cEspecie = SESPECIE.
  icalidad = INTEGER(SCALIDAD).

  cVariedad      = SVARIEDAD.
  cEnvase        = POSICIONES.PENVASE.
  
  DEFINE VAR IUNIDAD AS INTEGER.

  MESSAGE '#3' SUNIDAD.
  
  CASE SUBSTRING(SUNIDAD,2,1):
    WHEN 'A' THEN IUNIDAD = 100 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'B' THEN IUNIDAD = 110 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'C' THEN IUNIDAD = 120 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'D' THEN IUNIDAD = 130 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'E' THEN IUNIDAD = 140 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'F' THEN IUNIDAD = 150 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'G' THEN IUNIDAD = 160 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'H' THEN IUNIDAD = 170 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    WHEN 'I' THEN IUNIDAD = 180 + INTEGER(SUBSTRING(SUNIDAD,3,1)).
    OTHERWISE IUNIDAD = INTEGER(SUBSTRING(SUNIDAD,2)).
  END.
  MESSAGE '#4' SUNIDAD IUNIDAD.
  
  iPaletizado    = IUNIDAD.


  ctipoPalet     = STIPOPALLET.
  ccategoria     = SCATEGORIA.


  MESSAGE 'VARIOS1' CESPECIE STIPOESQUINERO CCATEGORIA CTIPOPALET CMARCA CENVASE CCATEGORIA ICALIDAD ITRATAMIENTO CESPECIE IPALETIZADO.

  FIND FIRST variedades WHERE variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.                           
  FIND FIRST productos_terminados OF VARIEDADES NO-LOCK NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = SMARCA NO-LOCK NO-ERROR.
  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
  MESSAGE 'VARIOS2' CESPECIE CENVASE CMARCA CTIPOPALET STIPOESQUINERO CCATEGORIA ICALIDAD ITRATAMIENTO.
  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
  MESSAGE 'VARIOS3' ICALIDAD.
  FIND FIRST calidades WHERE calidades.id_calidad_sap = (TRIM(SCONTRAMARCA) + TRIM(SCalidad)) NO-LOCK NO-ERROR.
  MESSAGE 'VARIOS4' ITRATAMIENTO.
  FIND FIRST caracteristicas WHERE caracteristicas.id_caract_sap = STIPOESQUINERO NO-LOCK NO-ERROR.

  MESSAGE 'VARIOS5' CESPECIE CENVASE CMARCA CTIPOPALET STIPOESQUINERO CCATEGORIA ICALIDAD ITRATAMIENTO.

  MESSAGE 'CANTIDAD POSICION' POSICIONES.PCANTIDAD.

  ASSIGN 
      items_pedidos_packing.semana            = pedidos_packing.semana
      items_pedidos_packing.item              = iItem
      items_pedidos_packing.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
      items_pedidos_packing.id_vapor          = pedidos_packing.id_vapor
      items_pedidos_packing.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
      items_pedidos_packing.id_tipo_esquinero = 999
      items_pedidos_packing.id_punto_emisor   = pedidos_packing.id_punto_emisor
      items_pedidos_packing.id_pedido_sap     = pedidos_packing.id_pedido_sap 
      items_pedidos_packing.id_orden          = pedidos_packing.id_orden
      items_pedidos_packing.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
      items_pedidos_packing.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
      items_pedidos_packing.id_empresa        = pedidos_packing.id_empresa
      items_pedidos_packing.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
      items_pedidos_packing.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
      items_pedidos_packing.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
      items_pedidos_packing.c_usuario         = pedidos_packing.c_usuario
      items_pedidos_packing.contramarca       = pedidos_packing.contramarca
      items_pedidos_packing.cant_pallets      = DECIMAL(POSICIONES.PCANTIDAD) 
      items_pedidos_packing.calibre           = REPLACE(POSICIONES.PCALIBRESTD , '/' , '')
      items_pedidos_packing.bultos            = ipaletizado  /**** NO FUNCIONA CON LA NUEVA INTERFACE ***/
      items_pedidos_packing.anio              = pedidos_packing.ano 
      items_pedidos_packing.material_sap      = POSICIONES.PMATERIAL
      items_pedidos_packing.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
      items_pedidos_packing.c_hora            = STRING(NOW).
      items_pedidos_packing.c_fecha           = TODAY.
      

  RELEASE items_pedidos_packing.

  END.
  pedidos_packing.TOTAL_pallets = 0.
  FOR EACH items_pedidos_packing OF pedidos_packing.
      pedidos_packing.TOTAL_pallets = pedidos_packing.total_pallets + items_pedidos_packing.cant_pallets.
  END.
RETURN RESPUESTA.

CATCH EX AS Progress.Lang.Error :
    RESPUESTA = EX:GetMessage(1). 		
    UNDO, THROW EX.
END CATCH.

FINALLY:
    RETURN RESPUESTA.
END FINALLY.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-semanaAno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE semanaAno Procedure 
PROCEDURE semanaAno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pfecha AS DATE.
DEFINE OUTPUT PARAMETER psemana AS INTEGER.

DEFINE VAR idias AS INTEGER EXTENT 7 INITIAL [2,1,7,6,5,4,3].
DEFINE VAR fecha1 AS DATE NO-UNDO.
DEFINE VAR fecha2 AS DATE NO-UNDO.
DEFINE VAR pdia  AS INTEGER NO-UNDO.
DEFINE VAR pdia1 AS INTEGER NO-UNDO.
DEFINE VAR t AS INTEGER NO-UNDO.

DEFINE VAR bisiesto AS LOGICAL NO-UNDO.
DEFINE VAR bisiesto1 AS LOGICAL NO-UNDO.
DEFINE VAR dias AS INTEGER EXTENT 12 INITIAL [0,31,59,90,120,151,181,212,243,273,304,334].

DEFINE VAR nrodiasano AS INTEGER NO-UNDO.
DEFINE VAR nroano AS INTEGER NO-UNDO.
DEFINE VAR nrodias AS INTEGER NO-UNDO.

bisiesto = ( YEAR(pfecha) MODULO 4  = 0 AND YEAR(pfecha) MODULO 4 = 0 ) OR YEAR(pfecha) MODULO 400 = 0.
bisiesto1 = ( ( YEAR(pfecha) - 1 ) MODULO 4  = 0 AND ( YEAR(pfecha) - 1 ) MODULO 4 = 0 ) OR ( YEAR(pfecha) - 1 ) MODULO 400 = 0.

nrodiasano = dias[MONTH(pfecha)] + DAY(pfecha).

IF bisiesto AND MONTH ( pfecha ) > 2 THEN nrodiasano = nrodiasano + 1.

fecha1= DATE(1,1,YEAR(pfecha)).
pdia1 = WEEKDAY(fecha1).
pdia  = WEEKDAY(pfecha).

IF nrodiasano <= (8 - pdia1 ) AND pdia1 > 4  THEN DO:
    nroano = YEAR(pfecha) - 1.
    IF pdia1 = 5  OR ( pdia1 = 6 AND bisiesto1) THEN
        psemana = 53.
    ELSE
        psemana = 52.
END.
ELSE
   nroano = YEAR(pfecha).


IF nroano = YEAR(pfecha) THEN DO:
    IF bisiesto THEN
        nrodias = 366.
    ELSE
        nrodias = 365.
    IF nrodias - nrodiasano < ( 4 - pdia) THEN DO:
        nroano = YEAR(pfecha) + 1.
        psemana = 1.
    END.
END.

IF nroano = YEAR(pfecha) THEN DO:
    psemana = INTEGER( (nrodiasano + ( 7 - pdia ) + (pdia1 - 1)  ) / 7 ).
    IF pdia1 > 4 THEN  psemana = psemana - 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

