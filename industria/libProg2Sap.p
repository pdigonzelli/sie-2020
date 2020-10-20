&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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

DEFINE VARIABLE cSpace AS CHARACTER INITIAL "/" NO-UNDO.
DEFINE VARIABLE cTxtCh AS CHARACTER INITIAL "" NO-UNDO.
DEFINE TEMP-TABLE t_catvalmm
    FIELD id_articulo   LIKE articulos.id_articulo
    FIELD categoria     AS INTEGER
    INDEX i_articulo AS PRIMARY UNIQUE id_articulo.

DEFINE TEMP-TABLE t_stock
    FIELD id_sucursal   AS INTEGER
    FIELD id_articulo   LIKE articulos.id_articulo
    FIELD cantidad      AS DECIMAL
    FIELD precio        AS DECIMAL
    INDEX i_tstock AS PRIMARY UNIQUE id_articulo id_sucursal.

DEFINE TEMP-TABLE buProds LIKE euroamerica.productos_sap.

{..\industria\progs\iDefClientes.i}
{..\industria\progs\iDefMateriales.i}
{..\industria\progs\iDefProveedores.i}
{..\industria\progs\iDefTempTables.i}
{..\industria\progs\iDefExcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-convertToLetter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convertToLetter Procedure 
FUNCTION convertToLetter RETURNS CHARACTER
  (piCol AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlmacenInsumo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAlmacenInsumo Procedure 
FUNCTION getAlmacenInsumo RETURNS CHARACTER
  (INPUT ip_suc AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBancoSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBancoSap Procedure 
FUNCTION getBancoSap RETURNS CHARACTER
  (iBank AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferHandleFromTableName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBufferHandleFromTableName Procedure 
FUNCTION getBufferHandleFromTableName RETURNS HANDLE
  (ipcTable AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalidadAndPulpaSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidadAndPulpaSAP Procedure 
FUNCTION getCalidadAndPulpaSAP RETURNS CHARACTER
  (piCalidad AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadEnUMSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadEnUMSap Procedure 
FUNCTION getCantidadEnUMSap RETURNS CHARACTER
  (pcCod AS CHARACTER,
   piCant AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCategoriaValoracion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCategoriaValoracion Procedure 
FUNCTION getCategoriaValoracion RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCategoriaValoracionFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCategoriaValoracionFF Procedure 
FUNCTION getCategoriaValoracionFF RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCentroInsumo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCentroInsumo Procedure 
FUNCTION getCentroInsumo RETURNS CHARACTER
  (INPUT ip_suc AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCitde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCitde Procedure 
FUNCTION getCitde RETURNS CHARACTER
  (pcCitde AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getClienteSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClienteSap Procedure 
FUNCTION getClienteSap RETURNS INTEGER
  (piCli AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodigoProductoElabSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodigoProductoElabSap Procedure 
FUNCTION getCodigoProductoElabSap RETURNS CHARACTER
  (piArticulo AS INTEGER,
   piCalidad  AS INTEGER,
   piEnvase   AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodPostalSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodPostalSap Procedure 
FUNCTION getCodPostalSap RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcCP   AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCondicionIVA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCondicionIVA Procedure 
FUNCTION getCondicionIVA RETURNS CHARACTER
  (ip_iva AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCuentaAsociada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCuentaAsociada Procedure 
FUNCTION getCuentaAsociada RETURNS CHARACTER
  (pcSector AS CHARACTER, 
   pcPais   AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDenominacionProductoSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDenominacionProductoSAP Procedure 
FUNCTION getDenominacionProductoSAP RETURNS CHARACTER
  (piArticulo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescripcionRegion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescripcionRegion Procedure 
FUNCTION getDescripcionRegion RETURNS CHARACTER
  (pcPais AS CHARACTER,
   pcRegi AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestino Procedure 
FUNCTION getDestino RETURNS CHARACTER
  (piDes AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDomicilioCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDomicilioCliente Procedure 
FUNCTION getDomicilioCliente RETURNS CHARACTER
  (piCli AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEnvaseSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvaseSAP Procedure 
FUNCTION getEnvaseSAP RETURNS CHARACTER
  (piEnvase AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGrupoVendedoresFromPais) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGrupoVendedoresFromPais Procedure 
FUNCTION getGrupoVendedoresFromPais RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcSect AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIncotermFromContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIncotermFromContrato Procedure 
FUNCTION getIncotermFromContrato RETURNS CHARACTER
  (pcCon AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getJerarquiaProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getJerarquiaProducto Procedure 
FUNCTION getJerarquiaProducto RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosBrutosEnvase Procedure 
FUNCTION getKilosBrutosEnvase RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosEnvaseFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosBrutosEnvaseFF Procedure 
FUNCTION getKilosBrutosEnvaseFF RETURNS CHARACTER
  (piArt AS INTEGER,
   piEnv AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosNetosEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosNetosEnvase Procedure 
FUNCTION getKilosNetosEnvase RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosNetosEnvaseFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosNetosEnvaseFF Procedure 
FUNCTION getKilosNetosEnvaseFF RETURNS CHARACTER
  (piArt AS INTEGER,
   piEnv AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMonedaClienteFromIdMoneda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMonedaClienteFromIdMoneda Procedure 
FUNCTION getMonedaClienteFromIdMoneda RETURNS CHARACTER
  (piMoneda AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNroCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNroCliente Procedure 
FUNCTION getNroCliente RETURNS CHARACTER
  (piNum  AS INTEGER,
   pcPais AS CHARACTER, 
   pcSect AS CHARACTER, 
   pcCuit AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPaisFromLeyenda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPaisFromLeyenda Procedure 
FUNCTION getPaisFromLeyenda RETURNS CHARACTER
  (pcLeyenda AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPlazo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPlazo Procedure 
FUNCTION getPlazo RETURNS CHARACTER
  (pcCondPago AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPosicionArancelaria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPosicionArancelaria Procedure 
FUNCTION getPosicionArancelaria RETURNS CHARACTER
  (pcCod AS CHARACTER, 
   pcNet AS CHARACTER,
   pcBru AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProductoProgress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProductoProgress Procedure 
FUNCTION getProductoProgress RETURNS CHARACTER
  (pcMatnr AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProvincia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProvincia Procedure 
FUNCTION getProvincia RETURNS CHARACTER
  (INPUT ip_provincia AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRamo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRamo Procedure 
FUNCTION getRamo RETURNS CHARACTER
  (ip_prov AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegionesSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegionesSap Procedure 
FUNCTION getRegionesSap RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcRegi AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSedronarHazard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSedronarHazard Procedure 
FUNCTION getSedronarHazard RETURNS CHARACTER
  (INPUT ip_hazard AS INTEGER,
   INPUT ip_sedronar AS LOGICAL /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTextoFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTextoFactura Procedure 
FUNCTION getTextoFactura RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoMaterial Procedure 
FUNCTION getTipoMaterial RETURNS CHARACTER
  (ip_rubro AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoUnidadEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoUnidadEnvase Procedure 
FUNCTION getTipoUnidadEnvase RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoUnidadVta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoUnidadVta Procedure 
FUNCTION getTipoUnidadVta RETURNS INTEGER
  (pcUnidad AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadConversion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadConversion Procedure 
FUNCTION getUnidadConversion RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadMedida Procedure 
FUNCTION getUnidadMedida RETURNS CHARACTER
  (ip_um AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedidaBaseIN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadMedidaBaseIN Procedure 
FUNCTION getUnidadMedidaBaseIN RETURNS CHARACTER
  (pcCod AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getZonaVentas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getZonaVentas Procedure 
FUNCTION getZonaVentas RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcSect AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isNumeric) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isNumeric Procedure 
FUNCTION isNumeric RETURNS LOGICAL
  (pcValueString AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putSpaceIfEmpty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD putSpaceIfEmpty Procedure 
FUNCTION putSpaceIfEmpty RETURNS CHARACTER
  (pcData AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDatosBanco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDatosBanco Procedure 
FUNCTION setDatosBanco RETURNS CHARACTER
  (iPaymentInstruc AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addCliente Procedure 
PROCEDURE addCliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  CREATE BATCH_input_clientes_sap.
  ASSIGN  
    /* BKN00 */
    BATCH_input_clientes_sap.KUNNR      = cKUNNR
    BATCH_input_clientes_sap.BUKRS      = cBUKRS 
    BATCH_input_clientes_sap.VKORG      = cVKORG 
    BATCH_input_clientes_sap.VTWEG      = cVTWEG
    BATCH_input_clientes_sap.SPART      = cSPART
    BATCH_input_clientes_sap.KTOKD      = cKTOKD
    BATCH_input_clientes_sap.KKBER      = cKKBER 
    /* BKNB1 */
    BATCH_input_clientes_sap.AKONT      = cAKONT
    BATCH_input_clientes_sap.FDGRV      = cFDGRV
    BATCH_input_clientes_sap.KNB1-XZVER = cXZVER
    BATCH_input_clientes_sap.KNB1-ZWELS = cZWELS
    BATCH_input_clientes_sap.ALTKN      = cALTKN
    BATCH_input_clientes_sap.GRICD      = cGRICD
    BATCH_input_clientes_sap.GRIDT      = cGRIDT
    BATCH_input_clientes_sap.ZTERM      = cZTERM
    /* BKNBK */
    BATCH_input_clientes_sap.BANKS      = cBANKS
    BATCH_input_clientes_sap.BANKN      = cBANKN
    BATCH_input_clientes_sap.BANKL      = cBANKL
    BATCH_input_clientes_sap.BKONT      = cBKONT
    BATCH_input_clientes_sap.BVTYP      = cBVTYP
    BATCH_input_clientes_sap.XEZER      = cXEZER
    BATCH_input_clientes_sap.BKREF      = cBKREF
    BATCH_input_clientes_sap.KOINH      = cKOINH
    /* BKNA1 */
    BATCH_input_clientes_sap.ANRED      = cANRED
    BATCH_input_clientes_sap.NAME1      = cNAME1
    BATCH_input_clientes_sap.NAME2      = cNAME2
    BATCH_input_clientes_sap.NAME3      = cNAME3
    BATCH_input_clientes_sap.NAME4      = cNAME4
    BATCH_input_clientes_sap.SORT1      = cSORT1
    BATCH_input_clientes_sap.SORT2      = cSORT2
    BATCH_input_clientes_sap.STRAS      = cSTRAS
    BATCH_input_clientes_sap.ORT01      = cORT01
    BATCH_input_clientes_sap.PSTLZ      = cPSTLZ
    BATCH_input_clientes_sap.LAND1      = cLAND1
    BATCH_input_clientes_sap.REGIO      = cREGIO
    BATCH_input_clientes_sap.TELF1      = cTELF1
    BATCH_input_clientes_sap.TELFX      = cTELFX
    BATCH_input_clientes_sap.LIFNR      = cLIFNR
    BATCH_input_clientes_sap.STCD1      = cSTCD1
    BATCH_input_clientes_sap.KNA1-STKZN = cSTKZN
    BATCH_input_clientes_sap.BRSCH      = cBRSCH
    BATCH_input_clientes_sap.KUKLA      = cKUKLA
    BATCH_input_clientes_sap.LZONE      = cLZONE
    BATCH_input_clientes_sap.FITYP      = cFITYP
    BATCH_input_clientes_sap.STCDT      = cSTCDT
    /* BKNVA */
    BATCH_input_clientes_sap.ABLAD      = cABLAD
    BATCH_input_clientes_sap.KNFAK      = cKNFAK
    /* BKNVV */
    BATCH_input_clientes_sap.BZIRK      = cBZIRK 
    BATCH_input_clientes_sap.VKBUR      = cVKBUR 
    BATCH_input_clientes_sap.WAERS      = cWAERS 
    BATCH_input_clientes_sap.VKGRP      = cVKGRP 
    BATCH_input_clientes_sap.KDGRP      = cKDGRP 
    BATCH_input_clientes_sap.KONDA      = cKONDA 
    BATCH_input_clientes_sap.KALKS      = cKALKS 
    BATCH_input_clientes_sap.VSBED      = cVSBED 
    BATCH_input_clientes_sap.VWERK      = cVWERK 
    BATCH_input_clientes_sap.INCO1      = cINCO1 
    BATCH_input_clientes_sap.INCO2      = cINCO2 
    BATCH_input_clientes_sap.KTGRD      = cKTGRD 
    BATCH_input_clientes_sap.PLTYP      = cPLTYP 
    BATCH_input_clientes_sap.VERSG      = cVERSG 
    BATCH_input_clientes_sap.AUFSD      = cAUFSD 
    BATCH_input_clientes_sap.STCD2      = cSTCD2 
    BATCH_input_clientes_sap.COUNC      = cCOUNC 
    BATCH_input_clientes_sap.KZAZU      = cKZAZU 
    BATCH_input_clientes_sap.LPRIO      = cLPRIO 

    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addMaterial Procedure 
PROCEDURE addMaterial :
/*------------------------------------------------------------------------------
  Purpose: Procedimiento para asignar los datos de las tablas progres que se ha
           trasladado a las variables (comienzan "c").    
  Parameters:  <none>
  Notes:        
------------------------------------------------------------------------------*/

  CREATE BATCH_input_materiales_sap.
   
  ASSIGN BATCH_input_materiales_sap.MATNR  = cMATNR
         BATCH_input_materiales_sap.MBRSH  = cMBRSH
         BATCH_input_materiales_sap.MTART  = cMTART
         BATCH_input_materiales_sap.WERKS  = cWERKS
         BATCH_input_materiales_sap.XEIK1  = cXEIK1
         BATCH_input_materiales_sap.XEIC1  = cXEIC1
         BATCH_input_materiales_sap.XEIV1  = cXEIV1
         BATCH_input_materiales_sap.XEIV2  = cXEIV2
         BATCH_input_materiales_sap.XEIV4  = cXEIV4
         BATCH_input_materiales_sap.XEIV3  = cXEIV3
         BATCH_input_materiales_sap.XEIE1  = cXEIE1
         BATCH_input_materiales_sap.XEIE2  = cXEIE2
         BATCH_input_materiales_sap.XEID1  = cXEID1
         BATCH_input_materiales_sap.XEID2  = cXEID2
         BATCH_input_materiales_sap.XEID3  = cXEID3
         BATCH_input_materiales_sap.XEIP1  = cXEIP1
         BATCH_input_materiales_sap.XEIA1  = cXEIA1
         BATCH_input_materiales_sap.XEIL1  = cXEIL1
         BATCH_input_materiales_sap.XEIQ1  = cXEIQ1
         BATCH_input_materiales_sap.XEIB1  = cXEIB1
         BATCH_input_materiales_sap.XEIG1  = cXEIG1
         BATCH_input_materiales_sap.LGORT  = cLGORT
         BATCH_input_materiales_sap.VKORG  = cVKORG
         BATCH_input_materiales_sap.VTWEG  = cVTWEG
         BATCH_input_materiales_sap.MEINS  = cMEINS
         BATCH_input_materiales_sap.MAKTX  = cMAKTX
         BATCH_input_materiales_sap.BISMT  = cBISMT
         BATCH_input_materiales_sap.MATKL  = cMATKL
         BATCH_input_materiales_sap.BRGEW  = cBRGEW
         BATCH_input_materiales_sap.NTGEW  = cNTGEW
         BATCH_input_materiales_sap.GEWEI  = cGEWEI
         BATCH_input_materiales_sap.PRCTR  = cPRCTR
         BATCH_input_materiales_sap.VOLUM  = cVOLUM
         BATCH_input_materiales_sap.VOLEH  = cVOLEH
         BATCH_input_materiales_sap.GROES  = cGROES
         BATCH_input_materiales_sap.SPART  = cSPART
         BATCH_input_materiales_sap.SKTOF  = cSKTOF
         BATCH_input_materiales_sap.VRKME  = cVRKME
         BATCH_input_materiales_sap.TATY1  = cTATY1
         BATCH_input_materiales_sap.TAXKM1 = cTAXKM1
         BATCH_input_materiales_sap.TATY2  = cTATY2
         BATCH_input_materiales_sap.TAXKM2 = cTAXKM2
         BATCH_input_materiales_sap.TATY3  = cTATY3
         BATCH_input_materiales_sap.TAXKM3 = cTAXKM3
         BATCH_input_materiales_sap.TATY4  = cTATY4
         BATCH_input_materiales_sap.TAXKM4 = cTAXKM4
         BATCH_input_materiales_sap.KONDM  = cKONDM
         BATCH_input_materiales_sap.KTGRM  = cKTGRM
         BATCH_input_materiales_sap.MTPOS-MARA  = cMTPOS-MARA
         BATCH_input_materiales_sap.MTPOS  = cMTPOS
         BATCH_input_materiales_sap.PROVG  = cPROVG
         BATCH_input_materiales_sap.TRAGR  = cTRAGR
         BATCH_input_materiales_sap.LADGR  = cLADGR
         BATCH_input_materiales_sap.EKWSL  = cEKWSL
         BATCH_input_materiales_sap.MVGR1  = cMVGR1
         BATCH_input_materiales_sap.MVGR2  = cMVGR2
         BATCH_input_materiales_sap.MVGR3  = cMVGR3
         BATCH_input_materiales_sap.SSQSS  = cSSQSS
         BATCH_input_materiales_sap.QMPUR  = cQMPUR
         BATCH_input_materiales_sap.INSMK  = cINSMK
         BATCH_input_materiales_sap.EKGRP  = cEKGRP
         BATCH_input_materiales_sap.KAUTB  = cKAUTB
         BATCH_input_materiales_sap.WEBAZ  = cWEBAZ
         BATCH_input_materiales_sap.PLANI-WEBAZ  = cWEBAZ
         BATCH_input_materiales_sap.BSTME  = cBSTME
         BATCH_input_materiales_sap.VABME  = cVABME
         BATCH_input_materiales_sap.BESKZ  = cBESKZ
         BATCH_input_materiales_sap.MARC-SOBSL  = cMARC-SOBSL
         BATCH_input_materiales_sap.DISMM  = cDISMM
         BATCH_input_materiales_sap.DISPO  = cDISPO
         BATCH_input_materiales_sap.PLIFZ  = cPLIFZ
         BATCH_input_materiales_sap.DZEIT  = cDZEIT
         BATCH_input_materiales_sap.PERKZ  = cPERKZ
         BATCH_input_materiales_sap.LGPRO  = cLGPRO
         BATCH_input_materiales_sap.RGEKZ  = cRGEKZ
         BATCH_input_materiales_sap.FHORI  = cFHORI
         BATCH_input_materiales_sap.MINBE  = cMINBE
         BATCH_input_materiales_sap.EISBE  = cEISBE
         BATCH_input_materiales_sap.DISLS  = cDISLS
         BATCH_input_materiales_sap.BSTMI  = cBSTMI
         BATCH_input_materiales_sap.BSTMA  = cBSTMA
         BATCH_input_materiales_sap.BSTFE  = cBSTFE
         BATCH_input_materiales_sap.MABST  = cMABST
         BATCH_input_materiales_sap.BSTRF  = cBSTRF
         BATCH_input_materiales_sap.MAABC  = cMAABC
         BATCH_input_materiales_sap.LGRAD  = cLGRAD
         BATCH_input_materiales_sap.STRGR  = cSTRGR
         BATCH_input_materiales_sap.VRMOD  = cVRMOD
         BATCH_input_materiales_sap.VINT1  = cVINT1
         BATCH_input_materiales_sap.VINT2  = cVINT2
         BATCH_input_materiales_sap.ALTSL  = cALTSL
         BATCH_input_materiales_sap.SAUFT  = cSAUFT
         BATCH_input_materiales_sap.SFEPR  = cSFEPR
         BATCH_input_materiales_sap.SBDKZ  = cSBDKZ
         BATCH_input_materiales_sap.MTVFP  = cMTVFP
         BATCH_input_materiales_sap.DISGR  = cDISGR
         BATCH_input_materiales_sap.RWPRO  = cRWPRO
         BATCH_input_materiales_sap.AUTRU  = cAUTRU
         BATCH_input_materiales_sap.SHFLG  = cSHFLG
         BATCH_input_materiales_sap.SHZET  = cSHZET
         BATCH_input_materiales_sap.SHPRO  = cSHPRO
         BATCH_input_materiales_sap.EISLO  = cEISLO
         BATCH_input_materiales_sap.SFCPF  = cSFCPF
         BATCH_input_materiales_sap.XCHPF  = cXCHPF
         BATCH_input_materiales_sap.VENTAS-XCHPF = cXCHPF
         BATCH_input_materiales_sap.MARA-XCHPF   = cXCHPF
         BATCH_input_materiales_sap.FEVOR  = cFEVOR
         BATCH_input_materiales_sap.IPRKZ  = cIPRKZ
         BATCH_input_materiales_sap.VPRSV  = cVPRSV
         BATCH_input_materiales_sap.VERPR  = cVERPR
         BATCH_input_materiales_sap.STPRS  = cSTPRS
         BATCH_input_materiales_sap.PEINH  = cPEINH
         BATCH_input_materiales_sap.BKLAS  = cBKLAS
         BATCH_input_materiales_sap.XLIFO  = cXLIFO
         BATCH_input_materiales_sap.NCOST  = cNCOST
         BATCH_input_materiales_sap.EKALR  = cEKALR
         BATCH_input_materiales_sap.HKMAT  = cHKMAT
         BATCH_input_materiales_sap.AWSLS  = cAWSLS
         BATCH_input_materiales_sap.STLAN  = cSTLAN
         BATCH_input_materiales_sap.STLAL  = cSTLAL
         BATCH_input_materiales_sap.LOSGR  = cLOSGR
         BATCH_input_materiales_sap.EAN11  = cEAN11
         BATCH_input_materiales_sap.DWERK  = cDWERK
         BATCH_input_materiales_sap.TDLINE = cTDLINE
         BATCH_input_materiales_sap.MEINH  = cMEINH
         BATCH_input_materiales_sap.UMREN  = cUMREN
         BATCH_input_materiales_sap.UMREZ  = cUMREZ
         BATCH_input_materiales_sap.PLNTY  = cPLNTY
         BATCH_input_materiales_sap.VERSG  = cVERSG
         BATCH_input_materiales_sap.AESZN  = cAESZN
         BATCH_input_materiales_sap.BLATT  = cBLATT
         BATCH_input_materiales_sap.PROFL  = cPROFL
         BATCH_input_materiales_sap.EXTWG  = cEXTWG
         BATCH_input_materiales_sap.PRDHA  = cPRDHA
         BATCH_input_materiales_sap.STAWN  = cSTAWN
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addProveedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addProveedor Procedure 
PROCEDURE addProveedor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  CREATE BATCH_input_proveedores_sap.
  ASSIGN BATCH_INPUT_PROVEEDORES_SAP.ABUEB          = cABUEB     
         BATCH_INPUT_PROVEEDORES_SAP.ACTSS          = cACTSS     
         BATCH_INPUT_PROVEEDORES_SAP.AGREL          = cAGREL     
         BATCH_INPUT_PROVEEDORES_SAP.AKONT          = cAKONT     
         BATCH_INPUT_PROVEEDORES_SAP.ALTKN          = cALTKN     
         BATCH_INPUT_PROVEEDORES_SAP.ANRED          = cANRED     
         BATCH_INPUT_PROVEEDORES_SAP.BAHNS          = cBAHNS     
         BATCH_INPUT_PROVEEDORES_SAP.BBBNR          = cBBBNR     
         BATCH_INPUT_PROVEEDORES_SAP.BBSNR          = cBBSNR     
         BATCH_INPUT_PROVEEDORES_SAP.BEGRU          = cBEGRU     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-BEGRU    = cBEGRU     
         BATCH_INPUT_PROVEEDORES_SAP.BLIND          = cBLIND     
         BATCH_INPUT_PROVEEDORES_SAP.BLNKZ          = cBLNKZ     
         BATCH_INPUT_PROVEEDORES_SAP.BOIND          = cBOIND     
         BATCH_INPUT_PROVEEDORES_SAP.BOLRE          = cBOLRE     
         BATCH_INPUT_PROVEEDORES_SAP.BOPNR          = cBOPNR     
         BATCH_INPUT_PROVEEDORES_SAP.BRSCH          = cBRSCH     
         BATCH_INPUT_PROVEEDORES_SAP.BSTAE          = cBSTAE     
         BATCH_INPUT_PROVEEDORES_SAP.BUBKZ          = cBUBKZ     
         BATCH_INPUT_PROVEEDORES_SAP.BUKRS          = cBUKRS     
         BATCH_INPUT_PROVEEDORES_SAP.BUSAB          = cBUSAB     
         BATCH_INPUT_PROVEEDORES_SAP.CERDT          = cCERDT     
         BATCH_INPUT_PROVEEDORES_SAP.DATLT          = cDATLT     
         BATCH_INPUT_PROVEEDORES_SAP.DATLZ          = cDATLZ     
         BATCH_INPUT_PROVEEDORES_SAP.DLGRP          = cDLGRP     
         BATCH_INPUT_PROVEEDORES_SAP.DTAMS          = cDTAMS     
         BATCH_INPUT_PROVEEDORES_SAP.DTAWS          = cDTAWS     
         BATCH_INPUT_PROVEEDORES_SAP.EIKTO          = cEIKTO     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-EIKTO    = cEIKTO     
         BATCH_INPUT_PROVEEDORES_SAP.EKGRP          = cEKGRP     
         BATCH_INPUT_PROVEEDORES_SAP.EKORG          = cEKORG     
         BATCH_INPUT_PROVEEDORES_SAP.EMNFR          = cEMNFR     
         BATCH_INPUT_PROVEEDORES_SAP.ESRNR          = cESRNR     
         BATCH_INPUT_PROVEEDORES_SAP.EXPVZ          = cEXPVZ     
         BATCH_INPUT_PROVEEDORES_SAP.FDGRV          = cFDGRV     
         BATCH_INPUT_PROVEEDORES_SAP.FISKN          = cFISKN     
         BATCH_INPUT_PROVEEDORES_SAP.FITYP          = cFITYP     
         BATCH_INPUT_PROVEEDORES_SAP.FRGRP          = cFRGRP     
         BATCH_INPUT_PROVEEDORES_SAP.GBDAT          = cGBDAT     
         BATCH_INPUT_PROVEEDORES_SAP.GBORT          = cGBORT     
         BATCH_INPUT_PROVEEDORES_SAP.GRICD          = cGRICD     
         BATCH_INPUT_PROVEEDORES_SAP.GRIDT          = cGRIDT     
         BATCH_INPUT_PROVEEDORES_SAP.GUZTE          = cGUZTE     
         BATCH_INPUT_PROVEEDORES_SAP.HBKID          = cHBKID     
         BATCH_INPUT_PROVEEDORES_SAP.INCO1          = cINCO1     
         BATCH_INPUT_PROVEEDORES_SAP.INCO2          = cINCO2     
         BATCH_INPUT_PROVEEDORES_SAP.INTAD          = cINTAD     
         BATCH_INPUT_PROVEEDORES_SAP.IPISP          = cIPISP     
         BATCH_INPUT_PROVEEDORES_SAP.J_1KFREPRE     = cJ_1KFREPRE
         BATCH_INPUT_PROVEEDORES_SAP.J_1KFTBUS      = cJ_1KFTBUS 
         BATCH_INPUT_PROVEEDORES_SAP.J_1KFTIND      = cJ_1KFTIND 
         BATCH_INPUT_PROVEEDORES_SAP.KALSK          = cKALSK     
         BATCH_INPUT_PROVEEDORES_SAP.KONZS          = cKONZS     
         BATCH_INPUT_PROVEEDORES_SAP.KRAUS          = cKRAUS     
         BATCH_INPUT_PROVEEDORES_SAP.KTOCK          = cKTOCK     
         BATCH_INPUT_PROVEEDORES_SAP.KTOKK          = cKTOKK     
         BATCH_INPUT_PROVEEDORES_SAP.KULTG          = cKULTG     
         BATCH_INPUT_PROVEEDORES_SAP.KUNNR          = cKUNNR     
         BATCH_INPUT_PROVEEDORES_SAP.KVERM          = cKVERM     
         BATCH_INPUT_PROVEEDORES_SAP.KZABS          = cKZABS     
         BATCH_INPUT_PROVEEDORES_SAP.KZAUT          = cKZAUT     
         BATCH_INPUT_PROVEEDORES_SAP.KZRET          = cKZRET     
         BATCH_INPUT_PROVEEDORES_SAP.LAND1          = cLAND1     
         BATCH_INPUT_PROVEEDORES_SAP.LEBRE          = cLEBRE     
         BATCH_INPUT_PROVEEDORES_SAP.LFABC          = cLFABC     
         BATCH_INPUT_PROVEEDORES_SAP.LFRHY          = cLFRHY     
         BATCH_INPUT_PROVEEDORES_SAP.LFURL          = cLFURL.
  ASSIGN BATCH_INPUT_PROVEEDORES_SAP.LIBES          = cLIBES     
         BATCH_INPUT_PROVEEDORES_SAP.LIFNR          = cLIFNR     
         BATCH_INPUT_PROVEEDORES_SAP.LIPRE          = cLIPRE     
         BATCH_INPUT_PROVEEDORES_SAP.LISER          = cLISER     
         BATCH_INPUT_PROVEEDORES_SAP.LNRZA          = cLNRZA     
         BATCH_INPUT_PROVEEDORES_SAP.LNRZB          = cLNRZB     
         BATCH_INPUT_PROVEEDORES_SAP.LNRZE          = cLNRZE     
         BATCH_INPUT_PROVEEDORES_SAP.LOEVM          = cLOEVM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-LOEVM    = cLOEVM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-LOEVM    = cLOEVM     
         BATCH_INPUT_PROVEEDORES_SAP.LTSNA          = cLTSNA     
         BATCH_INPUT_PROVEEDORES_SAP.LZONE          = cLZONE     
         BATCH_INPUT_PROVEEDORES_SAP.MEGRU          = cMEGRU     
         BATCH_INPUT_PROVEEDORES_SAP.MEPRF          = cMEPRF     
         BATCH_INPUT_PROVEEDORES_SAP.MGRUP          = cMGRUP     
         BATCH_INPUT_PROVEEDORES_SAP.MINBW          = cMINBW     
         BATCH_INPUT_PROVEEDORES_SAP.MINDK          = cMINDK     
         BATCH_INPUT_PROVEEDORES_SAP.MRPPP          = cMRPPP     
         BATCH_INPUT_PROVEEDORES_SAP.NAME1          = cNAME1     
         BATCH_INPUT_PROVEEDORES_SAP.NAME2          = cNAME2     
         BATCH_INPUT_PROVEEDORES_SAP.NAME3          = cNAME3     
         BATCH_INPUT_PROVEEDORES_SAP.NAME4          = cNAME4     
         BATCH_INPUT_PROVEEDORES_SAP.NODEL          = cNODEL     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-NODEL    = cNODEL     
         BATCH_INPUT_PROVEEDORES_SAP.NRGEW          = cNRGEW     
         BATCH_INPUT_PROVEEDORES_SAP.ORT01          = cORT01     
         BATCH_INPUT_PROVEEDORES_SAP.ORT02          = cORT02     
         BATCH_INPUT_PROVEEDORES_SAP.PAPRF          = cPAPRF     
         BATCH_INPUT_PROVEEDORES_SAP.PERNR          = cPERNR     
         BATCH_INPUT_PROVEEDORES_SAP.PFACH          = cPFACH     
         BATCH_INPUT_PROVEEDORES_SAP.PFORT          = cPFORT     
         BATCH_INPUT_PROVEEDORES_SAP.PLIFZ          = cPLIFZ     
         BATCH_INPUT_PROVEEDORES_SAP.PODKZB         = cPODKZB    
         BATCH_INPUT_PROVEEDORES_SAP.PRFRE          = cPRFRE     
         BATCH_INPUT_PROVEEDORES_SAP.PROFS          = cPROFS     
         BATCH_INPUT_PROVEEDORES_SAP.PSTL2          = cPSTL2     
         BATCH_INPUT_PROVEEDORES_SAP.PSTLZ          = cPSTLZ     
         BATCH_INPUT_PROVEEDORES_SAP.QLAND          = cQLAND     
         BATCH_INPUT_PROVEEDORES_SAP.QSBGR          = cQSBGR     
         BATCH_INPUT_PROVEEDORES_SAP.QSREC          = cQSREC     
         BATCH_INPUT_PROVEEDORES_SAP.BLFBW-QSREC    = cQSREC     
         BATCH_INPUT_PROVEEDORES_SAP.QSSKZ          = cQSSKZ     
         BATCH_INPUT_PROVEEDORES_SAP.QSSYS          = cQSSYS     
         BATCH_INPUT_PROVEEDORES_SAP.QSSYSDAT       = cQSSYSDAT  
         BATCH_INPUT_PROVEEDORES_SAP.QSZDT          = cQSZDT     
         BATCH_INPUT_PROVEEDORES_SAP.QSZNR          = cQSZNR     
         BATCH_INPUT_PROVEEDORES_SAP.RDPRF          = cRDPRF     
         BATCH_INPUT_PROVEEDORES_SAP.REGIO          = cREGIO     
         BATCH_INPUT_PROVEEDORES_SAP.REGSS          = cREGSS     
         BATCH_INPUT_PROVEEDORES_SAP.REPRF          = cREPRF     
         BATCH_INPUT_PROVEEDORES_SAP.REVDB          = cREVDB     
         BATCH_INPUT_PROVEEDORES_SAP.SCACD          = cSCACD     
         BATCH_INPUT_PROVEEDORES_SAP.SENDE          = cSENDE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-SENDE    = cSENDE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-SENDE    = cSENDE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFBW-SENDE    = cSENDE     
         BATCH_INPUT_PROVEEDORES_SAP.SEXKZ          = cSEXKZ     
         BATCH_INPUT_PROVEEDORES_SAP.SFRGR          = cSFRGR     
         BATCH_INPUT_PROVEEDORES_SAP.SKRIT          = cSKRIT     
         BATCH_INPUT_PROVEEDORES_SAP.SORT1          = cSORT1     
         BATCH_INPUT_PROVEEDORES_SAP.SORT2          = cSORT2     
         BATCH_INPUT_PROVEEDORES_SAP.SPERM          = cSPERM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-SPERM    = cSPERM     
         BATCH_INPUT_PROVEEDORES_SAP.SPERQ          = cSPERQ     
         BATCH_INPUT_PROVEEDORES_SAP.SPERR          = cSPERR     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-SPERR    = cSPERR     
         BATCH_INPUT_PROVEEDORES_SAP.SPRAS          = cSPRAS     
         BATCH_INPUT_PROVEEDORES_SAP.STCD1          = cSTCD1     
         BATCH_INPUT_PROVEEDORES_SAP.STCD2          = cSTCD2     
         BATCH_INPUT_PROVEEDORES_SAP.STCD3          = cSTCD3     
         BATCH_INPUT_PROVEEDORES_SAP.STCD4          = cSTCD4     
         BATCH_INPUT_PROVEEDORES_SAP.STCDT          = cSTCDT     
         BATCH_INPUT_PROVEEDORES_SAP.BLFA1-STCDT    = cSTCDT     
         BATCH_INPUT_PROVEEDORES_SAP.STCEG          = cSTCEG     
         BATCH_INPUT_PROVEEDORES_SAP.STGDL          = cSTGDL.
  ASSIGN BATCH_INPUT_PROVEEDORES_SAP.STKZA          = cSTKZA     
         BATCH_INPUT_PROVEEDORES_SAP.STKZN          = cSTKZN     
         BATCH_INPUT_PROVEEDORES_SAP.STKZU          = cSTKZU     
         BATCH_INPUT_PROVEEDORES_SAP.STRAS          = cSTRAS     
         BATCH_INPUT_PROVEEDORES_SAP.STYPE          = cSTYPE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-STYPE    = cSTYPE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-STYPE    = cSTYPE     
         BATCH_INPUT_PROVEEDORES_SAP.BLFBW-STYPE    = cSTYPE     
         BATCH_INPUT_PROVEEDORES_SAP.STYPE1         = cSTYPE     
         BATCH_INPUT_PROVEEDORES_SAP.TAXBS          = cTAXBS     
         BATCH_INPUT_PROVEEDORES_SAP.TBNAM          = cTBNAM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFB1-TBNAM    = cTBNAM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-TBNAM    = cTBNAM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFBW-TBNAM    = cTBNAM     
         BATCH_INPUT_PROVEEDORES_SAP.TCODE          = cTCODE     
         BATCH_INPUT_PROVEEDORES_SAP.TELBX          = cTELBX     
         BATCH_INPUT_PROVEEDORES_SAP.TELF1          = cTELF1     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-TELF1    = cTELF1     
         BATCH_INPUT_PROVEEDORES_SAP.TELF2          = cTELF2     
         BATCH_INPUT_PROVEEDORES_SAP.TELFX          = cTELFX     
         BATCH_INPUT_PROVEEDORES_SAP.TELTX          = cTELTX     
         BATCH_INPUT_PROVEEDORES_SAP.TELX1          = cTELX1     
         BATCH_INPUT_PROVEEDORES_SAP.TLFNS          = cTLFNS     
         BATCH_INPUT_PROVEEDORES_SAP.TLFXS          = cTLFXS     
         BATCH_INPUT_PROVEEDORES_SAP.TOGRR          = cTOGRR     
         BATCH_INPUT_PROVEEDORES_SAP.TOGRU          = cTOGRU     
         BATCH_INPUT_PROVEEDORES_SAP.TXJCD          = cTXJCD     
         BATCH_INPUT_PROVEEDORES_SAP.UMSAE          = cUMSAE     
         BATCH_INPUT_PROVEEDORES_SAP.UZAWE          = cUZAWE     
         BATCH_INPUT_PROVEEDORES_SAP.VBUND          = cVBUND     
         BATCH_INPUT_PROVEEDORES_SAP.VENSL          = cVENSL     
         BATCH_INPUT_PROVEEDORES_SAP.VERKF          = cVERKF     
         BATCH_INPUT_PROVEEDORES_SAP.VSBED          = cVSBED     
         BATCH_INPUT_PROVEEDORES_SAP.VZSKZ          = cVZSKZ     
         BATCH_INPUT_PROVEEDORES_SAP.WAERS          = cWAERS     
         BATCH_INPUT_PROVEEDORES_SAP.WEBRE          = cWEBRE     
         BATCH_INPUT_PROVEEDORES_SAP.WEBTR          = cWEBTR     
         BATCH_INPUT_PROVEEDORES_SAP.WERKR          = cWERKR     
         BATCH_INPUT_PROVEEDORES_SAP.WERKS          = cWERKS     
         BATCH_INPUT_PROVEEDORES_SAP.WITHT          = cWITHT     
         BATCH_INPUT_PROVEEDORES_SAP.WT_EXDF        = cWT_EXDF   
         BATCH_INPUT_PROVEEDORES_SAP.WT_EXDT        = cWT_EXDT   
         BATCH_INPUT_PROVEEDORES_SAP.WT_EXNR        = cWT_EXNR   
         BATCH_INPUT_PROVEEDORES_SAP.WT_EXRT        = cWT_EXRT   
         BATCH_INPUT_PROVEEDORES_SAP.WT_SUBJCT      = cWT_SUBJCT 
         BATCH_INPUT_PROVEEDORES_SAP.WT_WITHCD      = cWT_WITHCD 
         BATCH_INPUT_PROVEEDORES_SAP.WT_WTEXRS      = cWT_WTEXRS 
         BATCH_INPUT_PROVEEDORES_SAP.WT_WTSTCD      = cWT_WTSTCD 
         BATCH_INPUT_PROVEEDORES_SAP.XAUSZ          = cXAUSZ     
         BATCH_INPUT_PROVEEDORES_SAP.XDELE          = cXDELE     
         BATCH_INPUT_PROVEEDORES_SAP.XDEZV          = cXDEZV     
         BATCH_INPUT_PROVEEDORES_SAP.XEDIP          = cXEDIP     
         BATCH_INPUT_PROVEEDORES_SAP.XERSR          = cXERSR     
         BATCH_INPUT_PROVEEDORES_SAP.XERSY          = cXERSY     
         BATCH_INPUT_PROVEEDORES_SAP.XNBWY          = cXNBWY     
         BATCH_INPUT_PROVEEDORES_SAP.XPORE          = cXPORE     
         BATCH_INPUT_PROVEEDORES_SAP.XVERR          = cXVERR     
         BATCH_INPUT_PROVEEDORES_SAP.XZEMP          = cXZEMP     
         BATCH_INPUT_PROVEEDORES_SAP.ZAHLS          = cZAHLS     
         BATCH_INPUT_PROVEEDORES_SAP.ZGRUP          = cZGRUP     
         BATCH_INPUT_PROVEEDORES_SAP.ZINDT          = cZINDT     
         BATCH_INPUT_PROVEEDORES_SAP.ZINRT          = cZINRT     
         BATCH_INPUT_PROVEEDORES_SAP.ZOLLA          = cZOLLA     
         BATCH_INPUT_PROVEEDORES_SAP.ZSABE          = cZSABE     
         BATCH_INPUT_PROVEEDORES_SAP.ZTERM          = cZTERM     
         BATCH_INPUT_PROVEEDORES_SAP.BLFM1-ZTERM    = cZTERM     
         BATCH_INPUT_PROVEEDORES_SAP.ZUAWA          = cZUAWA     
         BATCH_INPUT_PROVEEDORES_SAP.ZWELS          = cZWELS.
                                                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportArchivoLsmw) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportArchivoLsmw Procedure 
PROCEDURE exportArchivoLsmw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcTable     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcWhere     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcFile      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piColumns   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piStartData AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piFormatRow AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hQry    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hField  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cCell   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cLen    AS CHARACTER  NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  chApp = DYNAMIC-FUNCTION('openExcelApplication' IN hLibCom, "..\industria\" + pcFile, FALSE).
  IF NOT VALID-HANDLE(chApp) THEN RETURN.
  chSheet = chApp:Sheets:ITEM(1).
  iFila = piStartData.

  CREATE QUERY hQry.
  CREATE BUFFER hBuffer FOR TABLE pcTable.
  hQry:SET-BUFFERS(hBuffer).
  hQry:QUERY-PREPARE("FOR EACH " + pcTable + " " + pcWhere + " NO-LOCK").
  hQry:QUERY-OPEN.
  hQry:GET-FIRST.

  /* loop sobre query */
  DO WHILE NOT hQry:QUERY-OFF-END:
    /* loop sobre columnas de excel */
    cRange = "A" + STRING(piColumns).
    chCell = chSheet:Range(cRange).
    chCell:Activate.
    REPEAT:
      cCell = chCell:VALUE.
      IF cCell = ? THEN LEAVE.
      /* busco el campo en la dynamic query */
      hField = hBuffer:BUFFER-FIELD(cCell).
      IF hField:BUFFER-VALUE <> "" THEN DO:
        /* exporto el dato a excel */
        iCol = chCell:COLUMN.
        cCol = DYNAMIC-FUNCTION('convertToLetter', iCol).
        cRange = cCol + STRING(piFormatRow).
        cLen = IF chSheet:Range(cRange):VALUE = "" THEN "50" ELSE chSheet:Range(cRange):VALUE.
        cRange = cCol + STRING(iFila).
        chSheet:Range(cRange):NumberFormat = "@".
        chSheet:Range(cRange):EntireColumn:AutoFit.
        chSheet:Range(cRange):VALUE = SUBSTRING(hField:BUFFER-VALUE, 1, INT(cLen)) NO-ERROR.
        chSheet:Range(cRange):VALUE = TRIM(chSheet:Range(cRange):VALUE).

      END.
      chCell = chCell:NEXT. 
      chCell:Activate.  
    END.
    iFila = iFila + 1.
    hQry:GET-NEXT().  
      
  END.
  hQry:QUERY-CLOSE().
  DELETE OBJECT hQry.
  chApp:VISIBLE = TRUE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTTElabs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTElabs Procedure 
PROCEDURE fillTTElabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buElabs FOR ttElabs.

  FOR EACH ttElabs.
    DELETE ttElabs.
  END.

  FOR EACH r_productos_calidad_envase 
    NO-LOCK.

    FIND FIRST productos_terminados OF r_productos_calidad_envase NO-LOCK NO-ERROR.
    FIND FIRST calidades OF r_productos_calidad_envase NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF r_productos_calidad_envase NO-LOCK NO-ERROR.
  
    IF NOT AVAILABLE productos_terminados THEN NEXT.
    IF NOT AVAILABLE calidades THEN NEXT.
    IF NOT AVAILABLE envases_prod THEN NEXT.    
  
    cCod = getCodigoProductoElabSap(r_productos_calidad_envase.id_articulo, 
                                    r_productos_calidad_envase.id_calidad, 
                                    r_productos_calidad_envase.id_envase).

    cDes = productos_terminados.descripcion + " - " + 
           calidades.descripcion + " - " + 
           envases_prod.descripcion. 

    cPro = STRING(r_productos_calidad_envase.id_articulo) + "-" +
           STRING(r_productos_calidad_envase.id_calidad) + "-" + 
           STRING(r_productos_calidad_envase.id_envase).

    
    CREATE ttElabs.
    ASSIGN ttElabs.codigo_sap  = cCod
           ttElabs.descripcion = cDes
           ttElabs.flag        = ""
           ttElabs.codigo_prog = cPro
           .

  END.

  FOR EACH ttElabs
      BREAK BY ttElabs.codigo_sap.
    IF LAST-OF(ttElabs.codigo_sap) THEN DO:
      CREATE buElabs.
      ASSIGN buElabs.codigo_sap  = ttElabs.codigo_sap
             buElabs.descripcion = ttElabs.descripcion
             buElabs.flag        = "X"
             buElabs.codigo_pro  = ttElabs.codigo_pro
             .
    END.

  END.

  FOR EACH ttElabs
      WHERE ttElabs.flag <> "X".
    DELETE ttElabs.
  END.

END PROCEDURE.


/*  DISP productos_terminados.id_articulo
         productos_terminados.descripcion
         calidades.id_calidad
         calidades.descripcion 
         envases_prod.id_envase
         envases_prod.descripcion
         cCod FORMAT "x(20)"
         WITH WIDTH 200.
     */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferClientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferclientes Procedure 
PROCEDURE transferclientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO. /*tipo de contratos 100 fruta el resto industria*/
  DEFINE INPUT  PARAMETER pdDes AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHas AS DATE       NO-UNDO.

  DEFINE VARIABLE iNum AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  FOR EACH BATCH_input_clientes
      WHERE IF piTip <> 100 THEN spart = "IN" ELSE spart = "FF".
    DELETE BATCH_input_clientes.
  END.
  
  ASSIGN cBUKRS = cTxtCh + "1000"
         cVKORG = cTxtCh + "1000"
         cSPART = IF piTip <> 100 THEN "IN" ELSE "FF"
         cKKBER = cTxtCh + "1000"
         .

  iNum = 5.

  FOR EACH contratos
      WHERE (IF piTip = 100 THEN contratos.id_tipo_contrato = 100 ELSE contratos.id_tipo_contrato <> 100)
        AND contratos.fecha >= pdDes
        AND contratos.fecha <= pdHas
      BREAK BY contratos.id_cliente.


    IF LAST-OF(contratos.id_cliente) THEN DO:
      /* finds */
      FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.        
      FIND FIRST items_contratos OF contratos NO-LOCK NO-ERROR.
      IF NOT AVAILABLE clientes THEN NEXT.
      IF NOT AVAILABLE items_contratos THEN NEXT.


      ASSIGN cANRED = cTxtCh + "EMPRESA"  
             cNAME1 = IF AVAILABLE clientes THEN putSpaceIfEmpty(CAPS(clientes.razon_social)) ELSE "NO-INFO"
             cNAME2 = cSpace
             cNAME3 = cSpace
             cNAME4 = cSpace
             cSORT1 = IF AVAILABLE clientes THEN SUBSTRING(clientes.nombre, 1, 20) ELSE "NO-INFO"
             cSORT2 = cSpace
             cSTRAS = getDomicilioCliente(clientes.id_cliente)
             cPSTLZ = getCodPostalSap(cLAND1, clientes.codigo_postal) 
             cLAND1 = getPaisFromLeyenda(IF AVAILABLE clientes THEN clientes.pais ELSE cSpace)
             cREGIO = getRegionesSap(cLAND1, clientes.provincia)
             cORT01 = getDescripcionRegion(cLAND1, cREGIO)
             cTELF1 = IF AVAILABLE clientes THEN putSpaceIfEmpty(SUBSTRING(clientes.telefono[1], 1, 35)) ELSE cSpace
             cTELFX = IF AVAILABLE clientes THEN putSpaceIfEmpty(SUBSTRING(clientes.fax, 1, 35)) ELSE cSpace
             cLIFNR = cSpace
             cSTCD1 = getCitde(clientes.citde)
             cSTKZN = cSpace
             cBRSCH = cTxtCh + "0001"
             cKUKLA = cTxtCh + "01"
             cLZONE = cSpace
             cFITYP = cTxtCh + "09"
             cSTCDT = cTxtCh + IF cLAND1 = "AR" THEN "80" ELSE "99" 

             cVTWEG = IF cLAND1 = "AR" THEN "LO" ELSE "EX"
             cKTOKD = IF cLAND1 = "AR" THEN "CARG" ELSE "CEXT"

             /* BKNB1 */
             cAKONT = cTxtCh + getCuentaAsociada(cSPART, cLAND1) /*cuenta para clientes exterior industria*/
             cFDGRV = cTxtCh + "E3" /*grupo de tesoreria clientes mercado externo*/
             cXZVER = "X" /* flag para registrar historial de pagos*/
             cZWELS = cSpace 
             cZTERM = "0060" /*condicion pago de fi*/
             cALTKN = STRING(clientes.id_cliente)
             cGRICD = cSpace
             cGRIDT = cSpace

             /* BKNVA */
             cABLAD = cSpace
             cKNFAK = cSpace
             /* BKNVV */
             cBZIRK = getZonaVentas(cLAND1, cSPART) /*zona de ventas*/
             cVKBUR = cTxtCh + "1001"
             cWAERS = getMonedaClienteFromIdMoneda(items_contratos.id_moneda_origen)
             cVKGRP = getGrupoVendedoresFromPais(cLAND1, cSPART)
             cKDGRP = cTxtCh + "01" /*grupo de clientes*/
             cKONDA = cSpace
             cKALKS = cTxtCh + "2" 
             cVSBED = cTxtCh + "01"
             cVWERK = cTxtCh + IF cSPART = "IN" THEN "A200" ELSE "A100"
             cINCO1 = getIncotermFromContrato(contratos.id_contrato)
             cINCO2 = getDestino(items_contratos.id_destino)
             cZTERM = "0060" /*condicion de pago de sd*/
             cKTGRD = cTxtCh + IF cLAND1 = "AR" THEN "01" ELSE "02"
             cPLTYP = cSpace
             cVERSG = cSpace
             cAUFSD = cSpace
             cSTCD2 = cSpace
             cCOUNC = cSpace
             cKZAZU = "X"
             cLPRIO = cTxtCh + "01" /*analizar utilizar este campo para marca cliente chep*/
             cKUNNR = getNroCliente(iNum, cLAND1, cSPART, clientes.cuit)
             
             NO-ERROR.
      /* BKNBK */
      setDatosBanco(contratos.id_payment_instruction) .
      
      RUN addCliente.
      iNum = iNum + 1.

    END.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferMaterialesFrutaSD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferMaterialesFrutaSD Procedure 
PROCEDURE transferMaterialesFrutaSD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
  
  /* traigo datos de base gabriel */

  FOR EACH buProds.
    DELETE buProds.
  END.

  FOR EACH euroamerica.productos_sap.
    CREATE buProds.
    BUFFER-COPY euroamerica.productos_sap TO buProds.
  END.
  /* disconnect -db euroamerica */

  /* elimino elaborados fruta */
  FOR EACH BATCH_input_materiales_sap
      WHERE SPART = "FF".
    DELETE BATCH_input_materiales_sap.
  END.

  /*************/

  FOR EACH buProds.
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = buProds.id_articulo NO-LOCK NO-ERROR.
    ASSIGN 
      /* GENERAL */
      cMATNR = buProds.id_producto_sap
      cMBRSH = "Z"
      cMTART = "FERT"
      cWERKS = "A100" /*crear los materciales en todos los centros*/
      cLGORT = "1010"
      cMEINS = "CJ"
      cSPART = "FF"
      cMAKTX = STRING(TRIM(buProds.abreviatura_sap), "x(40)")
      cBISMT = STRING(buProds.id_articulo)
      cMATKL = "200"
      cBRGEW = STRING(buProds.bruto)
      cNTGEW = STRING(buProds.neto)
      cGEWEI = "KG"
      cPRCTR = cSpace
      cVOLUM = cSpace 
      cVOLEH = cSpace
      cGROES = cSpace

      /* VENTAS */
      cVKORG = "1000"
      cVTWEG = "EX"
      cSKTOF = "X"
      cVRKME = "PAL"

      cTATY1  = "J1AU"
      cTAXKM1 = "0"
      cTATY2  = "J1AP"
      cTAXKM2 = "0"
      cTATY3  = cSpace
      cTAXKM3 = cSpace
      cTATY4  = cSpace
      cTAXKM4 = cSpace 

      cKONDM = cSpace
      cKTGRM = cSpace
      cMTPOS-MARA = cTxtCh + "NORM"
      cMTPOS = cTxtCh + "NORM"
      cPROVG = cSpace

      /* SOLAPA ventas gral / centro */
      cTRAGR = "0001"
      cLADGR = "0001"

      cEKWSL = cSpace
      cMVGR1 = cSpace
      cMVGR2 = cSpace
      cMVGR3 = cSpace

      cXEIK1 = "X"
      cXEIC1 = "X"
      cXEIV1 = "X"
      cXEIV2 = "X"
      cXEIV4 = "X"
      cXEIV3 = "X"
      cXEIE1 = cSpace
      cXEIE2 = "X"
      cXEID1 = cSpace
      cXEID2 = cSpace
      cXEID3 = cSpace
      cXEIP1 = cSpace
      cXEIA1 = cSpace
      cXEIL1 = "X"
      cXEIQ1 = "X"
      cXEIB1 = "X"
      cXEIG1 = cSpace
      
      
      cSSQSS = cSpace
      cQMPUR = cSpace
      cINSMK = cSpace
      cEKGRP = "A05"
      cKAUTB = cSpace
      cWEBAZ = cSpace
      cBSTME = cSpace
      cVABME = cSpace
      cBESKZ = cSpace
      cMARC-SOBSL = cSpace
      cDISMM = cSpace
      cDISPO = cSpace /*"CH" */
      cPLIFZ = cSpace
      cDZEIT = cSpace
      cPERKZ = cSpace
      cLGPRO = cSpace
      cRGEKZ = cSpace
      cFHORI = cSpace
      cMINBE = cSpace
      cEISBE = cSpace
      cDISLS = cSpace
      cBSTMI = cSpace
      cBSTMA = cSpace
      cBSTFE = cSpace
      cMABST = cSpace
      cBSTRF = cSpace

      cMAABC = cSpace
      cLGRAD = cSpace
      cSTRGR = cSpace

      cVRMOD = cSpace
      cVINT1 = cSpace
      cVINT2 = cSpace
      cALTSL = cSpace
      cSAUFT = cSpace
      cSFEPR = cSpace 
      cSBDKZ = cSpace

      cMTVFP = "CH"
      cDISGR = cSpace
      cRWPRO = cSpace
      cAUTRU = cSpace
      cSHFLG = cSpace
      cSHZET = cSpace

      cSHPRO = cSpace 
      cEISLO = cSpace
      cSFCPF = cSpace
      cXCHPF = "X"
      cFEVOR = cSpace

      cIPRKZ = cSpace
      cVPRSV = "S"
      cVERPR = cSpace
      cSTPRS = "1"
      cPEINH = "1"
      cBKLAS = getCategoriaValoracionFF(buProds.id_producto_sap)
      cXLIFO = cSpace
      cNCOST = cSpace
      cEKALR = cSpace
      cHKMAT = cSpace
      cAWSLS = cSpace
      cSTLAN = cSpace
      cSTLAL = cSpace
      cLOSGR = cSpace
      cEAN11 = cSpace

      cDWERK  = "A100" 
      cTDLINE = productos_terminados.descripcion_ingles + " - VARIETY: " + buProds.variedad

      cMEINH  = "PAL"
      cUMREN  = "1"
      cUMREZ  = "63"

      cPLNTY = cSpace
      cVERSG = cSpace
      cAESZN = cSpace
      cBLATT = cSpace
      cPROFL = "004"
      cEXTWG = "02"
      cPRDHA = getJerarquiaProducto(buProds.id_producto_sap)
      cSTAWN = getPosicionArancelaria(buProds.id_producto_sap, cNTGEW, cBRGEW)
    .
    RUN addMaterial.
 
  END.
   
  /*************/


/* 
  FOR EACH ttMat.
    DELETE ttMat.
  END.

  FOR EACH BATCH_input_materiales_sap
      WHERE batch_input_materiales_sap.SPART = "FF".
    CREATE ttMat.
    BUFFER-COPY BATCH_input_materiales_sap TO ttMat.
  END.

  RUN generateExcel.p (INPUT TABLE ttMat,
                      INPUT " MATERIALES ELABORADOS FRUTA FRESCA SAP",
                      INPUT ""  ,
                      INPUT 7,
                      INPUT 8,
                      INPUT "Arial",
                      INPUT 8).
 */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferMaterialesIndustriaSD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferMaterialesIndustriaSD Procedure 
PROCEDURE transferMaterialesIndustriaSD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.


  RUN fillTTElabs.

  FOR EACH BATCH_input_materiales_sap
      WHERE SPART = "IN".
    DELETE BATCH_input_materiales_sap.
  END.
  
  FOR EACH ttElabs
      WHERE NUM-ENTRIES(ttElabs.codigo_sap, "-") = 5
      NO-LOCK.

    iArt = INTEGER(ENTRY(1, ttElabs.codigo_prog, "-")).
    ASSIGN 
      /* GENERAL */
      cMATNR = ttElabs.codigo_sap
      cMBRSH = "Z"
      cMTART = IF (ENTRY(1, ttElabs.codigo_sap, "-") = "LJT" OR ENTRY(1, ttElabs.codigo_sap, "-") = "LTR") THEN "HALB" ELSE "FERT" /*para turbios semielaborados*/
      cWERKS = "A200" /*crear los materciales en todos los centros*/
      cLGORT = "2020"
      cMEINS = getUnidadMedidaBaseIN(ttElabs.codigo_sap)
      cSPART = "IN"
      cMAKTX = STRING(TRIM(ttElabs.descripcion), "x(40)")
      cBISMT = STRING(ttElabs.codigo_pro)
      cMATKL = "201"
      cBRGEW = cTxtCh + getKilosBrutosEnvase(ttElabs.codigo_prog)
      cNTGEW = cTxtCh + getKilosNetosEnvase(ttElabs.codigo_prog)
      cGEWEI = cTxtCh + "KG"
      cPRCTR = "5200"
      cVOLUM = cTxtCh + cSpace 
      cVOLEH = cSpace
      cGROES = cSpace

      /* VENTAS */
      cVKORG = cTxtCh + "1000"
      cVTWEG = cTxtCh + "EX"
      cSKTOF = cTxtCh + "X"
      cVRKME = cSpace

      cTATY1  = "J1AU"
      cTAXKM1 = "0"
      cTATY2  = "J1AP"
      cTAXKM2 = "0"
      cTATY3  = cSpace
      cTAXKM3 = cSpace
      cTATY4  = cSpace
      cTAXKM4 = cSpace 

      cKONDM = cSpace
      cKTGRM = cSpace
      cMTPOS-MARA = cTxtCh + "NORM"
      cMTPOS = cTxtCh + "NORM"
      cPROVG = cSpace

      /* SOLAPA ventas gral / centro */
      cTRAGR = "0003"
      cLADGR = "0001"

      cEKWSL = cSpace
      cMVGR1 = cSpace
      cMVGR2 = cSpace
      cMVGR3 = cSpace

      cXEIK1 = "X"
      cXEIC1 = "X"
      cXEIV1 = "X"
      cXEIV2 = "X"
      cXEIV4 = "X"
      cXEIV3 = "X"
      cXEIE1 = cSpace
      cXEIE2 = "X"
      cXEID1 = cSpace
      cXEID2 = cSpace
      cXEID3 = cSpace
      cXEIP1 = cSpace
      cXEIA1 = cSpace
      cXEIL1 = "X"
      cXEIQ1 = "X"
      cXEIB1 = "X"
      cXEIG1 = cSpace
      
      
      cSSQSS = cSpace
      cQMPUR = cSpace
      cINSMK = cSpace
      cEKGRP = cSpace
      cKAUTB = cSpace
      cWEBAZ = cSpace
      cBSTME = cSpace
      cVABME = cSpace
      cBESKZ = cSpace
      cMARC-SOBSL = cSpace
      cDISMM = cSpace
      cDISPO = cSpace
      cPLIFZ = cSpace
      cDZEIT = cSpace
      cPERKZ = cSpace
      cLGPRO = cSpace
      cRGEKZ = cSpace
      cFHORI = cSpace
      cMINBE = cSpace
      cEISBE = cSpace
      cDISLS = cSpace
      cBSTMI = cSpace
      cBSTMA = cSpace
      cBSTFE = cSpace
      cMABST = cSpace
      cBSTRF = cSpace

      cMAABC = cSpace
      cLGRAD = cSpace
      cSTRGR = cSpace

      cVRMOD = cSpace
      cVINT1 = cSpace
      cVINT2 = cSpace
      cALTSL = cSpace
      cSAUFT = cSpace
      cSFEPR = cSpace 
      cSBDKZ = cSpace

      cMTVFP = cSpace
      cDISGR = cSpace
      cRWPRO = cSpace
      cAUTRU = cSpace
      cSHFLG = cSpace
      cSHZET = cSpace

      cSHPRO = cSpace 
      cEISLO = cSpace
      cSFCPF = cSpace
      cXCHPF = "X"
      cFEVOR = cSpace

      cIPRKZ = cSpace
      cVPRSV = cSpace
      cVERPR = cSpace
      cSTPRS = cSpace
      cPEINH = cSpace
      cBKLAS = getCategoriaValoracion(ttElabs.codigo_sap)
      cXLIFO = cSpace
      cNCOST = cSpace
      cEKALR = cSpace
      cHKMAT = cSpace
      cAWSLS = cSpace
      cSTLAN = cSpace
      cSTLAL = cSpace
      cLOSGR = cSpace
      cEAN11 = cSpace

      cDWERK  = "A200" 
      cTDLINE = getTextoFactura(ttElabs.codigo_prog)

      cMEINH  = IF ENTRY(5, ttElabs.codigo_sap, "-") <> "400" THEN getUnidadConversion(ttElabs.codigo_sap) ELSE cSpace
      cUMREN  = IF ENTRY(5, ttElabs.codigo_sap, "-") <> "400" THEN "1" ELSE cSpace
      cUMREZ  = IF ENTRY(5, ttElabs.codigo_sap, "-") <> "400" THEN getKilosNetosEnvase(ttElabs.codigo_prog) ELSE cSpace

      cPLNTY = cSpace
      cVERSG = cSpace
      cAESZN = cSpace
      cBLATT = cSpace
      cPROFL = "004"
      cEXTWG = "05"
      cSTAWN = getPosicionArancelaria(ttElabs.codigo_sap, cNTGEW, cBRGEW)
      cPRDHA = getJerarquiaProducto(ttElabs.codigo_sap)
    .
    RUN addMaterial.
  END.
  
/* 
  FOR EACH BATCH_input_materiales_sap
      WHERE spart = "IN".
    CREATE ttMat.
    BUFFER-COPY BATCH_input_materiales_sap TO ttMat.
  END.

  RUN generateExcel.p (INPUT TABLE ttMat,
                      INPUT " MATERIALES ELABORADOS INDUSTRIA SAP",
                      INPUT ""  ,
                      INPUT 7,
                      INPUT 8,
                      INPUT "Arial",
                      INPUT 8).
  */  
END PROCEDURE.

/* 
OUTPUT TO d:\elabs.txt.
  FOR EACH ttElabs
      WHERE NUM-ENTRIES(ttElabs.codigo_sap, "-") >= 4.

    DISP codigo_sap FORMAT "x(20)" ";" 
         descripcion FORMAT "x(100)" ";" 
         flag WITH WIDTH 200.
  END.
  OUTPUT CLOSE.
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferMaterialesMM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferMaterialesMM Procedure 
PROCEDURE transferMaterialesMM :
/*------------------------------------------------------------------------------
  Purpose: Recorremos la tabla de Articulos, para volcar a la estructura que se
           preparo para la migracion al sistema de SAP.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pdDes AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdHas AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdRub AS INTEGER NO-UNDO.

  DEFINE VARIABLE iNum     AS INTEGER NO-UNDO.
  DEFINE VARIABLE v_precio AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v_items  AS INTEGER NO-UNDO INITIAL 0.
  DEFINE VARIABLE hLibCom  AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  
  FOR EACH BATCH_input_materiales_sap:
     DELETE BATCH_input_materiales_sap.
  END.
  
  INPUT FROM d:\temp\categoria_valoracion.csv.
  REPEAT:
     CREATE t_catvalmm.
     IMPORT DELIMITER "," t_catvalmm.
  END.

  /*---------------------------------------------------------------------------
  Tomamos los materiales viegentes unicamente: fecha_baja = ?
  ---------------------------------------------------------------------------*/
  FOR EACH articulos WHERE articulos.fecha_baja = ? AND
      (IF pdDes                 <> 0     AND
          articulos.id_articulo >= pdDes AND
          articulos.id_articulo <= pdHas THEN
            TRUE
       ELSE
          IF pdDes = 0  and
             articulos.id_rubro = pdRub THEN
             TRUE
          ELSE
             FALSE
      )
      NO-LOCK
  BY articulos.id_articulo DESCENDING: /*--- VER DE ORDENAR DE OTRA MANERA SI ES NECESARIO ---*/

      /****
      v_items = v_items + 1.
      IF v_items > 20 THEN
          LEAVE.
      *****/

      FIND t_catvalmm OF articulos NO-LOCK NO-ERROR.

      IF NOT AVAILABLE T_CATVALMM THEN NEXT.

      /*----- PARA DEFINIR EL GRUPO DE ABASTECIMIENTO ------*/
      FIND FIRST r_grupo_articulo WHERE
          r_grupo_articulo.id_articulo = articulos.id_articulo AND
          r_grupo_articulo.id_grupo    > 0
          NO-LOCK NO-ERROR.
      /*----- PARA DEFINIR UN PRECIO STANDARD DEL ARTICULO A PARTIR DE
              ULTIMA COMPRA ------*/
      FIND LAST items_pedidos USE-INDEX articulo_fecha WHERE
          items_pedidos.id_articulo =  articulos.id_articulo AND
          items_pedidos.fecha       <> ? NO-LOCK NO-ERROR.
      ASSIGN v_precio = IF AVAILABLE items_pedidos THEN
                           items_pedidos.costo_final_pesos
                        ELSE
                           articulos.costo_final_pesos.

      ASSIGN cMATNR       = STRING(articulos.id_articulo)
             cMBRSH       = "P"
             cMTART       = DYNAMIC-FUNCTION("getTipoMaterial", INPUT articulos.id_rubro)
             cWERKS       = "A100"
             cVKORG       = "1000"
             cTATY2       = "/"
             cTAXKM2      = "/"
             cTATY3       = "/"
             cTAXKM3      = "/"
             cTATY4       = "/"
             cTAXKM4      = "/"
             cVTWEG       = "LO"
             cXEIK1       = "/"
             cXEIC1       = "/"
             cXEIV1       = "/"
             cXEIV2       = "/"
             cXEIV4       = "/"
             cXEIV3       = "/"
             cXEIE1       = "/"
             cXEIE2       = "/"
             cXEID1       = "/"
             cXEID2       = "/"
             cXEID3       = "/"
             cXEIP1       = "/"
             cXEIA1       = "/"
             cXEIL1       = "/"
             cXEIQ1       = "/"
             cXEIB1       = "/"
             cXEIG1       = "/"
             cLGORT       = "1080"
             cMEINS       = dynamic-function("getUnidadMedida", input articulos.id_um)
             cMAKTX       = STRING(articulos.descripcion,"x(40)")
             cBISMT       = IF articulos.id_hazard = 3 THEN
                               "H-" + TRIM(STRING(articulos.id_articulo,">>>>>>9"))
                            ELSE
                               TRIM(STRING(articulos.id_articulo,">>>>>>9"))
             cMATKL       = STRING(r_grupo_articulo.id_grupo)
             cSPART       = "OT"
             
             cBRGEW       = "/"
             cNTGEW       = "/"
             cGEWEI       = "/"
             cPRCTR       = "/"
             cVOLUM       = "/"
             cVOLEH       = "/"
             cGROES       = "/"
             cSKTOF       = "X"
             cVRKME       = "/"
             cTATY1       = "/"
             cTAXKM1      = "/"
             cTATY2       = "/"
             cTAXKM2      = "/"
             cTATY3       = "/"
             cTAXKM3      = "/"
             cTATY4       = "/"
             cTAXKM4      = "/"
             cKONDM       = "01"
             cKTGRM       = "03"
             cMTPOS-MARA  = "VOLL"
             cMTPOS       = "VOLL"
             cPROVG       = "01"
             cTRAGR       = "0001"
             cLADGR       = "/"
             cEKWSL       = "3"
             cMVGR1       = "/"
             cMVGR2       = "/"
             cMVGR3       = "/"
             cSSQSS       = "0000"
             cQMPUR       = "X"
             cINSMK       = "/"
             cEKGRP       = "100"
             cKAUTB       = "/"
             cWEBAZ       = "0"
             cBSTME       = "/"
             cVABME       = "1"
             cBESKZ       = "/"
             cMARC-SOBSL  = "60".
      ASSIGN cDISMM       = "ND"
             cDISPO       = "/" /* POR AHORA NO HAY PLANIFICACION */
             cPLIFZ       = "0"
             cDZEIT       = "0"
             cPERKZ       = "M"
             cLGPRO       = "0001"
             cRGEKZ       = "1"
             cFHORI       = "/" /* POR AHORA NO HAY QUE DETALLAR, TEMA PLANIFICACION */
             cMINBE       = "0"
             cEISBE       = "0"
             cDISLS       = "/"
             cBSTMI       = "0"
             cBSTMA       = "0"
             cBSTFE       = "0"
             cMABST       = "0"
             cBSTRF       = "0"
             cMAABC       = "/"
             cLGRAD       = "/"
             cSTRGR       = "00"
             cVRMOD       = "/"
             cVINT1       = "0"
             cVINT2       = "0"
             cALTSL       = "/"
             cSAUFT       = "/"
             cSFEPR       = "0001"
             cSBDKZ       = "/"
             cMTVFP       = "KP"  /*-- GRUPO SIN VERIFICAR --*/
             cDISGR       = "0001"
             cRWPRO       = "/"
             cAUTRU       = "X"
             cSHFLG       = "/"
             cSHZET       = "0"
             cSHPRO       = "/"
             cEISLO       = "/"
             cSFCPF       = "/"
             cXCHPF       = "/"
             cFEVOR       = "001"
             cIPRKZ       = "/"
             cVPRSV       = "/"
             cVERPR       = "/"
             cSTPRS       = STRING(v_precio,">>>>>>>>>>9.99")
             cPEINH       = "/"
             cBKLAS       = IF AVAILABLE t_catvalmm THEN
                               STRING(t_catvalmm.categoria)
                            ELSE
                               "/"
             cXLIFO       = "/"
             cNCOST       = "/"
             cEKALR       = "/"
             cHKMAT       = "/"
             cAWSLS       = "000001"
             cSTLAN       = "/"
             cSTLAL       = "1"
             cLOSGR       = "1"
             cEAN11       = "/"
             cDWERK       = "/"
             cTDLINE      = "/"
             cMEINH       = "/"
             cUMREN       = "/"
             cUMREZ       = "/"
             cPLNTY       = "/"
             cVERSG       = "/"
             cAESZN       = "/"
             cBLATT       = "/"
             cPROFL       = DYNAMIC-FUNCTION("getSedronarHazard", INPUT articulos.id_hazard,
                                                                  INPUT articulos.es_sedronar)
             cEXTWG       = "01". /* VER LUEGO LA NUEVA RECLASIFICACION DE CRITICIDAD */

      /* BKNBK */
      RUN addMaterial.

  END.

  /*---------------------------------------------------------
  RUN exportArchivoLSMW (INPUT "batch_input_materiales_sap",
                         INPUT "LSMWmateriales.xls",
                         INPUT 7,
                         INPUT 16).
  ---------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferProveedores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferProveedores Procedure 
PROCEDURE transferProveedores :
/*------------------------------------------------------------------------------
  Purpose: Para la Migracion de datos del Proveedor    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ip_prov AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ip_pais AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iNum      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibCom   AS HANDLE.
  DEFINE VARIABLE v_moneda  AS INTEGER.
  DEFINE VARIABLE v_diasent AS INTEGER.
  DEFINE VARIABLE v_nuevocodigo AS CHARACTER.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  
  /*---------------------------------------------------------------------------
  Solamente tomamos los Proveedores activos (estado = true)
  ---------------------------------------------------------------------------*/
  FOR EACH BATCH_input_proveedores_sap:
      DELETE BATCH_input_proveedores_sap.
  END.


  FOR EACH proveedores WHERE proveedores.estado AND
                             /*-- luego sacar --*/
                             (IF ip_pais <> "" THEN
                                 proveedores.pais =  ip_pais
                              ELSE
                                 TRUE)           AND
                             proveedores.id_proveedor <= ip_prov
                             NO-LOCK
  BY proveedores.id_proveedor:
      FIND LAST pedidos USE-INDEX proveedores WHERE
          pedidos.id_proveedor     =  proveedores.id_proveedor AND
          pedidos.fecha_entrega[1] >= pedidos.fecha
          NO-LOCK NO-ERROR.

      v_diasent = IF AVAILABLE pedidos THEN
                    pedidos.fecha_entrega[1] - pedidos.fecha
                  ELSE
                    0.

      /*-------------------------------------------------
      Defino la Moneda del Proveedor, desde la O.COMPRA
      -------------------------------------------------*/
      FIND LAST pedidos USE-INDEX proveedores WHERE
                pedidos.id_proveedor = proveedores.id_proveedor NO-LOCK NO-ERROR.
      ASSIGN v_moneda =  IF AVAILABLE pedidos THEN
                            pedidos.id_moneda_origen
                         ELSE
                            1.

      /*-------------------------------------------------
      busca nueva numeracion de proveedores.
      -------------------------------------------------*/

      FIND prog2sap.r_prov_codsap WHERE r_prov_codsap.id_proveedor = proveedores.id_proveedor AND
                               r_prov_codsap.ktokk        = IF proveedores.pais MATCHES "*arg*" THEN
                                                               "PNAC"
                                                            ELSE
                                                               "PEXT".

      FIND prog2sap.r_prov_codsap WHERE prog2sap.r_prov_codsap.id_proveedor = proveedores.id_proveedor AND
                               prog2sap.r_prov_codsap.ktokk        = IF proveedores.pais MATCHES "*arg*" THEN
                                                                       "PNAC"
                                                                     ELSE
                                                                       "PEXT"

                               NO-LOCK NO-ERROR.
      /****
      v_nuevocodigo = IF AVAILABLE r_prov_codsap THEN
                        IF proveedores.pais MATCHES "*arg*" THEN
                           r_prov_codsap.id_provsap + 1000000000
                        ELSE
                           r_prov_codsap.id_provsap
                      ELSE
                        IF proveedores.pais MATCHES "*arg*" THEN
                           proveedores.id_proveedor + 1000000000
                        ELSE
                           proveedores.id_proveedor.
      ****/

      v_nuevocodigo = IF proveedores.pais MATCHES "*arg*" THEN
                         SUBSTRING(proveedores.cuit,1,10)
                      ELSE
                         IF AVAILABLE r_prov_codsap THEN
                            STRING(r_prov_codsap.id_provsap)
                         ELSE
                            STRING(proveedores.id_proveedor).
      

      ASSIGN cABUEB      = "/"
             cACTSS      = "/"
             cAGREL      = "/"
             cAKONT      = IF proveedores.pais = "Argentina" THEN
                              "210100001"
                           ELSE
                              "210100002"
             cALTKN      = STRING(proveedores.id_proveedor,">>>>>>>>9")
             cANRED      = "/"
             cBAHNS      = "/"
             cBBBNR      = "/"
             cBBSNR      = "/"
             cBEGRU      = "/"
             cBLIND      = "/"
             cBLNKZ      = "/"
             cBOIND      = "/"
             cBOLRE      = "/"
             cBOPNR      = "/"
             cBRSCH      = DYNAMIC-FUNCTION("getRamo", INPUT proveedores.id_proveedor)
             cBSTAE      = "/"
             cBUBKZ      = "/"
             cBUKRS      = "1000"
             cBUSAB      = "/"
             cCERDT      = "/"
             cDATLT      = "/"
             cDATLZ      = "/"
             cDLGRP      = "/"
             cDTAMS      = "/"
             cDTAWS      = "/"
             cEIKTO      = "/"
             cEKGRP      = "/"
             cEKORG      = "A100" /* A100-Argentina, U100-Uruguay, 0001-Otros */
             cEMNFR      = "/"
             cESRNR      = "/"
             cEXPVZ      = "/"
             cFDGRV      = "A1" /* ERROR despues clasificar */
             cFISKN      = "/"
             cFITYP      = DYNAMIC-FUNCTION("getCondicionIVA", INPUT proveedores.id_iva_contribuyente)
             cFRGRP      = "/"
             cGBDAT      = "/"
             cGBORT      = "/"
             cGRICD      = "/"
             cGRIDT      = "/"
             cGUZTE      = "/"
             cHBKID      = "/"
             cINCO1      = "/"
             cINCO2      = "/"
             cINTAD      = "/"
             cIPISP      = "/"
             cJ_1KFREPRE = "/"
             cJ_1KFTBUS  = "/"
             cJ_1KFTIND  = "/"
             cKALSK      = IF proveedores.pais MATCHES "*arg*" THEN
                              "01"
                           ELSE
                              "02"
             cKONZS      = "/"
             cKRAUS      = "/"
             cKTOCK      = "/"
             cKTOKK      = IF proveedores.pais MATCHES "*arg*" THEN 
                              "PNAC" /*- ERROR Hacer la Funcion por tema Fondo FIJO */
                           ELSE
                              "PEXT"
             cKULTG      = "/"
             cKUNNR      = v_nuevocodigo
             cKVERM      = "/"
             cKZABS      = "/"
             cKZAUT      = "/"
             cKZRET      = "/"
             cLAND1      = DYNAMIC-FUNCTION("getPaisFromLeyenda", INPUT proveedores.pais)
             cLEBRE      = "/"
             cLFABC      = "A"
             cLFRHY      = "/"
             cLFURL      = "/".
      ASSIGN cLIBES      = "/"
             cLIFNR      = STRING(v_nuevocodigo,"9999999999")
             cLIPRE      = "/"
             cLISER      = "/"
             cLNRZA      = "/"
             cLNRZB      = "/"
             cLNRZE      = "/"
             cLOEVM      = "/"
             cLTSNA      = "/"
             cLZONE      = "/"
             cMEGRU      = "/"
             cMEPRF      = "/"
             cMGRUP      = "/"
             cMINBW      = "/"
             cMINDK      = "/"
             cMRPPP      = "/"
             cNAME1      = proveedores.nombre
             cNAME2      = "/"
             cNAME3      = "/"
             cNAME4      = "/"
             cNODEL      = "/"
             cNRGEW      = "/"
             cORT01      = proveedores.localidad
             cORT02      = "/"
             cPAPRF      = "/"
             cPERNR      = "/"
             cPFACH      = "/"
             cPFORT      = "/"
             cPLIFZ      = STRING(v_diasent,">>9")
             cPODKZB     = "/"
             cPRFRE      = "/"
             cPROFS      = "/"
             cPSTL2      = "/"
             cPSTLZ      = STRING(proveedores.postal,"9999999999")
             cQLAND      = "/"
             cQSBGR      = "/"
             cQSREC      = "/"
             cQSSKZ      = "/"
             cQSSYS      = "/"
             cQSSYSDAT   = "/"
             cQSZDT      = "/"
             cQSZNR      = "/"
             cRDPRF      = "/"
             cREGIO      = DYNAMIC-FUNCTION("getProvincia", INPUT proveedores.provincia)
             cREGSS      = "/"
             cREPRF      = "X"
             cREVDB      = "/"
             cSCACD      = "/"
             cSENDE      = "/"
             cSEXKZ      = "/"
             cSFRGR      = "/"
             cSKRIT      = "/"
             cSORT1      = SUBSTRING(proveedores.nombre,1,10)
             cSORT2      = "/"
             cSPERM      = "/"
             cSPERQ      = "/"
             cSPERR      = "/"
             cSPRAS      = "S"
             cSTCD1      = proveedores.cuit
             cSTCD2      = "/"
             cSTCD3      = "/"
             cSTCD4      = "/"
             cSTCDT      = "80"
             cSTCEG      = "/"
             cSTGDL      = "/".
      ASSIGN cSTKZA      = "/"
             cSTKZN      = "/"
             cSTKZU      = "/"
             cSTRAS      = REPLACE(proveedores.domicilio,CHR(248),"")
             cSTYPE      = "/"
             cTAXBS      = "/"
             cTBNAM      = "/"
             cTCODE      = "/"
             cTELBX      = "/"
             cTELF1      = proveedores.telefono[1]
             cTELF2      = proveedores.telefono[2]
             cTELFX      = "/"
             cTELTX      = "/"
             cTELX1      = "/"
             cTLFNS      = "/"
             cTLFXS      = "/"
             cTOGRR      = "001" /* obligatorio y el valor debe ser 001 */
             cTOGRU      = "/"
             cTXJCD      = "/"
             cUMSAE      = "/"
             cUZAWE      = "/"
             cVBUND      = "100000"
             cVENSL      = "/"
             cVERKF      = "/"
             cVSBED      = "/"
             cVZSKZ      = "/"
             cWAERS      = DYNAMIC-FUNCTION("getMonedaClienteFromIdMoneda", INPUT v_moneda)
             cWEBRE      = "X"
             cWEBTR      = "/"
             cWERKR      = "/"
             cWERKS      = "/"
             cWITHT      = "/"
             cWT_EXDF    = "/"
             cWT_EXDT    = "/"
             cWT_EXNR    = "/"
             cWT_EXRT    = "/"
             cWT_SUBJCT  = "/"
             cWT_WITHCD  = "/"
             cWT_WTEXRS  = "/"
             cWT_WTSTCD  = "/"
             cXAUSZ      = "/"
             cXDELE      = "/"
             cXDEZV      = "/"
             cXEDIP      = "/"
             cXERSR      = "/"
             cXERSY      = "/"
             cXNBWY      = "/"
             cXPORE      = "/"
             cXVERR      = "/"
             cXZEMP      = "/"
             cZAHLS      = "/"
             cZGRUP      = "/"
             cZINDT      = "/"
             cZINRT      = "/"
             cZOLLA      = "/"
             cZSABE      = "/"
             cZTERM      = "0030" /* ERROR hay que tratar de matchear */
             cZUAWA      = "/"
             cZWELS      = "/".
                          
      /* BKNBK */         
      
      RUN addProveedor.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferStockIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferStockIndustria Procedure 
PROCEDURE transferStockIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cFec AS CHARACTER  NO-UNDO.

  FOR EACH BATCH_input_stock_sap.
    DELETE BATCH_input_stock_sap.
  END.

  /* productos terminados */
  FOR EACH BATCH_input_materiales_sap
      WHERE BATCH_input_materiales_sap.SPART = "IN"
      NO-LOCK.

    iArt = INTEGER(ENTRY(1, BISMT, "-")) .
    iCal = INTEGER(ENTRY(2, BISMT, "-")).
    iEnv = INTEGER(ENTRY(3, BISMT, "-")).

    FOR EACH tambores_industria
        WHERE tambores_industria.fecha                >= DATE('01/01/2007')
          AND tambores_industria.id_articulo           = iArt
          AND tambores_industria.id_calidad            = iCal
          AND tambores_industria.id_envase             = iEnv
          AND tambores_industria.id_locacion_ubicacion = 4
        BREAK BY tambores_industria.nromov.
      i = i + 1.
      fKil = fKil + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        cFec = STRING(YEAR(TODAY)) +  STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
        CREATE BATCH_input_stock_sap.
        ASSIGN BATCH_input_stock_sap.bldat = cFec
               BATCH_input_stock_sap.budat = cFec
               BATCH_input_stock_sap.bktxt = "Saldo Inicial (nromov: " + STRING(tambores_industria.nromov) + ")"
               BATCH_input_stock_sap.bwart = "561"
               BATCH_input_stock_sap.matnr = BATCH_input_materiales_sap.matnr
               BATCH_input_stock_sap.WERKS = "A200"
               BATCH_input_stock_sap.LGORT = "2020"
               BATCH_input_stock_sap.charg = STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio)
               BATCH_input_stock_sap.erfmg = STRING(fKil)
               BATCH_input_stock_sap.erfme = getUnidadMedidaBaseIN(BATCH_input_materiales_sap.matnr)
               BATCH_input_stock_sap.sgtxt = cSpace
               BATCH_input_stock_sap.xeiv1 = cSpace
               BATCH_input_stock_sap.xeiv2 = cSpace
               BATCH_input_stock_sap.xeiv4 = cSpace
               BATCH_input_stock_sap.xeiv3 = cSpace
               .
        i = 0.
        fKil = 0.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferStockInsumos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferStockInsumos Procedure 
PROCEDURE transferStockInsumos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE v_alm AS CHARACTER  FORMAT "x(04)" NO-UNDO.
  DEFINE VARIABLE v_cen AS CHARACTER  FORMAT "x(04)" NO-UNDO.

  INPUT FROM d:\temp\categoria_valoracion.csv.
  REPEAT:
     CREATE t_catvalmm.
     IMPORT DELIMITER "," t_catvalmm.
  END.

  INPUT FROM d:\control_stock.txt.
  REPEAT:
     CREATE t_stock.
     IMPORT DELIMITER ";" t_stock.
  END.
  INPUT CLOSE.

  OUTPUT TO d:\temp\saldos_stock.txt.
  FOR EACH articulos NO-LOCK:

     FIND t_catvalmm OF articulos NO-LOCK NO-ERROR.
     IF NOT AVAILABLE T_CATVALMM THEN NEXT.

     FOR EACH t_stock WHERE t_stock.id_articulo = articulos.id_articulo AND
         (t_stock.id_sucursal = 99 OR
          t_stock.id_sucursal = 52 OR
          t_stock.id_sucursal = 51):

        v_alm = DYNAMIC-FUNCTION("getAlmacenInsumo", INPUT t_stock.id_sucursal).
        v_cen = DYNAMIC-FUNCTION("getCentroInsumo", INPUT t_stock.id_sucursal).

        EXPORT DELIMITER ";"
               articulos.id_articulo
               "A100"
               v_alm
               "/"
               t_stock.cantidad
               dynamic-function("getUnidadMedida", input articulos.id_um)
               .
     END.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-convertToLetter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convertToLetter Procedure 
FUNCTION convertToLetter RETURNS CHARACTER
  (piCol AS INTEGER):

  DEFINE VARIABLE dPartialValue   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCumSum         AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iModulus        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStringPosition AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i               AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cTempString     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet            AS CHARACTER  NO-UNDO.

  iStringPosition = 0.
  dCumSum = 0 .
  cTempString = "".

  DO WHILE piCol > dCumSum:
    dPartialValue = INT(TRUNCATE((piCol - dCumSum - 1) / EXP(26, iStringPosition), 0)).
    iModulus = dPartialValue - ((TRUNCATE(dPartialValue / 26, 0)) * 26).
    cTempString = CHR(iModulus + 65) + cTempString.
    iStringPosition = iStringPosition + 1.
    dCumSum = 0.
    DO i = 1 TO iStringPosition + 1:
      dCumSum = (dCumSum + 1) *  26.
      i = i + 1.
    END.
  END.

  cRet = cTempString.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlmacenInsumo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAlmacenInsumo Procedure 
FUNCTION getAlmacenInsumo RETURNS CHARACTER
  (INPUT ip_suc AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER NO-UNDO.

  CASE ip_suc:
      WHEN 51  THEN "1060".
      WHEN 52  THEN "1070".
      WHEN 99  THEN "1080".
      WHEN 152 THEN "1090".
      WHEN 70  THEN "2010".
      WHEN 151 THEN "2020".
      WHEN 153 THEN "2030".
      WHEN 161 THEN "2040".
      WHEN 2   THEN "3010".
      WHEN 3   THEN "3020".
      WHEN 4   THEN "3030".
      WHEN 5   THEN "3040".
      WHEN 6   THEN "3050".
      WHEN 7   THEN "3060".
      WHEN 8   THEN "3070".
      WHEN 9   THEN "3080".
      WHEN 10  THEN "3090".
      WHEN 12  THEN "3110".
      WHEN 13  THEN "3120".
      WHEN 14  THEN "3130".
      WHEN 15  THEN "3140".
      WHEN 16  THEN "3150".
      WHEN 17  THEN "3160".
      WHEN 18  THEN "3170".
      WHEN 19  THEN "3180".
      WHEN 20  THEN "3190".
      WHEN 21  THEN "3200".
      WHEN 23  THEN "3210".
      WHEN 24  THEN "3220".
      WHEN 25  THEN "3230".
      WHEN 28  THEN "3240".
      WHEN 164 THEN "3250".
      WHEN 165 THEN "3260".
      WHEN 167 THEN "3280".
      WHEN 168 THEN "3290".
      WHEN 11  THEN "3310".
      WHEN 58  THEN "3340".
      WHEN 163 THEN "3343".
      WHEN 170 THEN "3345".
      WHEN 63  THEN "3482".
      OTHERWISE "0000".
  END CASE.

  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBancoSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBancoSap Procedure 
FUNCTION getBancoSap RETURNS CHARACTER
  (iBank AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferHandleFromTableName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBufferHandleFromTableName Procedure 
FUNCTION getBufferHandleFromTableName RETURNS HANDLE
  (ipcTable AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hRet AS HANDLE     NO-UNDO.

  CASE ipcTable:
    WHEN "batch_input_clientes_sap" THEN hRet = BUFFER BATCH_input_clientes_sap:HANDLE.
    WHEN "batch_input_materiales_sap" THEN hRet = BUFFER BATCH_input_materiales_sap:HANDLE.
  END CASE.

  RETURN hRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalidadAndPulpaSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidadAndPulpaSAP Procedure 
FUNCTION getCalidadAndPulpaSAP RETURNS CHARACTER
  (piCalidad AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  CASE piCalidad:
    WHEN 603 THEN cRet = "400-00-".
    WHEN 53  THEN cRet = "500-00-".
    WHEN 59  THEN cRet = "807-00-".
    WHEN 795 THEN cRet = "285-00-".
    WHEN 51  THEN cRet = "400-00-".
    WHEN 66  THEN cRet = "400-00-".
    WHEN 70  THEN cRet = "400-00-".
    WHEN 80  THEN cRet = "400-19-".
    WHEN 109 THEN cRet = "400-00-".
    WHEN 82  THEN cRet = "400-00-".
    WHEN 64  THEN cRet = "450-00-".
    WHEN 60  THEN cRet = "460-00-".
    WHEN 416 THEN cRet = "480-00-".
    WHEN 68  THEN cRet = "485-00-".

    WHEN 65  THEN cRet = "500-00-".
    WHEN 313 THEN cRet = "500-01-".
    WHEN 342 THEN cRet = "500-03-".
    WHEN 324 THEN cRet = "500-15-".
    WHEN 325 THEN cRet = "500-19-".
    WHEN 52  THEN cRet = "500-00-".
    WHEN 337 THEN cRet = "500-06-".
    WHEN 644 THEN cRet = "500-22-".
    WHEN 330 THEN cRet = "500-09-".
    WHEN 327 THEN cRet = "500-02-".
    WHEN 338 THEN cRet = "500-13-".
    WHEN 340 THEN cRet = "500-14-".
    WHEN 410 THEN cRet = "500-16-".
    WHEN 335 THEN cRet = "500-18-".
    WHEN 87  THEN cRet = "500-15-".
    WHEN 319 THEN cRet = "500-00-".
    WHEN 412 THEN cRet = "500-00-".

    WHEN 328 THEN cRet = "560-00-".
    WHEN 423 THEN cRet = "600-00-".
    WHEN 424 THEN cRet = "700-00-".

    WHEN 331 THEN cRet = "200-11-".
    WHEN 414 THEN cRet = "220-00-".
    WHEN 639 THEN cRet = "220-06-".
    WHEN 432 THEN cRet = "325-02-".
    WHEN 320 THEN cRet = "400-01-".
    WHEN 323 THEN cRet = "400-02-".
    WHEN 316 THEN cRet = "400-18-".
    WHEN 326 THEN cRet = "400-09-".
    WHEN 329 THEN cRet = "400-11-".
    WHEN 250 THEN cRet = "400-12-".
    WHEN 336 THEN cRet = "400-16-".
    WHEN 67  THEN cRet = "400-00-".
    WHEN 57  THEN cRet = "400-21-".
    WHEN 629 THEN cRet = "400-17-".
    WHEN 418 THEN cRet = "400-00-".

    WHEN 428 THEN cRet = "430-10-".
    WHEN 425 THEN cRet = "430-17-".
    WHEN 86  THEN cRet = "430-00-".
    WHEN 421 THEN cRet = "430-07-".
    WHEN 417 THEN cRet = "430-02-".

    WHEN 60  THEN cRet = "460-00-".

    WHEN 426 THEN cRet = "480-05-".

    WHEN 420 THEN cRet = "500-01-".
    WHEN 401 THEN cRet = "500-03-".
    WHEN 54  THEN cRet = "500-00-".

    WHEN 431 THEN cRet = "550-02-".
    WHEN 415 THEN cRet = "570-00-".
    WHEN 84  THEN cRet = "570-02-".
    WHEN 419 THEN cRet = "570-20-".
    WHEN 427 THEN cRet = "570-19-".

    WHEN 422 THEN cRet = "600-02-".
    WHEN 423 THEN cRet = "600-00-".

    WHEN 424 THEN cRet = "700-00-".

    WHEN 796 THEN cRet = "NFC-00-".

    WHEN 602 THEN cRet = "800-00-".
    WHEN 606 THEN cRet = "801-00-".
    WHEN 620 THEN cRet = "800-00-".
    WHEN 631 THEN cRet = "806-00-".
    WHEN 604 THEN cRet = "806-00-".
    WHEN 601 THEN cRet = "805-00-".
    WHEN 622 THEN cRet = "802-00-".

    WHEN 797 THEN cRet = "809-00-".
    WHEN 799 THEN cRet = "809-00-".
    WHEN 798 THEN cRet = "810-00-".
    WHEN 800 THEN cRet = "810-00-".
    WHEN 544 THEN cRet = "811-00-".

    WHEN 630 THEN cRet = "816-00-".
    WHEN 794 THEN cRet = "816-00-".
    WHEN 614 THEN cRet = "816-00-".
    WHEN 311 THEN cRet = "400-06-".
    WHEN 339 THEN cRet = "400-16-".
    WHEN 85  THEN cRet = "430-00-".
    WHEN 317 THEN cRet = "816-00-".
    WHEN 78  THEN cRet = "809-00-".
    WHEN 626 THEN cRet = "809-00-".
    WHEN 600 THEN cRet = "804-00-".
    WHEN 613 THEN cRet = "800-00-".
    WHEN 605 THEN cRet = "816-00-".
    WHEN 669 THEN cRet = "808-00-".
    
  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadEnUMSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadEnUMSap Procedure 
FUNCTION getCantidadEnUMSap RETURNS CHARACTER
  (pcCod AS CHARACTER,
   piCant AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  FOR FIRST tambores_industria
      WHERE tambores_industria.nromov = piNro
        AND tambores_industria.id_locacion_ubicacion = 4
      NO-LOCK.
    fKil = tambores_industria.kilos_tambor.
  END.

  fKil = fKil * piCant.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCategoriaValoracion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCategoriaValoracion Procedure 
FUNCTION getCategoriaValoracion RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  
  cTip = ENTRY(1, pcCod, "-").
  CASE cTip:
    WHEN "LAC" THEN cRet = "3201".
    WHEN "LCA" THEN cRet = "3204".
    WHEN "LJC" THEN cRet = "3203".
    WHEN "LJT" THEN cRet = "4102".
    WHEN "LOP" THEN cRet = "3206".
    WHEN "LPA" THEN cRet = "3209".
    WHEN "LPU" THEN cRet = "3208".
    WHEN "LTP" THEN cRet = "3205".
    WHEN "LTR" THEN cRet = "4102".
    WHEN "LWP" THEN cRet = "3207".
    OTHERWISE cRet = cSpace.
  END CASE.
  

  RETURN cRet.
      
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCategoriaValoracionFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCategoriaValoracionFF Procedure 
FUNCTION getCategoriaValoracionFF RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cArt = ENTRY(1, pcCod, "-").

  CASE cArt:
    WHEN "L01" THEN cRet = "3101".
    WHEN "N01" THEN cRet = "3103".
    WHEN "N03" THEN cRet = "3103".
    WHEN "M01" THEN cRet = "3104".
    WHEN "M04" THEN cRet = "3104".
    WHEN "P02" THEN cRet = "3102".
    WHEN "A06" THEN cRet = "3105".
    WHEN "T05" THEN cRet = "3104".
  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCentroInsumo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCentroInsumo Procedure 
FUNCTION getCentroInsumo RETURNS CHARACTER
  (INPUT ip_suc AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER FORMAT "x(04)" NO-UNDO.

  CASE ip_suc:
      WHEN 51  OR
      WHEN 52  OR
      WHEN 99  OR
      WHEN 152 THEN "A100".
      WHEN 70  OR
      WHEN 151 OR
      WHEN 153 OR
      WHEN 161 THEN "A200".
      WHEN 2   OR
      WHEN 3   OR
      WHEN 4   OR
      WHEN 5   OR
      WHEN 6   OR
      WHEN 7   OR
      WHEN 8   OR
      WHEN 9   OR
      WHEN 10  OR
      WHEN 12  OR
      WHEN 13  OR
      WHEN 14  OR
      WHEN 15  OR
      WHEN 16  OR
      WHEN 17  OR
      WHEN 18  OR
      WHEN 19  OR
      WHEN 20  OR
      WHEN 21  OR
      WHEN 23  OR
      WHEN 24  OR
      WHEN 25  OR
      WHEN 28  OR
      WHEN 164 OR
      WHEN 165 OR
      WHEN 167 OR
      WHEN 168 OR
      WHEN 11  OR
      WHEN 58  OR
      WHEN 163 OR
      WHEN 170 OR
      WHEN 63  THEN "A300".
      OTHERWISE "0000".
  END CASE.

  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCitde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCitde Procedure 
FUNCTION getCitde RETURNS CHARACTER
  (pcCitde AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = REPLACE(pcCitde, " ", "").
  cRet = REPLACE(cRet, ".", "").
  cRet = REPLACE(cRet, "-", "").
  cRet = REPLACE(cRet, "BE", "").

  cRet = putSpaceIfEmpty(cRet).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getClienteSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClienteSap Procedure 
FUNCTION getClienteSap RETURNS INTEGER
  (piCli AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCli AS INTEGER    NO-UNDO.
  
  FOR FIRST clientes_sap
      WHERE clientes_sap.id_cliente_sap = piCli
      NO-LOCK.
    iCli = clientes_sap.id_cliente.
  END.

  RETURN iCli.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodigoProductoElabSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodigoProductoElabSap Procedure 
FUNCTION getCodigoProductoElabSap RETURNS CHARACTER
  (piArticulo AS INTEGER,
   piCalidad  AS INTEGER,
   piEnvase   AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = getDenominacionProductoSap(piArticulo) + 
         getCalidadAndPulpaSap(piCalidad) + 
         getEnvaseSap(piEnvase).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodPostalSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodPostalSap Procedure 
FUNCTION getCodPostalSap RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcCP   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFor AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLng AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  IF NUM-ENTRIES(pcCP, ".") > 0 THEN
    cRet = ENTRY(1, pcCp, ".").
  ELSE
    cRet = cSpace.
  RETURN cRet.

  FOR FIRST validacion_codigo_postal
      WHERE clave_pais = pcPais
      NO-LOCK.
    cFor = "X(" + STRING(INTEGER(validacion_codigo_postal.longitud)) + ")".
    DO i = 1 TO integer(validacion_codigo_postal.longitud):
      cLng = cLng + "9".
    END.


    CASE clave_regla:
      WHEN "1" THEN DO:
        cRet = STRING(TRIM(pcCP), validacion_codigo_postal.longitud).
      END.
      WHEN "2" THEN DO:     
        cRet = TRIM(STRING(INTEGER(pcCP))).
      END.
      WHEN "4" THEN DO:
        cRet = STRING(TRIM(STRING(INTEGER(pcCP))), cLng). 
      END.
      WHEN "5" THEN DO:
        cRet = STRING(pcCP, cFor).
      END.
      WHEN "7" THEN DO:
        cRet = STRING(pcCP, cLng).
      END.
      WHEN "9" THEN DO:
        CASE pcPais:
          WHEN "CA" THEN cFor = "X9X 9X9".
          WHEN "NL" THEN cFor = "9999 XX".
          WHEN "PL" THEN cFor = "99-999".
          WHEN "CZ" THEN cFor = "999 99".
          WHEN "KR" THEN cFor = "999-999".
          WHEN "PT" THEN cFor = "9999-999".
          OTHERWISE cFor = "".
        END CASE.
          
        cRet = STRING(pcCP, cFor).
      END.
      OTHERWISE cRet = ".".
    END CASE.
  END.

  cRet = cTxtCh + cRet.

  RETURN cRet.

END FUNCTION.


/* 
 Canad�:         ANA NAN

Holanda:        NNNN AA

Polonia         NN-NNN

Suecia,

Eslovaquia,

Rep.Checa:      NNN NN

Corea del Sur:  NNN-NNN

Portugal:       NNNN-NNN o NNNN

De forma transitoria, en Portugal se utilizan los patrones NNNN (forma antigua) y NNNN-NNN (despu�s de la reforma de los c�digos postales) si se ha configurado la regla de verificaci�n "9".

 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCondicionIVA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCondicionIVA Procedure 
FUNCTION getCondicionIVA RETURNS CHARACTER
  (ip_iva AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v_ret AS CHARACTER FORMAT 'x(03)'.

   CASE ip_iva:
       WHEN 0 THEN
           ASSIGN v_ret = "04".
       WHEN 1 THEN
           ASSIGN v_ret = "01".
       WHEN 2 THEN
           ASSIGN v_ret = "02".
       WHEN 3 THEN
           ASSIGN v_ret = "05".
       WHEN 4 THEN
           ASSIGN v_ret = "06".
       WHEN 5 THEN
           ASSIGN v_ret = "07".
       OTHERWISE
           ASSIGN v_ret = "03".
   END.
   /*
Provincias
0 - Cap. Federal
1 - Buenos Aires
2 - Catamarca
3 - C�rdoba
4 - Corrientes
5 - Entre R�os
6 - Jujuy
7 - Mendoza
8 - La Rioja
9 - Salta
10 - San Juan
11 - San Luis
12 - Santa Fe
13 - Sgo. Del Estero
14 - Tucum�n
16 - Chaco
17 - Chubut
18 - Formosa
19 - Misiones
20 - Neuquen
21 - La Pampa
22 - R�o Negro
23 - Santa Cruz
24 - Tierra del Fuego
*/
   RETURN v_ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCuentaAsociada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCuentaAsociada Procedure 
FUNCTION getCuentaAsociada RETURNS CHARACTER
  (pcSector AS CHARACTER, 
   pcPais   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF pcSector = "IN" AND pcPais <> "AR" THEN RETURN "110310004".
  IF pcSector = "FF" AND pcPais <> "AR" THEN RETURN "110310002".
  
  IF pcSector = "IN" AND pcPais = "AR" THEN RETURN "110310003".
  IF pcSector = "FF" AND pcPais = "AR" THEN RETURN "110310001".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDenominacionProductoSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDenominacionProductoSAP Procedure 
FUNCTION getDenominacionProductoSAP RETURNS CHARACTER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.


  CASE piArticulo:
    WHEN 30   THEN cRet = "LPA-01-".
    WHEN 71   THEN cRet = "LPU-01-".
    WHEN 244  THEN cRet = "LTR-01-".
    WHEN 52   THEN cRet = "LJT-01-".
    WHEN 53   THEN cRet = "LJC-01-".
    WHEN 550  THEN cRet = "LJT-01-".

    WHEN 76   THEN cRet = "LAC-01-".
    WHEN 41   THEN cRet = "LAC-02-".
    WHEN 51   THEN cRet = "LAC-01-".
    WHEN 96   THEN cRet = "LTP-02-".
    WHEN 74   THEN cRet = "LTP-01-".
    WHEN 745  THEN cRet = "LTP-01-".
    WHEN 58   THEN cRet = "LWP-01-".
    WHEN 28   THEN cRet = "LTR-01-".
    WHEN 36   THEN cRet = "LTR-01-".
    WHEN 42   THEN cRet = "LJT-02-".

    WHEN 54   THEN cRet = "LCA-01-".

    WHEN 46   THEN cRet = "LJT-03-".
    WHEN 50   THEN cRet = "LAC-01-".
    WHEN 57   THEN cRet = "LOP-01-".
    WHEN 61   THEN cRet = "LAC-04-".
    WHEN 62   THEN cRet = "LJT-04-".
    WHEN 65   THEN cRet = "LAC-04-".
    WHEN 70   THEN cRet = "LPU-02-".
    WHEN 245  THEN cRet = "LTR-01-".
    WHEN 246  THEN cRet = "LTR-01-".
    WHEN 247  THEN cRet = "LTR-01-".

  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescripcionRegion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescripcionRegion Procedure 
FUNCTION getDescripcionRegion RETURNS CHARACTER
  (pcPais AS CHARACTER,
   pcRegi AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = cSpace.
  FOR FIRST regiones_sap
      WHERE TRIM(clave_pais)   = TRIM(pcPais)
        AND TRIM(clave_region) = TRIM(pcRegi)
      NO-LOCK.
    cRet = putSpaceIfEmpty(CAPS(TRIM(regiones_sap.descripcion))).
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDestino Procedure 
FUNCTION getDestino RETURNS CHARACTER
  (piDes AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST destinos
      WHERE destinos.id_destino = piDes
      NO-LOCK.
    cRet = CAPS(destinos.descripcion).
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDomicilioCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDomicilioCliente Procedure 
FUNCTION getDomicilioCliente RETURNS CHARACTER
  (piCli AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER INITIAL "" NO-UNDO.

  cRet = ".".
  FOR FIRST clientes
      WHERE clientes.id_cliente = piCli
      NO-LOCK.
    cRet = CAPS(clientes.domicilio).
  END.
  cRet = putSpaceIfEmpty(cRet).
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEnvaseSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvaseSAP Procedure 
FUNCTION getEnvaseSAP RETURNS CHARACTER
  (piEnvase AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  CASE piEnvase:
    WHEN 527 THEN cRet = "200".
    WHEN 519 THEN cRet = "300".
    WHEN 520 THEN cRet = "301".
    WHEN 521 THEN cRet = "302".
    WHEN 533 THEN cRet = "303".
    WHEN 518 THEN cRet = "304".
    WHEN 534 THEN cRet = "400".
    WHEN 523 THEN cRet = "401".
    WHEN 522 THEN cRet = "402".
    WHEN 14  THEN cRet = "500".
    WHEN 532 THEN cRet = "600".
    WHEN 250 THEN cRet = "700".
    WHEN 564 THEN cRet = "601".
    WHEN 21  THEN cRet = "602".
    WHEN 528 THEN cRet = "201".
    WHEN 557 THEN cRet = "100".
    WHEN 558 THEN cRet = "101".
    WHEN 560 THEN cRet = "102".
    WHEN 561 THEN cRet = "103".
    WHEN 501 THEN cRet = "104".
    WHEN 503 THEN cRet = "105".
    WHEN 504 THEN cRet = "107".
    WHEN 515 THEN cRet = "108".
    WHEN 555 THEN cRet = "109".
    WHEN 535 THEN cRet = "110".
    WHEN 556 THEN cRet = "111".
    WHEN 502 THEN cRet = "112".
    WHEN 536 THEN cRet = "113".
    WHEN 537 THEN cRet = "114".
    WHEN 500 THEN cRet = "115".
    WHEN 563 THEN cRet = "116".
    WHEN 506 THEN cRet = "117".
    WHEN 507 THEN cRet = "118".
    WHEN 538 THEN cRet = "604".
    WHEN 559 THEN cRet = "119".
    WHEN 517 THEN cRet = "120".
    WHEN 516 THEN cRet = "121".
    WHEN 511 THEN cRet = "500".

  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGrupoVendedoresFromPais) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGrupoVendedoresFromPais Procedure 
FUNCTION getGrupoVendedoresFromPais RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcSect AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF pcSect = "FF" THEN DO:
    CASE pcPais:
      WHEN "US" THEN cRet = "002".
      WHEN "JP" THEN cRet = "105".
      WHEN "DE" THEN cRet = "103".
      WHEN "FR" THEN cRet = "103".
      WHEN "MY" THEN cRet = "101".
      WHEN "RU" THEN cRet = "105".
      WHEN "CN" THEN cRet = "101".
      WHEN "TR" THEN cRet = "102".
      WHEN "PL" THEN cRet = "101".
      WHEN "SI" THEN cRet = "102".
      WHEN "SA" THEN cRet = "102".
      WHEN "CA" THEN cRet = "101".
      WHEN "IT" THEN cRet = "103".
      WHEN "CY" THEN cRet = "101".
      WHEN "SG" THEN cRet = "101".
      WHEN "HK" THEN cRet = "104".
      WHEN "ES" THEN cRet = "102".
      WHEN "GB" THEN cRet = "101".
      WHEN "MY" THEN cRet = "101".
      WHEN "LT" THEN cRet = "103".
      WHEN "UA" THEN cRet = "106".
      WHEN "FI" THEN cRet = "106".
      WHEN "NL" THEN cRet = "102".
      WHEN "TR" THEN cRet = "102".
      WHEN "PL" THEN cRet = "106".
      WHEN "SI" THEN cRet = "106".
      WHEN "HR" THEN cRet = "106".
      WHEN "PH" THEN cRet = "106".
      WHEN "SA" THEN cRet = "106".
      WHEN "CH" THEN cRet = "106".
      WHEN "AU" THEN cRet = "106".
      WHEN "BR" THEN cRet = "106".
      WHEN "PT" THEN cRet = "106".
      WHEN "AT" THEN cRet = "106".
      WHEN "TW" THEN cRet = "106".
      WHEN "IL" THEN cRet = "106".
      WHEN "VN" THEN cRet = "106".
      WHEN "DN" THEN cRet = "106".
      WHEN "BE" THEN cRet = "106".
      WHEN "AR" THEN cRet = "106".
      WHEN "HU" THEN cRet = "106".
      WHEN "IE" THEN cRet = "106".
      WHEN "GR" THEN cRet = "106".
      WHEN "SE" THEN cRet = "106".
      WHEN "ID" THEN cRet = "106".
      WHEN "BN" THEN cRet = "106".
      WHEN "PR" THEN cRet = "106".
      WHEN "RO" THEN cRet = "105".
    END CASE.
  END.

  IF pcSect = "IN" THEN DO:
    CASE pcPais:
      WHEN "US" THEN cRet = "002".
      WHEN "JP" THEN cRet = "001".
      WHEN "DE" THEN cRet = "005".
      WHEN "FR" THEN cRet = "005".
      WHEN "MY" THEN cRet = "001".
      WHEN "RU" THEN cRet = "001".
      WHEN "CN" THEN cRet = "001".
      WHEN "TR" THEN cRet = "001".
      WHEN "PL" THEN cRet = "005".
      WHEN "SI" THEN cRet = "005".
      WHEN "SA" THEN cRet = "001".
      WHEN "CA" THEN cRet = "002".
      WHEN "IT" THEN cRet = "005".
      WHEN "CY" THEN cRet = "001".
      WHEN "SG" THEN cRet = "001".
      WHEN "HK" THEN cRet = "001".
      WHEN "ES" THEN cRet = "005".
      WHEN "GB" THEN cRet = "005".
      WHEN "MY" THEN cRet = "001".
      WHEN "LT" THEN cRet = "005".
      WHEN "UA" THEN cRet = "005".
      WHEN "FI" THEN cRet = "001".
      WHEN "NL" THEN cRet = "005".
      WHEN "TR" THEN cRet = "001".
      WHEN "PL" THEN cRet = "005".
      WHEN "SI" THEN cRet = "005".
      WHEN "HR" THEN cRet = "005".
      WHEN "PH" THEN cRet = "001".
      WHEN "SA" THEN cRet = "001".
      WHEN "CH" THEN cRet = "005".
      WHEN "AU" THEN cRet = "001".
      WHEN "BR" THEN cRet = "001".
      WHEN "PT" THEN cRet = "005".
      WHEN "AT" THEN cRet = "005".
      WHEN "TW" THEN cRet = "001".
      WHEN "IL" THEN cRet = "001".
      WHEN "VN" THEN cRet = "001".
      WHEN "DN" THEN cRet = "005".
      WHEN "BE" THEN cRet = "005".
      WHEN "AR" THEN cRet = "003".
      WHEN "HU" THEN cRet = "005".
      WHEN "IE" THEN cRet = "005".
      WHEN "GR" THEN cRet = "005".
      WHEN "SE" THEN cRet = "005".
      WHEN "ID" THEN cRet = "001".
      WHEN "BN" THEN cRet = "001".
      WHEN "PR" THEN cRet = "002".
      WHEN "RO" THEN cRet = "001".
    END CASE.
  END.

  cRet = cTxtCh + cRet.
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIncotermFromContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIncotermFromContrato Procedure 
FUNCTION getIncotermFromContrato RETURNS CHARACTER
  (pcCon AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "FOB".
  FOR FIRST contratos 
      WHERE id_contrato = pcCon
      NO-LOCK, 
      FIRST items_contratos 
         OF contratos NO-LOCK, 
      FIRST clausulas 
         OF items_contratos NO-LOCK.

    cRet = REPLACE(clausulas.descripcion, ".", "").
    cRet = REPLACE(cRet, "Duty Paid", "").
    cRet = REPLACE(cRet, "Duty Unpaid", "").
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getJerarquiaProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getJerarquiaProducto Procedure 
FUNCTION getJerarquiaProducto RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

 
  cTip = ENTRY(1, pcCod, "-").
  CASE cTip:
    WHEN "LAC" THEN cRet = "000010000100000001".
    WHEN "LCA" THEN cRet = "000030000100000001".
    WHEN "LJC" THEN cRet = "000020000100000001".
    WHEN "LJT" THEN cRet = "000020000100000002".
    WHEN "LOP" THEN cRet = "000040000100000001".
    WHEN "LPA" THEN cRet = "000050000100000001".
    WHEN "LPU" THEN cRet = "000050000100000002".
    WHEN "LTP" THEN cRet = "000040000100000002".
    WHEN "LTR" THEN cRet = "000020000100000002".
    WHEN "LWP" THEN cRet = "000040000100000003".

    /* fruta */
    WHEN "A06" THEN cRet = "60000".
    WHEN "L01" THEN cRet = "10000".
    WHEN "M01" THEN cRet = "30000".
    WHEN "N01" THEN cRet = "20000".
    WHEN "M04" THEN cRet = "30000".
    WHEN "N03" THEN cRet = "20000".
    WHEN "P02" THEN cRet = "40000".
    WHEN "T05" THEN cRet = "50000".

    OTHERWISE cRet = cSpace.
  END CASE.

  

  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosBrutosEnvase Procedure 
FUNCTION getKilosBrutosEnvase RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  ASSIGN iArt = INTEGER(ENTRY(1, pcCod, "-"))
         iCal = INTEGER(ENTRY(2, pcCod, "-"))
         iEnv = INTEGER(ENTRY(3, pcCod, "-"))
         .

  FOR FIRST r_productos_calidad_envase
      WHERE r_productos_calidad_envase.id_articulo = iArt
        AND r_productos_calidad_envase.id_calidad  = iCal
        AND r_productos_calidad_envase.id_envase   = iEnv
      NO-LOCK.
    fRet = r_productos_calidad_envase.kilos.
  END.


  FOR FIRST envases_prod
      WHERE envases_prod.id_envase = iEnv
      NO-LOCK.
    fRet = fRet + envases_prod.tara.
  END.

  RETURN STRING(fRet).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosEnvaseFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosBrutosEnvaseFF Procedure 
FUNCTION getKilosBrutosEnvaseFF RETURNS CHARACTER
  (piArt AS INTEGER,
   piEnv AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST r_envases_prod 
      WHERE r_envases_prod.id_articulo = piArt
        AND r_envases_prod.id_envase   = piEnv
      NO-LOCK.
    cRet = STRING(r_envases_prod.kilos).

  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosNetosEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosNetosEnvase Procedure 
FUNCTION getKilosNetosEnvase RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  ASSIGN iArt = INTEGER(ENTRY(1, pcCod, "-"))
         iCal = INTEGER(ENTRY(2, pcCod, "-"))
         iEnv = INTEGER(ENTRY(3, pcCod, "-"))
         .

  FOR FIRST r_productos_calidad_envase
      WHERE r_productos_calidad_envase.id_articulo = iArt
        AND r_productos_calidad_envase.id_calidad  = iCal
        AND r_productos_calidad_envase.id_envase   = iEnv
      NO-LOCK.
    fRet = r_productos_calidad_envase.kilos.
  END.


  RETURN STRING(INTEGER(fRet)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosNetosEnvaseFF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosNetosEnvaseFF Procedure 
FUNCTION getKilosNetosEnvaseFF RETURNS CHARACTER
  (piArt AS INTEGER,
   piEnv AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST r_envases_prod 
      WHERE r_envases_prod.id_articulo = piArt
        AND r_envases_prod.id_envase   = piEnv
      NO-LOCK.
    cRet = STRING(r_envases_prod.kilos_nominal).

  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMonedaClienteFromIdMoneda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMonedaClienteFromIdMoneda Procedure 
FUNCTION getMonedaClienteFromIdMoneda RETURNS CHARACTER
  (piMoneda AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  CASE piMoneda:
    WHEN 2  THEN cRet = "USD".
    WHEN 59 THEN cRet = "EUR".
    WHEN 21 THEN cRet = "GBP".
    WHEN 3  THEN cRet = "CDF".
    WHEN 6  THEN cRet = "DEM".
    WHEN 19 THEN cRet = "JPY".
    WHEN 1  THEN cRet = "ARS".
    OTHERWISE cRet = "USD".
  END CASE.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNroCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNroCliente Procedure 
FUNCTION getNroCliente RETURNS CHARACTER
  (piNum  AS INTEGER,
   pcPais AS CHARACTER, 
   pcSect AS CHARACTER, 
   pcCuit AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCui AS CHARACTER  NO-UNDO.

  cCui = SUBSTRING(pcCuit, 1, LENGTH(pcCuit) - 1).

  IF pcPais <> "AR" THEN 
    cRet = STRING(piNum + IF pcSect <> "FF" THEN 0 ELSE 5000).
  ELSE
    cRet = cCui.

  cRet = IF cRet = "" THEN STRING(INTEGER(pcCuit) + 9999) ELSE cRet.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPaisFromLeyenda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPaisFromLeyenda Procedure 
FUNCTION getPaisFromLeyenda RETURNS CHARACTER
  (pcLeyenda AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = cSpace.

  /* FALTAN DE FINIR ESTOS PAISES , REVISAR LUEGO O CONFIRMAR */
  IF pcLeyenda MATCHES "*PARA*" THEN cRet = "PA".
  IF pcLeyenda MATCHES "*PERU*" THEN cRet = "PE".
  IF pcLeyenda MATCHES "*MEXICO*" THEN cRet = "MX".
  IF pcLeyenda MATCHES "*NORUEGA*" THEN cRet = "NO".
  /*---------------------------------------------------------------*/

  /* Antillas Holandesas */
  IF pcLeyenda MATCHES "*ANTIL*" THEN cRet = "AN".


  /* japon */
  IF pcLeyenda MATCHES "*JAPON*" THEN cRet = "JP".
  IF pcLeyenda MATCHES "*JAPAN*" THEN cRet = "JP".

  /* francia */
  IF pcLeyenda MATCHES "*FRANCIA*" THEN cRet = "FR".
  IF pcLeyenda MATCHES "*FR*" THEN cRet = "FR".

  /* candada */
  IF pcLeyenda MATCHES "*CANADA*" THEN cRet = "CA".

  /* Rusia */
  IF pcLeyenda MATCHES "*RUS*" THEN cRet = "RU".

  /* Italia */
  IF pcLeyenda MATCHES "*ITA*" THEN cRet = "IT".

  /* Chipre */
  IF pcLeyenda MATCHES "*CYP*" THEN cRet = "CY".
  IF pcLeyenda MATCHES "*CHIP*" THEN cRet = "CY".
  
  /* Singapur */
  IF pcLeyenda MATCHES "*SINGA*" THEN cRet = "SG".

  /* Hong Kong */
  IF pcLeyenda MATCHES "*HON*KON*" THEN cRet = "HK".

  /* Espa�a */
  IF pcLeyenda MATCHES "*SPA*" THEN cRet = "ES".
  IF pcLeyenda MATCHES "*ESPA*" THEN cRet = "ES".

  /* Inglaterra */
  IF pcLeyenda MATCHES "*NGL*" THEN cRet = "GB".
  IF pcLeyenda MATCHES "*KINGDOM*" THEN cRet = "GB".
  IF pcLeyenda MATCHES "*INGLA*" THEN cRet = "GB".

  /* Malasia */
  IF pcLeyenda MATCHES "*MALA*SIA*" THEN cRet = "MY".

  /* Lituania */
  IF pcLeyenda MATCHES "*LIT*NIA*" THEN cRet = "LT".

  /* Alemania */
  IF pcLeyenda MATCHES "*ALEM*" THEN cRet = "DE".
  IF pcLeyenda MATCHES "*GERM*" THEN cRet = "DE".

  /* Rumania */
  IF pcLeyenda MATCHES "*RUM*" THEN cRet = "RO".

  /* China */
  IF pcLeyenda MATCHES "*CHIN*" THEN cRet = "CN".

  /* Ucrania */
  IF pcLeyenda MATCHES "*U*RAN*" THEN cRet = "UA".
  IF pcLeyenda MATCHES "*UKRAINE*" THEN cRet = "UA".

  /* Finlandia */
  IF pcLeyenda MATCHES "*FINL*" THEN cRet = "FI".

  /* Holanda */
  IF pcLeyenda MATCHES "*Netherlands*" THEN cRet = "NL".
  IF pcLeyenda MATCHES "*HOL*AND*" THEN cRet = "NL".
  IF pcLeyenda MATCHES "*ROTTERD*" THEN cRet = "NL".

  /* Turquia */
  IF pcLeyenda MATCHES "*TUR*" THEN cRet = "TR".

  /* Polonia */
  IF pcLeyenda MATCHES "*POLAND*" THEN cRet = "PL".
  IF pcLeyenda MATCHES "*PO*NI*A*" THEN cRet = "PL".

  /* Eslovenia */
  IF pcLeyenda MATCHES "*SLOV*" THEN cRet = "SI".

  /* Croacia */
  IF pcLeyenda MATCHES "*CROA*" THEN cRet = "HR".

  /* Filipinas */
  IF pcLeyenda MATCHES "*PHILI*" THEN cRet = "PH".
  IF pcLeyenda MATCHES "*FILIP*" THEN cRet = "PH".

  /* Arabia Saudita */
  IF pcLeyenda MATCHES "*SAUDI*" THEN cRet = "SA".

  /* Suiza */
  IF pcLeyenda MATCHES "*SUIZ*" THEN cRet = "CH".
  IF pcLeyenda MATCHES "*SWITZ*LAND*" THEN cRet = "CH".

  /* Australia */
  IF pcLeyenda MATCHES "*AUSTRALIA*" THEN cRet = "AU".

  /* Brasil */
  IF pcLeyenda MATCHES "*BRAS*" THEN cRet = "BR".

  /* Portugal */
  IF pcLeyenda MATCHES "*PORTU*" THEN cRet = "PT".

  /* Austria */
  IF pcLeyenda MATCHES "*AUSTRIA*" THEN cRet = "AT".

  /* Taiwan */
  IF pcLeyenda MATCHES "*TAIWAN*" THEN cRet = "TW".

  /* Israel */
  IF pcLeyenda MATCHES "*ISRAEL*" THEN cRet = "IL".

  /* Vietnam */
  IF pcLeyenda MATCHES "*VIETNAM*" THEN cRet = "VN".

  /* Dinamarca */
  IF pcLeyenda MATCHES "*D*NMARK*" THEN cRet = "DK".
  IF pcLeyenda MATCHES "*DINAMAR*" THEN cRet = "DK".

  /* Belgica */
  IF pcLeyenda MATCHES "*BELGICA*" THEN cRet = "BE".
  IF pcLeyenda MATCHES "*BELGIUM*" THEN cRet = "BE".

  /* Argentina */
  IF pcLeyenda MATCHES "*ARGENTINA*" THEN cRet = "AR".

  /* Hungria */
  IF pcLeyenda MATCHES "*HUNG*" THEN cRet = "HU".

  /* Irlanda */
  IF pcLeyenda MATCHES "*IR*LAND*" THEN cRet = "IE".

  /* Grecia */
  IF pcLeyenda MATCHES "*GREECE*" THEN cRet = "GR".
  IF pcLeyenda MATCHES "*GRECIA*" THEN cRet = "GR".

  /* Suecia */
  IF pcLeyenda MATCHES "*SUECIA*" THEN cRet = "SE".
  IF pcLeyenda MATCHES "*SWED*EN*" THEN cRet = "SE".

  /* Indonesia */
  IF pcLeyenda MATCHES "*INDON*SIA*" THEN cRet = "ID".

  /* Brunei */
  IF pcLeyenda MATCHES "*BRUN*I*" THEN cRet = "BN".

  /* Puerto Rico */
  IF pcLeyenda MATCHES "*PUERTO*RICO*" THEN cRet = "PR".
  
  /* usa */
  IF pcLeyenda MATCHES "*EE*UU*" THEN cRet = "US".
  IF pcLeyenda MATCHES "*USA*" THEN cRet = "US".
  IF pcLeyenda MATCHES "*U.S.A.*" THEN cRet = "US". 
  IF pcLeyenda MATCHES "*New York*" THEN cRet = "US".
  IF pcLeyenda MATCHES "*Estado*Unido*" THEN cRet = "US".

  /* Chile */
  IF pcLeyenda MATCHES "*CHILE*" THEN cRet = "CL".

  /* Uruguay */
  IF pcLeyenda MATCHES "*URUGUAY*" THEN cRet = "UY".

  /* Korea */
  IF pcLeyenda MATCHES "*Kore*" THEN cRet = "KR".


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPlazo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPlazo Procedure 
FUNCTION getPlazo RETURNS CHARACTER
  (pcCondPago AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.

  CASE pcCondPago:
    WHEN "0015" THEN cReturn = "15,3".
    WHEN "0030" THEN cReturn = "30,3".
    WHEN "0090" THEN cReturn = "90,3".
    WHEN "0180" THEN cReturn = "180,3".
    WHEN "B030" THEN cReturn = "30,3".
    WHEN "B045" THEN cReturn = "45,3".
    WHEN "B060" THEN cReturn = "60,3".
    WHEN "B180" THEN cReturn = "180,3".
    WHEN "C030" THEN cReturn = "30,3".
    WHEN "C045" THEN cReturn = "45,3".
    WHEN "C060" THEN cReturn = "60,3".
    WHEN "CAD0" THEN cReturn = "0,21".
    WHEN "D030" THEN cReturn = "30,3".
    WHEN "D045" THEN cReturn = "45,3".
    WHEN "DC30" THEN cReturn = "30,3".
    WHEN "L/C0" THEN cReturn = "0,8".


  END CASE.

  RETURN cReturn.

END FUNCTION.


/* 15
30
90
180
   B030
   B045
   B060
   B180
   C030
   C045
   C060
   CAD0
   D030
   D045
   DC30
   L/C0
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPosicionArancelaria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPosicionArancelaria Procedure 
FUNCTION getPosicionArancelaria RETURNS CHARACTER
  (pcCod AS CHARACTER, 
   pcNet AS CHARACTER,
   pcBru AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  
  cTip = ENTRY(1, pcCod, "-").
  CASE cTip:
    /* industria */
    WHEN "LAC" THEN cRet = "3301.13.00".
    WHEN "LCA" THEN cRet = "2308.00.00.000K".
    WHEN "LJC" THEN cRet = "2009.39.00.110T".
    WHEN "LJT" THEN cRet = "2009.39.00.190U".
    WHEN "LOP" THEN cRet = "3301.13.00.900N".
    WHEN "LPA" THEN cRet = "2008.30.00.900G".
    WHEN "LPU" THEN cRet = "2008.30.00.900G".
    WHEN "LTP" THEN cRet = "3301.90.20".
    WHEN "LTR" THEN cRet = "2009.39.00.190U".
    WHEN "LWP" THEN cRet = "3301.13.00.900N".
    
    /* fruta */
    WHEN "A06" THEN cRet = "0703.20.90.100Y".
    WHEN "L01" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.50.00.910T" ELSE "0805.50.00.920W".
    WHEN "M01" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.40.00.910D" ELSE "0805.40.00.920G".
    WHEN "N01" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.10.00.910L" ELSE "0805.10.00.920P".
    WHEN "M04" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.40.00.910D" ELSE "0805.40.00.920G".
    WHEN "N03" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.10.00.910L" ELSE "0805.10.00.920P".
    WHEN "P02" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.40.00.910D" ELSE "0805.40.00.920G".
    WHEN "T05" THEN cRet = IF INTEGER(pcBru) <= 16 THEN "0805.40.00.910D" ELSE "0805.40.00.920G".
    

    OTHERWISE cRet = cSpace.
  END CASE.



  
  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProductoProgress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProductoProgress Procedure 
FUNCTION getProductoProgress RETURNS CHARACTER
  (pcMatnr AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST materiales_sap 
      WHERE materiales_sap.id_material_sap = pcMatnr:
    cRet = STRING(materiales_sap.id_articulo) + "," +
           STRING(materiales_sap.id_calidad) + "," +
           STRING(materiales_sap.id_envase).
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProvincia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProvincia Procedure 
FUNCTION getProvincia RETURNS CHARACTER
  (INPUT ip_provincia AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v_ret AS CHARACTER FORMAT 'x(03)'.

   CASE ip_provincia:
       WHEN 'CAPITAL FEDERAL' THEN
           ASSIGN v_ret = '0'.
       WHEN 'BUENOS AIRES' THEN
           ASSIGN v_ret = '1'.
       WHEN 'CATAMARCA' THEN
           ASSIGN v_ret = '2'.
       WHEN 'CORDOBA' THEN
           ASSIGN v_ret = '3'.
       WHEN 'CORRIENTES' THEN
           ASSIGN v_ret = '4'.
       WHEN 'ENTRE RIOS' THEN
           ASSIGN v_ret = '5'.
       WHEN 'JUJUY' THEN
           ASSIGN v_ret = '6'.
       WHEN 'MENDOZA' THEN
           ASSIGN v_ret = '7'.
       WHEN 'LA RIOJA' THEN
           ASSIGN v_ret = '8'.
       WHEN 'SALTA' THEN
           ASSIGN v_ret = '9'.
       WHEN 'SAN JUAN' THEN
           ASSIGN v_ret = '10'.
       WHEN 'SAN LUIS' THEN
           ASSIGN v_ret = '11'.
       WHEN 'SANTA FE' THEN
           ASSIGN v_ret = '12'.
       WHEN 'STGO.DEL ESTERO' THEN
           ASSIGN v_ret = '13'.
       WHEN 'TUCUMAN' THEN
           ASSIGN v_ret = '14'.
       WHEN 'CHACO' THEN
           ASSIGN v_ret = '16'.
       WHEN 'CHUBUT' THEN
           ASSIGN v_ret = '17'.
       WHEN 'FORMOSA' THEN
           ASSIGN v_ret = '18'.
       WHEN 'MISIONES' THEN
           ASSIGN v_ret = '19'.
       WHEN 'NEUQUEN' THEN
           ASSIGN v_ret = '20'.
       WHEN 'LA PAMPA' THEN
           ASSIGN v_ret = '21'.
       WHEN 'RIO NEGRO' THEN
           ASSIGN v_ret = '22'.
       WHEN 'SANTA CRUZ' THEN
           ASSIGN v_ret = '23'.
       WHEN 'TIERRA DEL FUEGO' THEN
           ASSIGN v_ret = '24'.
   END.
   /*
Provincias
0 - Cap. Federal
1 - Buenos Aires
2 - Catamarca
3 - C�rdoba
4 - Corrientes
5 - Entre R�os
6 - Jujuy
7 - Mendoza
8 - La Rioja
9 - Salta
10 - San Juan
11 - San Luis
12 - Santa Fe
13 - Sgo. Del Estero
14 - Tucum�n
16 - Chaco
17 - Chubut
18 - Formosa
19 - Misiones
20 - Neuquen
21 - La Pampa
22 - R�o Negro
23 - Santa Cruz
24 - Tierra del Fuego
*/
   RETURN v_ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRamo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRamo Procedure 
FUNCTION getRamo RETURNS CHARACTER
  (ip_prov AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v_ret AS CHARACTER INITIAL "0001".

  FIND r_proveedor_tipo_transporte WHERE
      r_proveedor_tipo_transporte.id_proveedor = ip_prov NO-LOCK NO-ERROR.

  IF AVAILABLE r_proveedor_tipo_transporte THEN
     v_ret = "0005".

  IF proveedores.id_tipo_proveedor = 3 THEN
     v_ret = "0003".
  IF proveedores.id_tipo_proveedor = 4 THEN
     v_ret = "0005".

  RETURN v_ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegionesSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegionesSap Procedure 
FUNCTION getRegionesSap RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcRegi AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = ".".
  FOR EACH regiones_sap
      WHERE TRIM(regiones_sap.clave_pais) = TRIM(pcPais) 
      NO-LOCK.
    IF descripcion MATCHES "*" + pcRegi + "*" THEN DO:
      cRet = TRIM(regiones_sap.clave_region).
      LEAVE.
    END.
      
  END.
      
  IF isNumeric(cRet) THEN cRet = cTxtCh + cRet.
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSedronarHazard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSedronarHazard Procedure 
FUNCTION getSedronarHazard RETURNS CHARACTER
  (INPUT ip_hazard AS INTEGER,
   INPUT ip_sedronar AS LOGICAL /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER INITIAL "004".

  /*-- Es Hazard unicamente --*/
  IF ip_hazard = 3   AND
     NOT ip_sedronar THEN
     cRet = "001".

  /*-- Es Sedronar unicamente --*/
  IF ip_hazard <> 3 AND
     ip_sedronar    THEN
     cRet = "002".

  /*-- Es Sedronar y Hazard --*/
  IF ip_hazard = 3 AND
     ip_sedronar   THEN
     cRet = "003".


  /*-- No es ninguno de los casos anteriores --*/
  IF ip_hazard <> 3  AND
     NOT ip_sedronar THEN
     cRet = "004".

  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTextoFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTextoFactura Procedure 
FUNCTION getTextoFactura RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.

    iArt = INTEGER(ENTRY(1, pcCod, "-")).
    iCal = INTEGER(ENTRY(2, pcCod, "-")).
    
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = iArt NO-LOCK NO-ERROR.
    FIND FIRST calidades WHERE calidades.id_calidad = iCal NO-LOCK NO-ERROR.
    
    IF AVAILABLE productos_terminados AND AVAILABLE calidades THEN DO:
      cRet = productos_terminados.descripcion_ingles + " " + calidades.descripcion_ingles.
    END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoMaterial Procedure 
FUNCTION getTipoMaterial RETURNS CHARACTER
  (ip_rubro AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v_ret AS CHARACTER FORMAT 'x(03)'.

   CASE ip_rubro:
       OTHERWISE
           v_ret = 'HAWA'.
       /*---------------------------
       WHEN 1 THEN
           ASSIGN v_ret = 'ZMSU'.
       WHEN 2 THEN
           ASSIGN v_ret = 'ZPQU'.
       WHEN 3 THEN
           ASSIGN v_ret = 'ZTAC'.
       WHEN 4 THEN
           ASSIGN v_ret = 'ZTJU'.
       WHEN 5 THEN
           ASSIGN v_ret = 'ZBCA'.
       WHEN 6 THEN
           ASSIGN v_ret = 'ZBJU'.
       WHEN 12 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 13 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 14 THEN
           ASSIGN v_ret = 'ZPAK'.
       WHEN 15 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 11 THEN
           ASSIGN v_ret = 'ZPAL'.
       WHEN 16 THEN
           ASSIGN v_ret = 'ZETI'.
       WHEN 20 THEN
           ASSIGN v_ret = 'ZFER'.
       WHEN 21 THEN
           ASSIGN v_ret = 'ZPLA'.
       WHEN 22 THEN
           ASSIGN v_ret = 'ZHER'.
       WHEN 25 THEN
           ASSIGN v_ret = 'ZCOM'.
       WHEN 26 THEN
           ASSIGN v_ret = 'ZLUB'.
       WHEN 27 THEN
           ASSIGN v_ret = 'ZNVA'.
       WHEN 28 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 29 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 30 THEN
           ASSIGN v_ret = 'ZHEM'.
       WHEN 31 THEN
           ASSIGN v_ret = 'ZALQ'.
       WHEN 32 THEN
           ASSIGN v_ret = 'WERT'.
       WHEN 33 THEN
           ASSIGN v_ret = 'ZEPP'.
       WHEN 34 THEN
           ASSIGN v_ret = 'ZROD'.
       WHEN 35 THEN
           ASSIGN v_ret = 'IBAU'.
       WHEN 36 THEN
           ASSIGN v_ret = 'ZMPR'.
       WHEN 37 THEN
           ASSIGN v_ret = 'ZALI'.
       OTHERWISE
           ASSIGN v_ret = 'WERT'.
       ---------------------------------------*/
   END.

   RETURN v_ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoUnidadEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoUnidadEnvase Procedure 
FUNCTION getTipoUnidadEnvase RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTipoUnidadVta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoUnidadVta Procedure 
FUNCTION getTipoUnidadVta RETURNS INTEGER
  (pcUnidad AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iReturn AS INTEGER    NO-UNDO.

  CASE pcUnidad:
    WHEN "GLN" THEN iReturn = 3.
    WHEN "KG" THEN  iReturn = 2.
    WHEN "LB" THEN  iReturn = 4.
    WHEN "T" THEN   iReturn = 1.
    WHEN "TR" THEN  iReturn = 6.

  END CASE.

  RETURN iReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadConversion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadConversion Procedure 
FUNCTION getUnidadConversion RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  iEnv = INTEGER(ENTRY(5, pcCod, "-")).

  CASE iEnv:
    WHEN 200 THEN cRet = "BLD".
    WHEN 201 THEN cRet = "BLD".

    WHEN 300 THEN cRet = "BID".
    WHEN 301 THEN cRet = "BID".
    WHEN 302 THEN cRet = "BID".
    WHEN 303 THEN cRet = "BID".
    WHEN 304 THEN cRet = "BID".

    WHEN 400 THEN cRet = "BIN".
    WHEN 401 THEN cRet = "BID".
    WHEN 402 THEN cRet = "BID".

    WHEN 500 THEN cRet = "BOL".

    WHEN 600 THEN cRet = "KG".
    WHEN 601 THEN cRet = "KG".
    WHEN 602 THEN cRet = "KG".
    WHEN 604 THEN cRet = "KG".
    OTHERWISE cRet = "TR".
  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadMedida Procedure 
FUNCTION getUnidadMedida RETURNS CHARACTER
  (ip_um AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v_ret AS CHARACTER FORMAT 'x(03)'.

   CASE ip_um:
       WHEN 1 THEN
           ASSIGN v_ret = 'UN'.
       WHEN 2 THEN
           ASSIGN v_ret = 'KG'.
       WHEN 3 THEN
           ASSIGN v_ret = 'M'.
       WHEN 4 THEN
           ASSIGN v_ret = 'L'.
       WHEN 5 THEN
           ASSIGN v_ret = 'M2'.
       WHEN 6 THEN
           ASSIGN v_ret = 'M3'.
       WHEN 7 THEN
           ASSIGN v_ret = 'G'.
       WHEN 8 THEN
           ASSIGN v_ret = 'CA'.
       WHEN 9 THEN
           ASSIGN v_ret = 'ROL'.
       WHEN 10 THEN
           ASSIGN v_ret = '%O'.
       WHEN 11 THEN
           ASSIGN v_ret = 'MG'.
       /*-- para SAMI se expresaba en Resma --*/
       WHEN 12 THEN
           ASSIGN v_ret = 'UN'.
       WHEN 13 THEN
           ASSIGN v_ret = 'ML'.
       WHEN 14 THEN
           ASSIGN v_ret = 'UN'.
       WHEN 15 THEN
           ASSIGN v_ret = 'BOL'.
       WHEN 16 THEN
           ASSIGN v_ret = 'UN'.
       WHEN 17 THEN
           ASSIGN v_ret = 'MON'.
       WHEN 18 THEN
           ASSIGN v_ret = 'D'.
       WHEN 19 THEN
           ASSIGN v_ret = 'UN'.
       OTHERWISE
           ASSIGN v_ret = 'UN'.
   END.

   RETURN v_ret.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedidaBaseIN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadMedidaBaseIN Procedure 
FUNCTION getUnidadMedidaBaseIN RETURNS CHARACTER
  (pcCod AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.


  cRet = "KG".

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getZonaVentas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getZonaVentas Procedure 
FUNCTION getZonaVentas RETURNS CHARACTER
  (pcPais AS CHARACTER, 
   pcSect AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF pcSect = "IN" THEN DO:
    CASE pcPais:
      WHEN "US" THEN cRet = "IN0001".
      WHEN "CA" THEN cRet = "IN0001".
      WHEN "FR" THEN cRet = "IN0009".
      WHEN "CH" THEN cRet = "IN0009".
      WHEN "DE" THEN cRet = "IN0009".
      WHEN "CN" THEN cRet = "IN0009".
      WHEN "JP" THEN cRet = "IN0002".
      WHEN "AR" THEN cRet = "IN0002".
      WHEN "IE" THEN cRet = "IN0009".
      WHEN "ES" THEN cRet = "IN0009".
      WHEN "HU" THEN cRet = "IN0009".
      WHEN "RU" THEN cRet = "IN0007".
      WHEN "SG" THEN cRet = "IN0003".
      WHEN "GB" THEN cRet = "IN0008".
      WHEN "MY" THEN cRet = "IN0009".
      WHEN "BE" THEN cRet = "IN0007".
      WHEN "PR" THEN cRet = "IN0002".
      WHEN "AT" THEN cRet = "IN0001".
      WHEN "AU" THEN cRet = "IN0001".
      WHEN "BN" THEN cRet = "IN0001".
      WHEN "BR" THEN cRet = "IN0002".
      WHEN "DK" THEN cRet = "IN0001".
      WHEN "IL" THEN cRet = "IN0006".
      WHEN "IT" THEN cRet = "IN0009".
      WHEN "NL" THEN cRet = "IN0008".
      WHEN "PT" THEN cRet = "IN0009".
      WHEN "TW" THEN cRet = "IN0003".
      WHEN "KR" THEN cRet = "IN0003".
      WHEN "LT" THEN cRet = "IN0009".
      WHEN "UY" THEN cRet = "IN0002".
      WHEN "MX" THEN cRet = "IN0002".
    END CASE.
  END.

  IF pcSect = "FF" THEN DO:
    CASE pcPais:
      WHEN "US" THEN cRet = "FF0001".
      WHEN "CA" THEN cRet = "FF0001".
      WHEN "FR" THEN cRet = "FF0002".
      WHEN "CH" THEN cRet = "FF0002".
      WHEN "DE" THEN cRet = "FF0002".
      WHEN "CN" THEN cRet = "FF0002".
      WHEN "JP" THEN cRet = "FF0004".
      WHEN "AR" THEN cRet = "FF0001".
      WHEN "IE" THEN cRet = "FF0002".
      WHEN "ES" THEN cRet = "FF0002".
      WHEN "HU" THEN cRet = "FF0002".
      WHEN "RU" THEN cRet = "FF0005".
      WHEN "SG" THEN cRet = "FF0002".
      WHEN "GB" THEN cRet = "FF0001".
      WHEN "MY" THEN cRet = "FF0002".
      WHEN "BE" THEN cRet = "FF0002".
      WHEN "PR" THEN cRet = "FF0001".
      WHEN "CL" THEN cRet = "FF0004".
      WHEN "CY" THEN cRet = "FF0005".
      WHEN "DK" THEN cRet = "FF0002".
      WHEN "FI" THEN cRet = "FF0001".
      WHEN "GR" THEN cRet = "FF0002".
      WHEN "HK" THEN cRet = "FF0004".
      WHEN "HR" THEN cRet = "FF0002".
      WHEN "ID" THEN cRet = "FF0002".
      WHEN "IT" THEN cRet = "FF0003".
      WHEN "LT" THEN cRet = "FF0004".
      WHEN "NL" THEN cRet = "FF0001".
      WHEN "PH" THEN cRet = "FF0004".
      WHEN "PL" THEN cRet = "FF0004".
      WHEN "PT" THEN cRet = "FF0002".
      WHEN "SA" THEN cRet = "FF0004".
      WHEN "SE" THEN cRet = "FF0002".
      WHEN "SI" THEN cRet = "FF0004".
      WHEN "TR" THEN cRet = "FF0004".
      WHEN "UA" THEN cRet = "FF0004".
      WHEN "VN" THEN cRet = "FF0004".
      WHEN "RO" THEN cRet = "FF0002".
    END CASE.
  END.


  


  RETURN cRet.

END FUNCTION.


/* 
  IN0001 - AMERICA NORTE Y CENTRAL
  IN0002 - AMERICA DEL SUR
  IN0002 - ASIA
  IN0004 - OCEANIA
  IN0005 - AFRICA
  IN0006 - MEDIO ORIENTE
  IN0007 - EUROPA ORIENTAL
  IN0008 - EUROPA DEL NORTE
  IN0009 - EUROPA MEDITERRANEA
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isNumeric) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isNumeric Procedure 
FUNCTION isNumeric RETURNS LOGICAL
  (pcValueString AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPos    AS INTEGER NO-UNDO.
  DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
  DEFINE VARIABLE cData   AS CHARACTER NO-UNDO.
  
  ASSIGN iLength = LENGTH(pcValueString).
  
  IF NUM-ENTRIES(pcValueString,".") > 2 THEN
  RETURN NO.
  
  DO iPos = 1 TO iLength:
  ASSIGN cData = SUBSTRING(pcValueString,iPos,1).
  IF cData < '0' OR cData > '9' THEN
  IF (iPos = 1 AND INDEX("+-.",cData) = 0) OR (iPos > 1 AND cData <> ".") THEN
  RETURN NO.
  END.
  
  RETURN YES.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putSpaceIfEmpty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION putSpaceIfEmpty Procedure 
FUNCTION putSpaceIfEmpty RETURNS CHARACTER
  (pcData AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  cRet = IF pcData = "" THEN cSpace ELSE pcData.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDatosBanco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDatosBanco Procedure 
FUNCTION setDatosBanco RETURNS CHARACTER
  (iPaymentInstruc AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  ASSIGN
      cBANKS = cSpace
      cBANKN = cSpace
      cBANKL = cSpace
      cBKONT = cSpace 
      cBVTYP = cSpace
      cXEZER = cSpace
      cBKREF = cSpace
      cBKREF = cSpace
      cBKREF = cSpace
      cKOINH = cSpace   
    .

  RETURN "". /* QUITAR ESTA LINEA CUANDO TENGAMOS BANCOS*/

  FOR FIRST instruc_pago
      WHERE instruc_pago.id_instruc_pago = iPaymentInstruc
      NO-LOCK.

    ASSIGN
      cBANKS = putSpaceIfEmpty(getPaisFromLeyenda(instruc_pago.nombre))
      cBANKN = putSpaceIfEmpty(instruc_pago.account)
      cBANKL = putSpaceIfEmpty(getBancoSap(instruc_pago.id_instruc_pago)) /*faltan crear bancos en sap*/
      cBKONT = cSpace 
      cBVTYP = cSpace
      cXEZER = cSpace
      cBKREF = REPLACE(instruc_pago.referencia, CHR(10), " ")
      cBKREF = REPLACE(cBKREF, CHR(13), " ")
      cBKREF = putSpaceIfEmpty(SUBSTRING(cBKREF, 1, 20))
      cKOINH = putSpaceIfEmpty(instruc_pago.beneficiary)   
    .
    
  END.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
