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


DEFINE TEMP-TABLE ttStockFecha
  RCODE-INFORMATION
  FIELD id_empresa            AS INTEGER COLUMN-LABEL "id_empresa"
  FIELD id_sucursal           AS INTEGER COLUMN-LABEL "id_sucursal"
  FIELD id_tipotambor         AS INTEGER COLUMN-LABEL "id_tipotambor"
  FIELD nromov                AS INTEGER COLUMN-LABEL "nromov"
  FIELD id_sucursal_ubicacion AS INTEGER COLUMN-LABEL "Suc Ubicacion"
  FIELD id_tambor             AS INTEGER COLUMN-LABEL "Tambor"
  FIELD id_articulo           AS INTEGER COLUMN-LABEL "id_articulo" 
  FIELD id_calidad            AS INTEGER COLUMN-LABEL "id_calidad"
  FIELD id_envase             AS INTEGER COLUMN-LABEL "id_envase"
  FIELD id_lote               AS INTEGER COLUMN-LABEL "id_lote" 
  FIELD anio_lote             AS INTEGER COLUMN-LABEL "anio_lote"
  FIELD kilos                 AS INTEGER COLUMN-LABEL "kilos"
  FIELD cantidad              AS INTEGER COLUMN-LABEL "tambores"
  FIELD sucursal              AS CHARACTER COLUMN-LABEL "Suc Lote"
  FIELD ubicacion             AS CHARACTER COLUMN-LABEL "Suc Ubicacion"
  FIELD articulo              AS CHARACTER COLUMN-LABEL "Articulo"
  FIELD calidad               AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase                AS CHARACTER COLUMN-LABEL "Envase"
  FIELD fecha                 AS DATE COLUMN-LABEL "Fecha"
  FIELD eliminar              AS LOGICAL COLUMN-LABEL "a Eliminar".


DEFINE TEMP-TABLE ttUbiCam  
  FIELD id_empresa             AS INTEGER
  FIELD id_sucursal            AS INTEGER
  FIELD id_tipotambor          AS INTEGER
  FIELD nromov                 AS INTEGER
  FIELD id_sucursal_ubicacion  AS INTEGER
  FIELD id_tambor              AS INTEGER
  FIELD id_empresa_camara      AS INTEGER
  FIELD id_sucursal_camara     AS INTEGER
  FIELD id_camara              AS INTEGER
  FIELD nro_fila               AS CHARACTER
  FIELD nro_columna            AS CHARACTER
  FIELD ubicacion              AS CHARACTER
  FIELD camara                 AS CHARACTER
  FIELD obs                    AS CHARACTER
  FIELD id_lote                AS INTEGER
  FIELD anio                   AS INTEGER
  FIELD id_articulo            AS INTEGER
  FIELD id_calidad             AS INTEGER
  FIELD id_envase              AS INTEGER
  FIELD articulo               AS CHARACTER
  FIELD calidad                AS CHARACTER
  FIELD envase                 AS CHARACTER.


DEFINE TEMP-TABLE ttRangos
    FIELD id_empresa      AS INTEGER
    FIELD id_sucursal     AS INTEGER
    FIELD id_tipotambor   AS INTEGER
    FIELD nromov          AS INTEGER
    FIELD id_tambor_desde AS INTEGER
    FIELD id_tambor_hasta AS INTEGER
    FIELD id_articulo     AS INTEGER
    FIELD id_calidad      AS INTEGER
    FIELD id_envase       AS INTEGER
    FIELD kilos_tambor    AS DECIMAL
    FIELD tara            AS DECIMAL
    FIELD id_lote         AS INTEGER
    FIELD anio            AS INTEGER
    .

  DEFINE TEMP-TABLE ttSelected
    FIELD id_empresa      AS INTEGER
    FIELD id_sucursal     AS INTEGER
    FIELD id_tipotambor   AS INTEGER
    FIELD nromov          AS INTEGER  
    FIELD id_tambor       AS INTEGER
    FIELD id_articulo     AS INTEGER
    FIELD id_calidad      AS INTEGER
    FIELD id_envase       AS INTEGER
    FIELD kilos_tambor    AS DECIMAL
    FIELD tara            AS DECIMAL
    FIELD id_lote         AS INTEGER
    FIELD anio            AS INTEGER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getAcidezGPL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAcidezGPL Procedure 
FUNCTION getAcidezGPL RETURNS DECIMAL
  (pdBx2020 AS DECIMAL, 
   pdAcPorc AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAporteGplNoRepro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAporteGplNoRepro Procedure 
FUNCTION getAporteGplNoRepro RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAporteGplRepro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAporteGplRepro Procedure 
FUNCTION getAporteGplRepro RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getArticulosMPJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulosMPJugo Procedure 
FUNCTION getArticulosMPJugo RETURNS CHARACTER
  (INPUT prRow    AS ROWID, 
   INPUT pcFuente AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrix2020) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrix2020 Procedure 
FUNCTION getBrix2020 RETURNS DECIMAL
  (pdRatio AS DECIMAL, 
   pdAcPor AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrixCorregido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrixCorregido Procedure 
FUNCTION getBrixCorregido RETURNS DECIMAL
  (pdBx2020 AS DECIMAL, 
   pdAcidez AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantEnvases) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantEnvases Procedure 
FUNCTION getCantEnvases RETURNS CHARACTER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadTambores Procedure 
FUNCTION getCantidadTambores RETURNS CHARACTER
  (pdKilos  AS DECIMAL, 
   piEnvase AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getClaveTamborFromRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClaveTamborFromRowid Procedure 
FUNCTION getClaveTamborFromRowid RETURNS CHARACTER
  (prTambor AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefConversion400) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoefConversion400 Procedure 
FUNCTION getCoefConversion400 RETURNS DECIMAL
  (piTipoTambor AS INTEGER,
   piArticulo   AS INTEGER,
   piCalidad    AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefCorreccionAcidez) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoefCorreccionAcidez Procedure 
FUNCTION getCoefCorreccionAcidez RETURNS DECIMAL
  (pdAcidez AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefPesoEspecifico) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoefPesoEspecifico Procedure 
FUNCTION getCoefPesoEspecifico RETURNS DECIMAL
  (pdBrix AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefSolidosSolubles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoefSolidosSolubles Procedure 
FUNCTION getCoefSolidosSolubles RETURNS DECIMAL
  (pdBrix AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposeNroLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getComposeNroLote Procedure 
FUNCTION getComposeNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER,
   piLot AS INTEGER,
   piAno AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getComposicionCarga Procedure 
FUNCTION getComposicionCarga RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionProceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getComposicionProceso Procedure 
FUNCTION getComposicionProceso RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getComposicionTambor Procedure 
FUNCTION getComposicionTambor RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   piTbo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlCalidadLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlCalidadLote Procedure 
FUNCTION getControlCalidadLote RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlTamborLeido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlTamborLeido Procedure 
FUNCTION getControlTamborLeido RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   pcAct AS CHARACTER,    /*accion*/
   pcArg AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlVolumen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlVolumen Procedure 
FUNCTION getControlVolumen RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCumplimentoContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCumplimentoContrato Procedure 
FUNCTION getCumplimentoContrato RETURNS CHARACTER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosTambor Procedure 
FUNCTION getDatosTambor RETURNS CHARACTER
  (prTambor AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescArticulo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescArticulo Procedure 
FUNCTION getDescArticulo RETURNS CHARACTER
  (piArticulo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescCalidad Procedure 
FUNCTION getDescCalidad RETURNS CHARACTER
  (piCalidad AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescEnvase Procedure 
FUNCTION getDescEnvase RETURNS CHARACTER
  (piEnvase AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescSucursal Procedure 
FUNCTION getDescSucursal RETURNS CHARACTER
  (piSucursal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescTipoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescTipoTambor Procedure 
FUNCTION getDescTipoTambor RETURNS CHARACTER
  (piTipoTambor AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFactorInterpolacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFactorInterpolacion Procedure 
FUNCTION getFactorInterpolacion RETURNS DECIMAL
  (pdMax AS DECIMAL,
   pdMin AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFliaProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFliaProducto Procedure 
FUNCTION getFliaProducto RETURNS INTEGER
  (piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalonesLote Procedure 
FUNCTION getGalonesLote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInterpolacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInterpolacion Procedure 
FUNCTION getInterpolacion RETURNS DECIMAL
  (pdX  AS DECIMAL,
   pdX1 AS DECIMAL,
   pdY1 AS DECIMAL,
   pdX2 AS DECIMAL,
   pdY2 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400 Procedure 
FUNCTION getKilos400 RETURNS DECIMAL
  (piTipoTambor AS INTEGER, 
   piArticulo   AS INTEGER, 
   piCalidad    AS INTEGER, 
   pdKilos      AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400Gpl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400Gpl Procedure 
FUNCTION getKilos400Gpl RETURNS DECIMAL
  (pdBx2020 AS DECIMAL,
   pdAcPorc AS DECIMAL,
   pdLitros AS DECIMAL, 
   plShowDt AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400GplCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400GplCarga Procedure 
FUNCTION getKilos400GplCarga RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400GplTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400GplTambor Procedure 
FUNCTION getKilos400GplTambor RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   piTam AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400Lote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400Lote Procedure 
FUNCTION getKilos400Lote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosFromAcidez) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosFromAcidez Procedure 
FUNCTION getKilosFromAcidez RETURNS DECIMAL
  (pdAcidez AS DECIMAL,
   pdBx2020 AS DECIMAL,
   pdLitros AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosFromBxCorreg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosFromBxCorreg Procedure 
FUNCTION getKilosFromBxCorreg RETURNS DECIMAL
  (pdBxc AS DECIMAL, 
   pdLit AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosLote Procedure 
FUNCTION getKilosLote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLitrosTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLitrosTambor Procedure 
FUNCTION getLitrosTambor RETURNS DECIMAL
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER,   
   pdKil AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteExistente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteExistente Procedure 
FUNCTION getLoteExistente RETURNS LOGICAL
  (piSuc AS INTEGER, 
   piTip AS INTEGER,
   piLot AS INTEGER,
   piAno AS INTEGER,
   piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneIngredientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneIngredientes Procedure 
FUNCTION getLoteTieneIngredientes RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneInspeccionFinal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneInspeccionFinal Procedure 
FUNCTION getLoteTieneInspeccionFinal RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneInsumos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneInsumos Procedure 
FUNCTION getLoteTieneInsumos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneOE Procedure 
FUNCTION getLoteTieneOE RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneRemitos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneRemitos Procedure 
FUNCTION getLoteTieneRemitos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneReprocesos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteTieneReprocesos Procedure 
FUNCTION getLoteTieneReprocesos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroLote Procedure 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER, 
   piAno AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroProcesoAceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroProcesoAceite Procedure 
FUNCTION getNextNroProcesoAceite RETURNS INTEGER
  (piSuc AS INTEGER,
   piArt AS INTEGER,
   piAno AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrigenesCargados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrigenesCargados Procedure 
FUNCTION getOrigenesCargados RETURNS LOGICAL
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParamsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParamsFile Procedure 
FUNCTION getParamsFile RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPerdidaClarificado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPerdidaClarificado Procedure 
FUNCTION getPerdidaClarificado RETURNS CHARACTER
  (pdDesde AS DATE,
   pdHasta AS DATE, 
   piSuc   AS INTEGER, 
   plRecal AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuimicoName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoName Procedure 
FUNCTION getQuimicoName RETURNS CHARACTER
  (piQuimico AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRangos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRangos Procedure 
FUNCTION getRangos RETURNS CHARACTER
  (pcRowIds AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRatio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRatio Procedure 
FUNCTION getRatio RETURNS DECIMAL
  (pdBrix   AS DECIMAL,
   pdAcidez AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSolidosSolubles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSolidosSolubles Procedure 
FUNCTION getSolidosSolubles RETURNS DECIMAL
  (pdBrix   AS DECIMAL, /*corregido*/
   pdLitros AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStockFechaSucursalTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStockFechaSucursalTambor Procedure 
FUNCTION getStockFechaSucursalTambor RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   piTbo AS INTEGER, 
   pdFec AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTaraEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTaraEnvase Procedure 
FUNCTION getTaraEnvase RETURNS DECIMAL
  (piEnvase AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUltimoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUltimoTambor Procedure 
FUNCTION getUltimoTambor RETURNS INTEGER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUsuariosLista) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUsuariosLista Procedure 
FUNCTION getUsuariosLista RETURNS CHARACTER
  (piLista AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValoresAnalisis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValoresAnalisis Procedure 
FUNCTION getValoresAnalisis RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

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
         HEIGHT             = 14.71
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addDrums) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addDrums Procedure 
PROCEDURE addDrums :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO.  
  DEFINE INPUT  PARAMETER piLote       AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnvase     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCalidad    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio       AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha      AS DATE       NO-UNDO.  
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/


  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEtq AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTar AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.

  dTar = pdTara.    /*revisar esto*/
  IF pdTara = 0 OR (pdTara = piEnvase) THEN DO:
    dTar = getTaraEnvase(piEnvase).
  END.

  iNro = piDesde.
  /*DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":  */ /*esto lo comento porque cuando corro este codigo desde un sdo, esta transaccion prevalece sobre la del sdo y da error*/
    DO i = 1 TO piCantidad:   

      iEtq = IF piSucursal = 96 THEN NEXT-VALUE(tambores) ELSE NEXT-VALUE(tambores_famailla).

      cCod = getComposeNroLote(piSucursal,
                               piArticulo, 
                               piLote,
                               piAnio).

      CREATE tambores_industria.
      ASSIGN tambores_industria.id_empresa            = piEmpresa
             tambores_industria.id_sucursal           = piSucursal
             tambores_industria.id_lote               = piLote
             tambores_industria.id_articulo           = piArticulo
             tambores_industria.id_tipotambor         = piTipoTambor
             tambores_industria.nromov                = piNromov
             tambores_industria.id_tambor             = iNro
             tambores_industria.fecha                 = pdFecha
             tambores_industria.anio                  = IF piAnio <> 0 THEN piAnio ELSE YEAR(TODAY)
             tambores_industria.id_envase             = piEnvase
             tambores_industria.tara                  = dTar
             tambores_industria.c_usuario             = USERID("userdb")
             tambores_industria.c_fecha               = TODAY
             tambores_industria.c_hora                = STRING(TIME,"HH:MM:SS")
             tambores_industria.kilos_tambor          = pdKilos
             tambores_industria.id_empresa_ubicacion  = 1
             tambores_industria.id_sucursal_ubicacion = piSucursal
             tambores_industria.id_locacion_ubicacion = 4
             tambores_industria.id_posicion_ubicacion = 1
             tambores_industria.id_calidad            = piCalidad
             tambores_industria.id_etiqueta           = iEtq 
             tambores_industria.id_estado             = piEstado
             tambores_industria.id_empresa_camara     = 1
             tambores_industria.id_sucursal_camara    = piSucursal
             tambores_industria.id_camara             = 0
             tambores_industria.codigo_lote           = cCod
             .
      iNro = iNro + 1.
    END. 
    IF plRegMov THEN DO:      
      RUN y_gstkcre.p (piEmpresa,
                       piSucursal,
                       piTipoTambor,
                       piNroMov,
                       piDesde,
                       piHasta,
                       1) "tambores_industria".
            
      IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
      END.
    END.
  /*END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addMailingSpoolEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addMailingSpoolEntry Procedure 
PROCEDURE addMailingSpoolEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcAct AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cUsr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQty AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBdy AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAvb AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec AS DATE       NO-UNDO.

  DEFINE BUFFER buTamIns FOR tambores_industria.

  crlf = CHR(13) + CHR(10).

  FOR EACH buTamIns WHERE buTamIns.id_empresa    = piEmp 
                      AND buTamIns.id_sucursal   = piSuc
                      AND buTamIns.id_tipotambor = piTip
                      AND buTamIns.nromov        = piNro
                    NO-LOCK.
    i = i + 1.  
    cLot = STRING(buTamIns.id_lote) + "/" + STRING(buTamIns.anio).
    ASSIGN iArt = buTamIns.id_articulo
           iCal = buTamIns.id_calidad
           iEnv = buTamIns.id_envase
           dFec = buTamIns.fecha.
  END.

  IF piSuc = 95 THEN DO:
    cAvb = "(FAM)".
    cUsr = "depositof@sa-sanmiguel.com,mmuroni@sa-sanmiguel.com".
  END.

  IF piSuc = 96 THEN DO:
    cAvb = "(LAV)".
    cUsr = "jchayle@sa-sanmiguel.com".
  END.
  
  cUsr = cUsr + ",rvelez@sa-sanmiguel.com,diegomf@sa-sanmiguel.com,depositoc@sa-sanmiguel.com,rocaran@sa-sanmiguel.com,ralvarez@sa-sanmiguel.com".
  
  
  cArt = getDescArticulo(iArt).
  cCal = getDescCalidad(iCal).
  cEnv = getDescEnvase(iEnv).
  cPro = getDescTipoTambor(piTip).
  cQty = getCantEnvases(piEmp, piSuc, piTip, piNro).
  cSuc = getDescSucursal(piSuc).

  CASE pcAct:
    WHEN "create" THEN 
      cSub  = "ATENCION " + cAvb + ": Consumo Envases Industria".
    WHEN "delete" THEN
      cSub  = "IMPORTANTE " + cAvb + " : DEVOLUCION consumo Envases Industria".
  END CASE.
  

  cBdy     = "Planta:          " + cSuc + crlf + 
             "Lote:            " + cLot + crlf + 
             "Tipo:            " + cArt + crlf + 
             "Calidad:         " + cCal + crlf + 
             "Envase:          " + cEnv + crlf + 
             "Cantidad:        " + STRING(i) + "  envases " + crlf +
             "Env. Nuevos:     " + ENTRY(1, cQty) + crlf + 
             "Env. Recup:      " + ENTRY(2, cQty) + crlf + 
             "Fecha Creacion:  " + STRING(dFec) + crlf + crlf + 
             "Ref. p/sistemas: " + STRING(piEmp) + "," + STRING(piSuc) + "," + STRING(piTip) + "," + STRING(piNro).

  CREATE mailing_insumos.
  ASSIGN mailing_insumos.id_empresa     = piEmp
         mailing_insumos.id_sucursal    = piSuc
         mailing_insumos.id_tipotambor  = piTip
         mailing_insumos.nromov         = piNro
         mailing_insumos.action         = pcAct
         mailing_insumos.fecha          = TODAY
         mailing_insumos.destinatario   = cUsr
         mailing_insumos.subject        = cSub 
         mailing_insumos.body           = cBdy 
         mailing_insumos.enviado        = "NO"
         mailing_insumos.c_usuario      = USERID("userdb")
         mailing_insumos.c_fecha        = TODAY
         mailing_insumos.c_hora         = STRING(TIME, "hh:mm:ss").

  RELEASE mailing_insumos.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addMovimientoCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addMovimientoCamara Procedure 
PROCEDURE addMovimientoCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambor     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucMovi    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTurno      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piMovimiento AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piItem       AS INTEGER    NO-UNDO.

  DEFINE BUFFER buMov FOR r_mov_camara_tambor.

  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.
  
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FIND FIRST buMov WHERE buMov.id_empresa             = piEmpresa
                       AND buMov.id_sucursal            = piSucursal
                       AND buMov.id_tipotambor          = piTipoTambor
                       AND buMov.nromov                 = piNroMov
                       AND buMov.id_tambor              = piTambor
                       AND buMov.id_sucursal_movimiento = piSucMovi
                       AND buMov.id_turno               = piTurno
                       AND buMov.fecha                  = pdFecha
                       AND buMov.id_movimiento_camara   = piMovimiento
                       AND buMov.ITEM                   = piItem
                     NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buMov THEN DO:
      CREATE r_mov_camara_tambor.
      ASSIGN r_mov_camara_tambor.id_empresa             = piEmpresa
             r_mov_camara_tambor.id_sucursal            = piSucursal
             r_mov_camara_tambor.id_tipotambor          = piTipoTambor
             r_mov_camara_tambor.nromov                 = piNroMov
             r_mov_camara_tambor.id_tambor              = piTambor
             r_mov_camara_tambor.id_sucursal_movimiento = piSucMovi
             r_mov_camara_tambor.id_turno               = piTurno
             r_mov_camara_tambor.fecha                  = pdFecha
             r_mov_camara_tambor.id_movimiento          = piMovimiento
             r_mov_camara_tambor.ITEM                   = piItem.
      
      /*grabo la camara en el tambor*/
      FIND FIRST items_movimiento_camara WHERE items_movimiento_camara.id_sucursal    = piSucMovi
                                           AND items_movimiento_camara.id_turno       = piTurno
                                           AND items_movimiento_camara.fecha          = pdFecha
                                           AND items_movimiento_camara.id_movimiento  = piMovimiento
                                           AND items_movimiento_camara.ITEM           = piItem
                                         NO-LOCK NO-ERROR.
      IF AVAILABLE items_movimiento_camara THEN DO:
        IF items_movimiento_camara.id_tipo_movimiento = 2 THEN
          ASSIGN cFil = items_movimiento_camara.nro_fila_origen
                 cCol = items_movimiento_Camara.nro_columna_origen.
        ELSE
          ASSIGN cFil = items_movimiento_camara.nro_fila
                 cCol = items_movimiento_Camara.nro_columna.

        RUN setMovimientoCamaraTambor (piEmpresa, 
                                       piSucursal, 
                                       piTipoTambor, 
                                       piNroMov,
                                       piTambor,
                                       piEmpresa, 
                                       piSucMovi, 
                                       items_movimiento_camara.id_camara, 
                                       cFil,
                                       cCol).
      END.
    END.
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-asocTamboresOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asocTamboresOE Procedure 
PROCEDURE asocTamboresOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOE  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piItm AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE p    AS DECIMAL    NO-UNDO.
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa     = piEmp
        AND tambores_industria.id_sucursal    = piSuc
        AND tambores_industria.id_tipotambor  = piTip
        AND tambores_industria.nromov         = piNro
        AND tambores_industria.id_tambor     >= piDes
        AND tambores_industria.id_tambor     <= piHas.

    ASSIGN i = i + 1
           k = k + tambores_industria.kilos_tambor
           p = p + (tambores_industria.kilos_tambor + tambores_industria.tara)
           .

    RUN setOEToTambor(piEmp, 
                      piSuc,
                      piTip,
                      piNro,
                      tambores_industria.id_tambor, 
                      piOE,
                      piItm).
  END.

  FOR FIRST items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piItm.

    IF piOE <> 0 AND piItm <> 0 THEN
      ASSIGN items_orden_entrega.kgs_netos         = items_orden_entrega.kgs_netos + k
             items_orden_entrega.kgs_brutos        = items_orden_entrega.kgs_brutos + p
             items_orden_entrega.cantidad_tambores = items_orden_entrega.cantidad_tambores + i.
    ELSE
      ASSIGN items_orden_entrega.kgs_netos         = items_orden_entrega.kgs_netos - k
             items_orden_entrega.kgs_brutos        = items_orden_entrega.kgs_brutos - p
             items_orden_entrega.cantidad_tambores = items_orden_entrega.cantidad_tambores - i.
           

  END.
  
  DYNAMIC-FUNCTION('calcularOE' IN hLib, piOE, FALSE).
  
  
  IF piOE <> 0 THEN RETURN.
  /*poner fecha en documento_oe asociacion de tambores*/
  FOR FIRST documentos_oe
      WHERE documentos_oe.id_orden_entrega  = piOE
        AND documentos_oe.id_tipo_documento = 4.

    ASSIGN documentos_oe.fecha_cumplido = TODAY.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-corregirTara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE corregirTara Procedure 
PROCEDURE corregirTara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTar AS DECIMAL    NO-UNDO.


  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":  
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                  AND tambores_industria.id_tambor    >= piDes
                                  AND tambores_industria.id_tambor    <= piHas.
  
      ASSIGN tambores_industria.tara = pdTar.
      
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createCabeceraLoteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createCabeceraLoteJugo Procedure 
PROCEDURE createCabeceraLoteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAno AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnv AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    CREATE lotes_jugo.
    ASSIGN lotes_jugo.codigo_lote    = getComposeNroLote(piSuc, piArt, piPro, piAno)
           lotes_jugo.id_empresa     = piEmp
           lotes_jugo.id_sucursal    = piSuc
           lotes_jugo.id_tipotambor  = piTip
           lotes_jugo.nromov         = piNro
           lotes_jugo.id_lote        = piPro
           lotes_jugo.anio           = piAno
           lotes_jugo.fecha          = pdFec
           lotes_jugo.id_articulo    = piArt
           lotes_jugo.id_calidad     = piCal
           lotes_jugo.id_envase      = piEnv           
           .

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createCabeceraProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createCabeceraProduccion Procedure 
PROCEDURE createCabeceraProduccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAno AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnv AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    CREATE produccion_jugo.
    ASSIGN produccion_jugo.codigo_prod    = getComposeNroLote(piSuc, piArt, piPro, piAno)
           produccion_jugo.id_empresa     = piEmp
           produccion_jugo.id_sucursal    = piSuc
           produccion_jugo.id_tipotambor  = piTip
           produccion_jugo.nromov         = piNro
           produccion_jugo.id_produccion  = piPro
           produccion_jugo.anio           = piAno
           produccion_jugo.fecha          = pdFec
           produccion_jugo.id_articulo    = piArt
           produccion_jugo.id_calidad     = piCal
           produccion_jugo.id_envase_1    = piEnv  
           produccion_jugo.c_usuario      = USERID("userdb")
           produccion_jugo.c_fecha        = TODAY
           produccion_jugo.c_hora         = STRING(TIME,"HH:MM:SS")
           .

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createComposicionLoteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createComposicionLoteJugo Procedure 
PROCEDURE createComposicionLoteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piQty AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil AS DECIMAL    NO-UNDO.

  FIND FIRST composicion_lote WHERE composicion_lote.id_empresa     = piEmp
                                AND composicion_lote.id_sucursal    = piSuc
                                AND composicion_lote.id_tipotambor  = piTip
                                AND composicion_lote.nromov         = piNro
                              NO-ERROR.
  IF NOT AVAILABLE composicion_lote THEN DO:
    CREATE composicion_lote.
    ASSIGN composicion_lote.id_empresa        = piEmp
           composicion_lote.id_sucursal       = piSuc
           composicion_lote.id_tipotambor     = piTip
           composicion_lote.nromov            = piNro
           composicion_lote.cantidad          = piQty
           composicion_lote.numeracion_desde  = piDes
           composicion_lote.numeracion_hasta  = piHas
           composicion_lote.fecha             = pdFec
           composicion_lote.kilos_tambor      = pdKil
           composicion_lote.c_usuario         = USERID('userdb')
           composicion_lote.c_fecha           = TODAY
           composicion_lote.c_hora            = STRING(TIME).

  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createDrumsInBatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createDrumsInBatch Procedure 
PROCEDURE createDrumsInBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.

  
  DEFINE VARIABLE cKey AS CHARACTER  NO-UNDO.

  CASE piTipoTambor:
    WHEN 1 THEN /*producciones de jugo*/
      RUN createTamboresProduccionJugo(piEmpresa,
                                       piSucursal, 
                                       piTipoTambor, 
                                       piNroMov, 
                                       piCantidad, 
                                       piDesde, 
                                       piHasta, 
                                       pdKilos, 
                                       pdTara, 
                                       piEstado,
                                       TRUE).

    WHEN 3 THEN /*lotes de jugo*/
      RUN createTamboresJugo(piEmpresa,
                             piSucursal, 
                             piTipoTambor, 
                             piNroMov, 
                             piCantidad, 
                             piDesde, 
                             piHasta, 
                             pdKilos, 
                             pdTara, 
                             piEstado,
                             TRUE).

    WHEN 4 THEN /*sobrante lote jugo*/
      RUN createTamboresSobranteJugo(piEmpresa,
                                     piSucursal, 
                                     piTipoTambor, 
                                     piNroMov, 
                                     piCantidad, 
                                     piDesde, 
                                     piHasta, 
                                     pdKilos, 
                                     pdTara, 
                                     piEstado,
                                     TRUE).

    WHEN 5 THEN /*arrastre lote jugo*/
      RUN createTamboresArrastreJugo(piEmpresa,
                                     piSucursal, 
                                     piTipoTambor, 
                                     piNroMov, 
                                     piCantidad, 
                                     piDesde, 
                                     piHasta, 
                                     pdKilos, 
                                     pdTara, 
                                     piEstado,
                                     TRUE).

    WHEN 6 THEN /*lote de aceite*/
      RUN createTamboresAceite(piEmpresa,
                               piSucursal, 
                               piTipoTambor, 
                               piNroMov, 
                               piCantidad, 
                               piDesde, 
                               piHasta, 
                               pdKilos, 
                               pdTara,
                               piEstado, 
                               TRUE).

    WHEN 8 THEN /*sobrante de aceite*/
      RUN createTamboresSobranteAceite(piEmpresa,
                                       piSucursal, 
                                       piTipoTambor, 
                                       piNroMov, 
                                       piCantidad, 
                                       piDesde, 
                                       piHasta, 
                                       pdKilos, 
                                       pdTara, 
                                       piEstado, 
                                       TRUE).
  END CASE.

  /*agrego entrada en el spool de mails a depositos*/
  RUN addMailingSpoolEntry(piEmpresa,
                           piSucursal,
                           piTipoTambor,
                           piNroMov, 
                           "create").
  
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresAceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresAceite Procedure 
PROCEDURE createTamboresAceite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS INTEGER    NO-UNDO.  
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  
  FIND FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmpresa
                            AND lotes_aceite.id_sucursal   = piSucursal
                            AND lotes_aceite.id_tipotambor = piTipoTambor
                            AND lotes_aceite.nromov        = piNroMov
                          NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_aceite THEN DO:  
    FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = lotes_aceite.id_articulo
                                   NO-LOCK NO-ERROR.
    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 lotes_aceite.id_lote, 
                 lotes_aceite.id_articulo, 
                 lotes_aceite.id_envase, 
                 IF AVAILABLE r_productos_calidad THEN r_productos_calidad.id_calidad ELSE 602, 
                 lotes_aceite.anio, 
                 piEstado, 
                 lotes_aceite.fecha,                  
                 TRUE).
  END.

  /*registrar ingreso en lotes_ubicacion */
  RUN updateLoteUbicacion (piEmpresa, 
                           piSucursal, 
                           piTipoTambor, 
                           piNroMov, 
                           piSucursal).

  
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresArrastreJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresArrastreJugo Procedure 
PROCEDURE createTamboresArrastreJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS INTEGER    NO-UNDO.  
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt    AS INTEGER    NO-UNDO.

  FIND FIRST arrastre_lote WHERE arrastre_lote.id_empresa             = piEmpresa
                             AND arrastre_lote.id_sucursal            = piSucursal
                             AND arrastre_lote.id_tipotambor_arrastre = piTipoTambor
                             AND arrastre_lote.nromov_arrastre        = piNroMov
                           NO-LOCK NO-ERROR.
  IF AVAILABLE arrastre_lote THEN DO:  
    FIND FIRST lotes_jugo WHERE lotes_jugo.nromov = arrastre_lote.nromov
                          NO-LOCK NO-ERROR.
    iArt = lotes_jugo.id_articulo.

    IF iArt = 52 THEN 
      iArt = 523.
    IF iArt = 53 THEN
      iArt = 535.


    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 arrastre_lote.id_lote, 
                 iArt, 
                 arrastre_lote.id_envase, 
                 lotes_jugo.id_calidad, 
                 lotes_jugo.anio, 
                 piEstado, 
                 arrastre_lote.fecha,                  
                 TRUE).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresJugo Procedure 
PROCEDURE createTamboresJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO.  
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/


  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:   
    
    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 lotes_jugo.id_lote, 
                 lotes_jugo.id_articulo, 
                 lotes_jugo.id_envase, 
                 lotes_jugo.id_calidad, 
                 lotes_jugo.anio, 
                 piEstado, 
                 lotes_jugo.fecha, 
                 TRUE).
  END.

  /*registrar ingreso en lotes_ubicacion */
  RUN updateLoteUbicacion (piEmpresa, 
                           piSucursal, 
                           piTipoTambor, 
                           piNroMov, 
                           piSucursal).

  /*
  /*mailing insumos*/
  RUN mailingInsumos (piEmpresa, 
                      piSucursal, 
                      piTipoTambor, 
                      piNroMov, 
                      "create").
                      */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresProduccionJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresProduccionJugo Procedure 
PROCEDURE createTamboresProduccionJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO. /*aqui viene el codigo de envase*/ 
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/


  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

  FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = piEmpresa
                               AND produccion_jugo.id_sucursal   = piSucursal
                               AND produccion_jugo.id_tipotambor = piTipoTambor
                               AND produccion_jugo.nromov        = piNroMov
                             NO-LOCK NO-ERROR.
  IF AVAILABLE produccion_jugo THEN DO:   
    IF pdTara = 0  THEN
      pdTara = produccion_jugo.id_envase_1.

    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 produccion_jugo.id_produccion, 
                 produccion_jugo.id_articulo, 
                 pdTara, 
                 produccion_jugo.id_calidad, 
                 produccion_jugo.anio, 
                 piEstado, 
                 produccion_jugo.fecha, 
                 TRUE).
  END.

  /*
  /*registrar ingreso en lotes_ubicacion */
  RUN updateLoteUbicacion (piEmpresa, 
                           piSucursal, 
                           piTipoTambor, 
                           piNroMov, 
                           piSucursal).
 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresSobranteAceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresSobranteAceite Procedure 
PROCEDURE createTamboresSobranteAceite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO.  
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

  FIND FIRST sobrante_lotes_aceite WHERE sobrante_lotes_aceite.id_empresa             = piEmpresa
                                     AND sobrante_lotes_aceite.id_sucursal            = piSucursal
                                     AND sobrante_lotes_aceite.id_tipotambor_sobrante = piTipoTambor
                                     AND sobrante_lotes_aceite.nromov_sobrante        = piNroMov
                                   NO-LOCK NO-ERROR.
  IF AVAILABLE sobrante_lotes_aceite THEN DO:  
    FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = sobrante_lotes_aceite.id_articulo
                                   NO-LOCK NO-ERROR.

    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 sobrante_lotes_aceite.id_lote, 
                 sobrante_lotes_aceite.id_articulo, 
                 sobrante_lotes_aceite.id_envase, 
                 IF AVAILABLE r_productos_calidad THEN r_productos_calidad.id_calidad ELSE 602, 
                 YEAR(TODAY), 
                 piEstado, 
                 sobrante_lotes_aceite.fecha_elaboracion,                  
                 TRUE).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTamboresSobranteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamboresSobranteJugo Procedure 
PROCEDURE createTamboresSobranteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCantidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara       AS DECIMAL    NO-UNDO.  
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO. /*registra o no movimientos de stock*/

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt    AS INTEGER    NO-UNDO.

  FIND FIRST sobrante WHERE sobrante.id_empresa             = piEmpresa
                        AND sobrante.id_sucursal            = piSucursal
                        AND sobrante.id_tipotambor_sobrante = piTipoTambor
                        AND sobrante.nromov_sobrante        = piNroMov
                     NO-LOCK NO-ERROR.
  IF AVAILABLE sobrante THEN DO:  
    FIND FIRST lotes_jugo WHERE lotes_jugo.nromov = sobrante.nromov NO-LOCK NO-ERROR.
    IF lotes_jugo.id_articulo = 52 THEN
      iArt = 524.

    IF lotes_jugo.id_articulo = 53 THEN
      iArt = 534.
 
    RUN addDrums(piEmpresa, 
                 piSucursal, 
                 piTipoTambor, 
                 piNroMov, 
                 piCantidad, 
                 piDesde, 
                 piHasta, 
                 pdKilos, 
                 pdTara, 
                 sobrante.id_lote, 
                 iArt, 
                 sobrante.id_envase, 
                 lotes_jugo.id_calidad, 
                 lotes_jugo.anio, 
                 piEstado, 
                 sobrante.fecha,                  
                 TRUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteDrumsFromBatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDrumsFromBatch Procedure 
PROCEDURE deleteDrumsFromBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE iDesde AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno   AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    IF plRegMov THEN DO:
      FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa
                                      AND tambores_industria.id_sucursal   = piSucursal
                                      AND tambores_industria.id_tipotambor = piTipoTambor
                                      AND tambores_industria.nromov        = piNroMov
                                    NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN 
        iDesde = tambores_industria.id_tambor.

      FIND LAST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa
                                     AND tambores_industria.id_sucursal   = piSucursal
                                     AND tambores_industria.id_tipotambor = piTipoTambor
                                     AND tambores_industria.nromov        = piNroMov
                                    NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN 
        iHasta = tambores_industria.id_tambor.

      IF iDesde <> 0 AND iHasta <> 0 THEN DO:
        RUN y_gstkcre.p (piEmpresa,
                         piSucursal,
                         piTipoTambor,
                         piNroMov,
                         iDesde,
                         iHasta,
                         2) "lotes".
        
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
          RETURN "error".
          UNDO, LEAVE.
        END.
      END.
    END.

    RUN deleterProcesoTambor (piEmpresa,
                              piSucursal,
                              piTipoTambor,
                              piNromov).
    
    
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa
                                  AND tambores_industria.id_sucursal   = piSucursal
                                  AND tambores_industria.id_tipotambor = piTipoTambor
                                  AND tambores_industria.nromov        = piNroMov.
      DELETE tambores_industria.      
    END.
  END.

  
  /*registrar egreso en lotes_ubicacion */
  RUN updateLoteUbicacion (piEmpresa, 
                           piSucursal, 
                           piTipoTambor, 
                           piNroMov, 
                           piSucursal).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteDrumsFromLoteRango) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDrumsFromLoteRango Procedure 
PROCEDURE deleteDrumsFromLoteRango :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesde      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHasta      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov     AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE iDesde AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.

  ASSIGN iDesde = piDesde
         iHasta = piHasta.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    IF plRegMov THEN DO:
      IF iDesde <> 0 AND iHasta <> 0 THEN DO:
        RUN y_gstkcre.p (piEmpresa,
                         piSucursal,
                         piTipoTambor,
                         piNroMov,
                         iDesde,
                         iHasta,
                         2) "lotes_aceite".
        
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
          RETURN "error".
          UNDO, LEAVE.
        END.
      END.
    END.
    
    /*
    RUN deleterCargaTamborRango (piEmpresa,
                                 piSucursal,
                                 piTipotambor,
                                 piNromov,
                                 iDesde,
                                 iHasta).
    */

    FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa
                                  AND tambores_industria.id_sucursal   = piSucursal
                                  AND tambores_industria.id_tipotambor = piTipoTambor
                                  AND tambores_industria.nromov        = piNroMov
                                  AND tambores_industria.id_tambor    >= iDesde
                                  AND tambores_industria.id_tambor    <= iHasta.
      DELETE tambores_industria.      
    END.
  END.

  
  /*registrar ingreso en lotes_ubicacion OJO todavia no funciona*/
  RUN updateLoteUbicacion (piEmpresa, 
                           piSucursal, 
                           piTipoTambor, 
                           piNroMov, 
                           piSucursal).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteParamsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteParamsFile Procedure 
PROCEDURE deleteParamsFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.

  cFile = "..\industria\paramsfile.conf".

  OUTPUT TO VALUE(cFile).
  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deletePuntoEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deletePuntoEnvase Procedure 
PROCEDURE deletePuntoEnvase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  FOR EACH punto_envase WHERE punto_envase.id_empresa     = piEmp
                          AND punto_envase.id_sucursal    = piSuc
                          AND punto_envase.id_tipotambor  = piTip
                          AND punto_envase.nromov         = piNro.
    DELETE punto_envase.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRelTamborRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRelTamborRemito Procedure 
PROCEDURE deleteRelTamborRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piRem AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSRe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piMov AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piFac AS INTEGER    NO-UNDO.

  FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_empresa         = piEmp
                               AND r_tambor_remito.id_sucursal        = piSuc 
                               AND r_tambor_remito.id_tipotambor      = piTip 
                               AND r_tambor_remito.nromov             = piNro
                               AND r_tambor_remito.id_tambor          = piTam
                               AND r_tambor_remito.id_sucursal_remito = piSre
                               AND r_tambor_remito.id_tipo_movsto     = piMov
                               AND r_tambor_remito.nro_remito         = piRem
                               AND r_tambor_remito.ITEM_factura       = piFac
                             NO-ERROR.
  IF AVAILABLE r_tambor_remito THEN DO:
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
      DELETE r_tambor_remito.
    END.
 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRProcesoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRProcesoTambor Procedure 
PROCEDURE deleteRProcesoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FOR EACH r_proceso_tambor WHERE r_proceso_tambor.id_empresa    = piEmp
                                AND r_proceso_tambor.id_sucursal   = piSuc
                                AND r_proceso_tambor.id_tipotambor = piTip
                                AND r_proceso_tambor.nromov        = piNro.
      DELETE r_proceso_tambor.
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRProcesoTamborRango) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRProcesoTamborRango Procedure 
PROCEDURE deleteRProcesoTamborRango :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FOR EACH r_proceso_tambor WHERE r_proceso_tambor.id_empresa    = piEmp
                                AND r_proceso_tambor.id_sucursal   = piSuc
                                AND r_proceso_tambor.id_tipotambor = piTip
                                AND r_proceso_tambor.nromov        = piNro
                                AND r_proceso_tambor.id_tambor    >= piDes
                                AND r_proceso_tambor.id_tambor    <= piHas.
      DELETE r_proceso_tambor.
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteTamborCero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTamborCero Procedure 
PROCEDURE deleteTamborCero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tambores_industria WHERE tambores_industria.id_tambor = 0.
    DELETE tambores_industria.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delMovimientoCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delMovimientoCamara Procedure 
PROCEDURE delMovimientoCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambor     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucMovi    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTurno      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piMovimiento AS INTEGER    NO-UNDO.  
  DEFINE INPUT  PARAMETER piItem       AS INTEGER    NO-UNDO.

  DEFINE BUFFER buMov FOR r_mov_camara_tambor.
  
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FIND FIRST buMov WHERE buMov.id_empresa             = piEmpresa
                       AND buMov.id_sucursal            = piSucursal
                       AND buMov.id_tipotambor          = piTipoTambor
                       AND buMov.nromov                 = piNroMov
                       AND buMov.id_tambor              = piTambor
                       AND buMov.id_sucursal_movimiento = piSucMovi
                       AND buMov.id_turno               = piTurno
                       AND buMov.fecha                  = pdFecha
                       AND buMov.id_movimiento_camara   = piMovimiento
                       AND buMov.ITEM                   = piItem
                     NO-ERROR.
    IF AVAILABLE buMov THEN DO:
      DELETE buMov.

      /*borro la info de camara del tambor*/
      RUN setMovimientoCamaraTambor (piEmpresa, 
                                     piSucursal, 
                                     piTipoTambor, 
                                     piNroMov,
                                     piTambor,
                                     piEmpresa, 
                                     piSucMovi, 
                                     0, 
                                     "", 
                                     "").
    END.
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTTCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTCamara Procedure 
PROCEDURE fillTTCamara :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcFil AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttUbiCam.

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF piCam = 0 THEN DO: /*lleno la tt con los tambores recien salidos del envase*/
    FOR EACH tambores_industria WHERE tambores_industria.fecha                >= DATE("01/01/2005")
                                  AND tambores_industria.id_sucursal_ubicacion = piSuc
                                  AND tambores_industria.id_empresa_camara     = piEmp
                                  AND tambores_industria.id_sucursal_camara    = piSuc
                                  AND tambores_industria.id_camara             = 0
                                  AND tambores_industria.id_tipotambor         = 3  
                                NO-LOCK.
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = piSuc NO-LOCK NO-ERROR.
      FIND FIRST camara WHERE camara.id_empresa   = piEmp
                          AND camara.id_sucursal  = piSuc
                          AND camara.id_camara    = piCam
                        NO-LOCK NO-ERROR.

      
      CREATE ttUbiCam.
      ASSIGN ttUbiCam.id_sucursal_ubicacion  = tambores_industria.id_sucursal_ubicacion
             ttUbiCam.id_empresa             = tambores_industria.id_empresa
             ttUbiCam.id_sucursal            = tambores_industria.id_sucursal
             ttUbiCam.id_tipotambor          = tambores_industria.id_tipotambor
             ttUbiCam.nromov                 = tambores_industria.nromov
             ttUbiCam.id_tambor              = tambores_industria.id_tambor
             ttUbiCam.id_empresa_camara      = tambores_industria.id_empresa_camara
             ttUbiCam.id_sucursal_camara     = tambores_industria.id_sucursal_camara
             ttUbiCam.id_camara              = tambores_industria.id_camara
             ttUbiCam.nro_fila               = tambores_industria.nro_fila_camara
             ttUbiCam.nro_columna            = tambores_industria.nro_columna_camara
             ttUbiCam.ubicacion              = sucursales.abreviatura
             ttUbiCam.camara                 = IF AVAILABLE camara THEN camara.descripcion ELSE "SIN-CAMARA"
             ttUbiCam.id_lote                = tambores_industria.id_lote
             ttUbiCam.anio                   = tambores_industria.anio
             ttUbiCam.id_articulo            = tambores_industria.id_Articulo
             ttUbiCam.id_calidad             = tambores_industria.id_calidad
             ttUbiCam.id_envase              = tambores_industria.id_envase
             ttUbiCam.articulo               = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-PROD"
             ttUbiCam.calidad                = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-CALIDAD"
             ttUbiCam.envase                 = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NO-ENVASE".
        
             
    END.
  END.
  ELSE DO: /*lleno la tt con los tambores ubicados en la fila y columna y camara de parametros*/
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa_camara  = piEmp
                                  AND tambores_industria.id_sucursal_camara = piSuc
                                  AND tambores_industria.id_camara          = piCam
                                  AND tambores_industria.nro_fila_camara    = pcFil
                                  AND tambores_industria.nro_columna_camara = pcCol
                                NO-LOCK .
      /*tengo que excluir tambores que hayan sacado de la camara con movimientos de egreso*/
      FIND LAST r_mov_camara_tambor OF tambores_industria NO-LOCK NO-ERROR.
      FIND LAST items_movimiento_camara OF r_mov_camara_Tambor 
                                         WHERE items_movimiento_camara.id_tipo_movimiento <> 2
                                         NO-LOCK NO-ERROR.
      IF AVAILABLE items_movimiento_camara THEN 
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST sucursales WHERE sucursales.id_sucursal = piSuc NO-LOCK NO-ERROR.
        FIND FIRST camara WHERE camara.id_empresa   = piEmp
                            AND camara.id_sucursal  = piSuc
                            AND camara.id_camara    = piCam
                          NO-LOCK NO-ERROR.

        
        CREATE ttUbiCam.
        ASSIGN ttUbiCam.id_sucursal_ubicacion  = tambores_industria.id_sucursal_ubicacion
               ttUbiCam.id_empresa             = tambores_industria.id_empresa
               ttUbiCam.id_sucursal            = tambores_industria.id_sucursal
               ttUbiCam.id_tipotambor          = tambores_industria.id_tipotambor
               ttUbiCam.nromov                 = tambores_industria.nromov
               ttUbiCam.id_tambor              = tambores_industria.id_tambor
               ttUbiCam.id_empresa_camara      = tambores_industria.id_empresa_camara
               ttUbiCam.id_sucursal_camara     = tambores_industria.id_sucursal_camara
               ttUbiCam.id_camara              = tambores_industria.id_camara
               ttUbiCam.nro_fila               = tambores_industria.nro_fila_camara
               ttUbiCam.nro_columna            = tambores_industria.nro_columna_camara
               ttUbiCam.ubicacion              = sucursales.abreviatura
               ttUbiCam.camara                 = IF AVAILABLE camara THEN camara.descripcion ELSE "SIN-CAMARA"
               ttUbiCam.id_lote                = tambores_industria.id_lote
               ttUbiCam.anio                   = tambores_industria.anio
               ttUbiCam.id_articulo            = tambores_industria.id_Articulo
               ttUbiCam.id_calidad             = tambores_industria.id_calidad
               ttUbiCam.id_envase              = tambores_industria.id_envase
               ttUbiCam.articulo               = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-PROD"
               ttUbiCam.calidad                = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-CALIDAD"
               ttUbiCam.envase                 = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NO-ENVASE".
        

    END.
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mailingAsocTamboresOF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailingAsocTamboresOF Procedure 
PROCEDURE mailingAsocTamboresOF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcCon AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTi FOR tambores_industria.

  FOR EACH buTi WHERE buTi.id_empresa     = piEMp
                  AND buTi.id_sucursal    = piSuc
                  AND buTi.id_tipotambor  = piTip
                  AND buTi.nromov         = piNro
                  AND buTi.id_tambor     >= piDes
                  AND buTi.id_tambor     <= piHas
                NO-LOCK.
    i = i + 1.
    k = k + buTi.kilos_tambor.    

    iLot = buTi.id_lote.
    iAno = buTi.anio.
  END.

  cTo  = getUsuariosLista(103).
  cSub = "aviso de asociacion de tambores a contrato".
  cMes = "Se vincularon " + STRING(i) + " tambores por " + STRING(k) + " Kgs. del lote " + STRING(iLot) + "/" + STRING(iAno) + " al contrato " + pcCon.

  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mailingControlCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailingControlCalidad Procedure 
PROCEDURE mailingControlCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plSta AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEst AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buTi FOR tambores_industria.

  IF plSta THEN 
    cEst = "Aprobo".
  ELSE 
    cEst = "Desaprobo".

  FOR FIRST buTi WHERE buTi.id_empresa    = piEmp
                   AND buTi.id_sucursal   = piSuc
                   AND buTi.id_tipotambor = piTip
                   AND buTi.nromov        = piNro
                  NO-LOCK.
    FIND FIRST productos_terminados OF buTi NO-LOCK NO-ERROR.
    FIND FIRST calidades OF buTi NO-LOCK NO-ERROR.

    cMes = "El lote " + STRING(buTi.id_lote) + "/" + STRING(buTi.anio) + " " + productos_terminados.descripcion + " " + calidades.descripcion + " " + cEst + " el control de calidad".

  END.

  cTo  = getUsuariosLista(103).
  cSub = "aviso control de calidad".

  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mailingInsumos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailingInsumos Procedure 
PROCEDURE mailingInsumos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH mailing_insumos WHERE mailing_insumos.enviado = "NO".
    RUN ..\industria\sendMail.p("",                               /* SIEMPRE TIENE QUE IR */
                                2,                                /* PRIORIDAD */
                                mailing_insumos.subject,          /* SUBJECT */
                                mailing_insumos.body,             /* BODY     */
                                mailing_insumos.destinatario,     /* DEST. SEP COMAS */
                                "").                              /* ARCHIVOS ATTACHED SEP POR COMAS */

    ASSIGN mailing_insumos.enviado   = "SI"
           mailing_insumos.c_usuario = USERID("userdb")
           mailing_insumos.c_fecha   = TODAY
           mailing_insumos.c_hora    = STRING(TIME,"HH:MM:SS").
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mailingTamboresItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailingTamboresItemOE Procedure 
PROCEDURE mailingTamboresItemOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOrd AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTi FOR tambores_industria.

  FOR EACH buTi WHERE buTi.id_empresa     = piEMp
                  AND buTi.id_sucursal    = piSuc
                  AND buTi.id_tipotambor  = piTip
                  AND buTi.nromov         = piNro
                  AND buTi.id_tambor     >= piDes
                  AND buTi.id_tambor     <= piHas
                NO-LOCK.
    i = i + 1.
    k = k + buTi.kilos_tambor.    

    iLot = buTi.id_lote.
    iAno = buTi.anio.
  END.

  cTo  = getUsuariosLista(103).
  cSub = "aviso de asociacion de tambores a orden entrega".
  cMes = "Se vincularon " + STRING(i) + " tambores por " + STRING(k) + " Kgs. del lote " + STRING(iLot) + "/" + STRING(iAno) + " a la OE " + STRING(piOrd) + " Parte " + STRING(piPte).

  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-removeOrigenes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeOrigenes Procedure 
PROCEDURE removeOrigenes :
/*------------------------------------------------------------------------------
  Purpose:     borra la referencia de reproceso a los tambores que apuntan al lote parametro.
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTamOrig FOR tambores_industria.

  FOR EACH buTamOrig WHERE buTamOrig.id_empresa_destino    = piEmp
                       AND buTamOrig.id_sucursal_destino   = piSuc
                       AND buTamOrig.id_tipotambor_destino = piTip
                       AND buTamOrig.nromov_destino        = piNro.
    RUN setLoteDestinoToTambor (buTamOrig.id_empresa, 
                                buTamOrig.id_sucursal, 
                                buTamOrig.id_tipotambor,
                                buTamOrig.nromov,
                                buTamOrig.id_tambor,       
                                0,
                                0, 
                                0, 
                                0,
                                TRUE).    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveParamsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParamsFile Procedure 
PROCEDURE saveParamsFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cKey     AS CHARACTER  FORMAT "X(50)" NO-UNDO.
  DEFINE VARIABLE cFile    AS CHARACTER  NO-UNDO.

  cFile = "..\industria\paramsfile.conf".

  cKey = STRING(piEmp) + CHR(1) + 
         STRING(piSuc) + CHR(1) + 
         STRING(piTip) + CHR(1) + 
         STRING(piNro).    

  OUTPUT TO VALUE(cFile) .
    DISP cKey WITH 1 COL SIDE-LABELS.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setArticuloLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArticuloLote Procedure 
PROCEDURE setArticuloLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    ASSIGN tambores_industria.id_articulo = piArt. 
  END.

  RELEASE tambores_industria.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalidadLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCalidadLote Procedure 
PROCEDURE setCalidadLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    ASSIGN tambores_industria.id_calidad = piCal. 
  END.

  RELEASE tambores_industria.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCargaToTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCargaToTambor Procedure 
PROCEDURE setCargaToTambor :
/*------------------------------------------------------------------------------
  Purpose:     graba en el tambor la clave de la carga (reproceso)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prCarga AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piEmp   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE pcMsg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcDat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcPrg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcUsr AS CHARACTER  NO-UNDO.


  FIND FIRST cargas WHERE ROWID(cargas) = prCarga 
                    NO-LOCK NO-ERROR.
  IF AVAILABLE cargas THEN DO:
    RUN setDestinoToTambor(piEmp,
                           piSuc,                   /*tambores_seleccionados*/
                           piTip, 
                           piNro, 
                           cargas.id_empresa, 
                           cargas.id_sucursal,      /*carga a asociar*/
                           cargas.id_tipotambor, 
                           cargas.nromov, 
                           piTam, 
                           4,                      /*loc ubi 4 porque el tbor continua en stock hasta que se cierre el proceso*/ 
                           TRUE).                  /*registra o no movimientos*/
    IF RETURN-VALUE <> "" THEN DO:
      pcMsg = "Error al asignar carga a tambores".
      pcDat = "carga nromov:" + STRING(cargas.nromov) + " prod: " + STRING(piNro) + " tbor: " + STRING(piTam).
      pcPrg = "libTamboresIndustria.setCargaToTambor".
      pcUsr = USERID("userdb").
      RUN logIndustria.p (pcMsg, pcDat, pcPrg, pcUsr).
      MESSAGE "Error al procesar tambores" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClaveRemitoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setClaveRemitoTambor Procedure 
PROCEDURE setClaveRemitoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piRem AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piMov AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSre AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piFac AS INTEGER    NO-UNDO.

  FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                 AND tambores_industria.id_sucursal   = piSuc
                                 AND tambores_industria.id_tipotambor = piTip
                                 AND tambores_industria.nromov        = piNro
                                 AND tambores_industria.id_tambor     = piTam.

    ASSIGN tambores_industria.id_sucursal_remito  = piSre
           tambores_industria.nro_remito          = piRem
           tambores_industria.id_tipo_movsto      = piMov
           tambores_industria.ITEM_factura        = piFac.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setControlCalidadJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setControlCalidadJugo Procedure 
PROCEDURE setControlCalidadJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plSta AS LOGICAL    NO-UNDO.  

  /*cambio estado a tambores*/
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    IF plSta THEN DO:
      ASSIGN tambores_industria.fecha_reproceso = TODAY
             tambores_industria.id_estado       = 11 /*apto para despacho*/
             tambores_industria.c_usuario       = USERID("userdb")
             tambores_industria.c_fecha         = TODAY
             tambores_industria.c_hora          = STRING(TIME, "HH:MM:SS").
    END.
    ELSE DO:
      ASSIGN tambores_industria.id_contrato_of         = ""
             tambores_industria.id_tipocontrato_of     = 0
             tambores_industria.anio_of                = 0
             tambores_industria.item_of                = 0
             tambores_industria.id_orden_entrega       = 0
             tambores_industria.ITEM_oe                = 0
             tambores_industria.fecha_reproceso        = TODAY
             tambores_industria.id_estado              = 2 /*mp para reproceso*/
             tambores_industria.c_usuario              = USERID("userdb")
             tambores_industria.c_fecha                = TODAY
             tambores_industria.c_hora                 = STRING(TIME, "HH:MM:SS").
    END.
  END.

  /*mailing*/
  RUN mailingControlCalidad (piEmp, piSuc, piTip, piNro, plSta).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDestinoToTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDestinoToTambor Procedure 
PROCEDURE setDestinoToTambor :
/*------------------------------------------------------------------------------
  Purpose:     dado un lote origen, assigno los tambores al lote destino
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOriEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piLocUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRegMov AS LOGICAL    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
  
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piOriEmp
                                    AND tambores_industria.id_sucursal   = piOriSuc
                                    AND tambores_industria.id_tipotambor = piOriTip
                                    AND tambores_industria.nromov        = piOriNro
                                    AND tambores_industria.id_tambor     = piTambor
                                  NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      ASSIGN tambores_industria.id_empresa_destino    = piDesEmp
             tambores_industria.id_sucursal_destino   = piDesSuc
             tambores_industria.id_tipotambor_destino = piDesTip
             tambores_industria.nromov_destino        = piDesNro
             tambores_industria.id_locacion_ubicacion = piLocUbi
             tambores_industria.fecha_reproceso       = TODAY 
             tambores_industria.id_estado             = IF piDesTip = 10 THEN 10 ELSE 8.
  
      IF plRegMov THEN DO:      
        RUN y_gstkcre.p (tambores_industria.id_empresa,
                         tambores_industria.id_sucursal,
                         tambores_industria.id_tipotambor,
                         tambores_industria.nromov,
                         tambores_industria.id_tambor,
                         tambores_industria.id_tambor,
                         16) "tambores_industria".
                
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
          RETURN "ADM-ERROR".
        END. 
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEnvaseLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEnvaseLote Procedure 
PROCEDURE setEnvaseLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnv AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    ASSIGN tambores_industria.id_envase = piEnv. 
  END.

  RELEASE tambores_industria.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEstadoControlCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstadoControlCalidad Procedure 
PROCEDURE setEstadoControlCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plSta AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piQco AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iEstado   AS INTEGER    NO-UNDO.

  DEFINE BUFFER buJugo FOR lotes_jugo.
  
  IF plSta AND piTip = 3 THEN
    iEstado = 3.
  
  IF plSta AND piTip = 6 THEN
    iEstado = 5.
  
  IF NOT plSta THEN
    iEstado = 4.


  IF piTip = 3 THEN DO: /*lotes de jugo*/
    FIND FIRST buJugo WHERE buJugo.id_empresa    = piEmp
                            AND buJugo.id_sucursal   = piSuc
                            AND buJugo.id_tipotambor = piTip
                            AND buJugo.nromov        = piNro.
    IF AVAILABLE buJugo THEN DO:
      ASSIGN buJugo.estado_lote             = iEstado
             buJugo.CONTROL_calidad         = plSta
             buJugo.quimico_control_calidad = piQco
             buJugo.fecha_control_calidad   = TODAY
             buJugo.c_usuario               = USERID("userdb")
             buJugo.c_fecha                 = TODAY
             buJugo.c_hora                  = STRING(TIME, "HH:MM:SS").
    END.
  END.

  IF piTip = 6 THEN DO: /*lotes de aceite*/
    FIND FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmp
                              AND lotes_aceite.id_sucursal   = piSuc
                              AND lotes_aceite.id_tipotambor = piTip
                              AND lotes_aceite.nromov        = piNro.
    IF AVAILABLE lotes_aceite THEN DO:
      ASSIGN lotes_aceite.estado_lote             = iEstado
             lotes_aceite.CONTROL_calidad         = plSta
             lotes_aceite.quimico_control_calidad = getQuimicoName(piQco)
             lotes_aceite.c_usuario               = USERID("userdb")
             lotes_aceite.c_fecha                 = TODAY
             lotes_aceite.c_hora                  = STRING(TIME, "HH:MM:SS").
    END.
  END.

  /*cambio estado a tambores*/
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    IF plSta THEN DO:
      ASSIGN tambores_industria.fecha_reproceso = TODAY
             tambores_industria.id_estado       = 11 /*apto para despacho*/
             tambores_industria.c_usuario       = USERID("userdb")
             tambores_industria.c_fecha         = TODAY
             tambores_industria.c_hora          = STRING(TIME, "HH:MM:SS").
    END.
    ELSE DO:
      ASSIGN tambores_industria.id_contrato_of         = ""
             tambores_industria.id_tipocontrato_of     = 0
             tambores_industria.anio_of                = 0
             tambores_industria.item_of                = 0
             tambores_industria.id_orden_entrega       = 0
             tambores_industria.ITEM_oe                = 0
             tambores_industria.fecha_reproceso        = TODAY
             tambores_industria.id_estado              = 2 /*mp para reproceso*/
             tambores_industria.c_usuario              = USERID("userdb")
             tambores_industria.c_fecha                = TODAY
             tambores_industria.c_hora                 = STRING(TIME, "HH:MM:SS").
    END.
  END.

  /*mailing*/
  RUN mailingControlCalidad (piEmp, piSuc, piTip, piNro, plSta).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEstadoDevolucionTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstadoDevolucionTambor Procedure 
PROCEDURE setEstadoDevolucionTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDev AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAno AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piUbi AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTambores FOR tambores_industria.


  FOR FIRST buTambores 
      WHERE buTambores.id_empresa    = piEmp
        AND buTambores.id_sucursal   = piSuc
        AND buTambores.id_tipotambor = piTip
        AND buTambores.nromov        = piNro
        AND buTambores.id_tambor     = piTam.

    ASSIGN buTambores.id_devolucion         = piDev
           buTambores.anio_devolucion       = piAno       
           buTambores.id_sucursal_ubicacion = piUbi
           buTambores.c_usuario             = USERID("userdb")
           buTambores.c_fecha               = TODAY
           buTambores.c_hora                = STRING(TIME,"HH:MM:SS")
           .
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEstadoMicrobiologia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstadoMicrobiologia Procedure 
PROCEDURE setEstadoMicrobiologia :
DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plSta AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piQco AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iEstado   AS INTEGER    NO-UNDO.
  
  IF plSta THEN
    iEstado = 5. 
  
  IF NOT plSta THEN
    iEstado = 4.

  
  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmp
                          AND lotes_jugo.id_sucursal   = piSuc
                          AND lotes_jugo.id_tipotambor = piTip
                          AND lotes_jugo.nromov        = piNro.
  IF AVAILABLE lotes_jugo THEN DO:
    ASSIGN lotes_jugo.estado_lote             = iEstado
           lotes_jugo.microbiologia           = plSta        
           lotes_jugo.fecha_microbiologia     = TODAY
           lotes_jugo.quimico_microbiologia   = piQco
           lotes_jugo.c_usuario               = USERID("userdb")
           lotes_jugo.c_fecha                 = TODAY
           lotes_jugo.c_hora                  = STRING(TIME, "HH:MM:SS").
  END.

  
  /*cambio estado a tambores*/
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro.
    IF plSta THEN DO:
      ASSIGN tambores_industria.fecha_reproceso = TODAY
             tambores_industria.id_estado       = 4
             tambores_industria.c_usuario       = USERID("userdb")
             tambores_industria.c_fecha         = TODAY
             tambores_industria.c_hora          = STRING(TIME, "HH:MM:SS").
    END.
    ELSE DO:
      ASSIGN tambores_industria.id_contrato_of         = ""
             tambores_industria.id_tipocontrato_of     = 0
             tambores_industria.anio_of                = 0
             tambores_industria.item_of                = 0
             tambores_industria.id_orden_entrega       = 0
             tambores_industria.ITEM_oe                = 0
             tambores_industria.fecha_reproceso        = TODAY
             tambores_industria.id_estado              = 2 /*mp para reproceso*/
             tambores_industria.c_usuario              = USERID("userdb")
             tambores_industria.c_fecha                = TODAY
             tambores_industria.c_hora                 = STRING(TIME, "HH:MM:SS").
    END.
  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEstadoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstadoTambor Procedure 
PROCEDURE setEstadoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEst AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTambores FOR tambores_industria.

  DEFINE VARIABLE iLocUbi AS INTEGER    NO-UNDO.
  

  IF piEst = 8 OR /*cuando se cierra un lote*/
     piEst = 9 OR /*cuando se cierra un proceso*/
     piEst = 3 OR /*cuando se vuelca un tambor para rebatcheo en un lote*/
     piEst = 10   /*cuando se cierra un proceso*/
    THEN 
    iLocUbi = 10.
  ELSE
    iLocUbi = 4.

  FOR FIRST buTambores 
      WHERE buTambores.id_empresa    = piEmp
        AND buTambores.id_sucursal   = piSuc
        AND buTambores.id_tipotambor = piTip
        AND buTambores.nromov        = piNro
        AND buTambores.id_tambor     = piTam.

    ASSIGN buTambores.id_estado             = piEst
           buTambores.id_locacion_ubicacion = iLocUbi                      
           buTambores.c_usuario             = USERID("userdb")
           buTambores.c_fecha               = TODAY
           buTambores.c_hora                = STRING(TIME,"HH:MM:SS")
           .
    IF piEst = 2 THEN
      RUN setOEToTambor (piEmp, piSuc, piTip, piNro, piTam, 0, 0). /*si el tambor pasa a materia prima, borro vinculacioneas*/

  END.

  /*registrar en movimiento de stock con rutina de cacha?*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFechaAperturaLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFechaAperturaLote Procedure 
PROCEDURE setFechaAperturaLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha AS DATE       NO-UNDO.

  DEFINE BUFFER buLj FOR lotes_jugo.

  FIND FIRST buLj WHERE bulj.id_empresa    = piEmp 
                    AND bulj.id_sucursal   = piSuc
                    AND bulj.id_tipotambor = piTip
                    AND bulj.nromov        = piNro
                  NO-ERROR.
  IF AVAILABLE bulj AND bulj.fecha_comienzo = ? THEN DO:
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":    
      ASSIGN bulj.fecha_comienzo        = pdFecha
             bulj.fecha_comienzo_envase = pdFecha
             bulj.hora_comienzo         = STRING(TIME, "HH:MM:SS")
             bulj.hora_comienzo_envase  = STRING(TIME, "HH:MM:SS")
             .
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFechaCierreLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFechaCierreLote Procedure 
PROCEDURE setFechaCierreLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha   AS DATE       NO-UNDO.


  DEFINE BUFFER buLj FOR lotes_jugo.
  DEFINE BUFFER buTi FOR tambores_industria.

  FIND FIRST buLj WHERE bulj.id_empresa    = piEmp 
                    AND bulj.id_sucursal   = piSuc
                    AND bulj.id_tipotambor = piTip
                    AND bulj.nromov        = piNro
                  NO-ERROR.
  IF AVAILABLE bulj AND bulj.fecha_finalizacion = ? THEN DO:
    /*ctrl fecha_comienzo*/
    IF bulj.fecha_comienzo = ?  THEN DO:
      MESSAGE "Imposible cerrar un lote que no fue abierto" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    
    /*ctrl inspeccion final*/
    IF LOGICAL(getLoteTieneInspeccionFinal(piEmp, piSuc, piTip, piNro)) = FALSE THEN DO:
      MESSAGE "Imposible cerrar un lote que no tiene cargados los valores de Analisis de la Lectura Final" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.

    /*
    /*ctrl ingredientes*/
    IF LOGICAL(getLoteTieneIngredientes(piEmp, piSuc, piTip, piNro)) = FALSE THEN DO:
      MESSAGE "Imposible cerrar un lote que no tiene cargados los ingredientes" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      /*RETURN.*/
    END.
    
    /*ctrl insumos*/
    IF LOGICAL(getLoteTieneInsumos(piEmp, piSuc, piTip, piNro)) = FALSE THEN DO:
      MESSAGE "Imposible cerrar un lote que no tiene cargados los Insumos de Envases" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      /*RETURN.*/
    END.
    */

    /*ctrl volumen origenes*/
    IF NOT getControlVolumen(piEmp, piSuc, piTip, piNro) THEN DO:
      MESSAGE "Esta tratando de cerrar un lote para el cual se obtuvieron mas kilos de los que se rebatchearon" SKIP "Desea Continuar?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
      IF NOT lChoice THEN RETURN.
    END.





    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":    
      ASSIGN bulj.fecha_finalizacion        = pdFecha
             bulj.fecha_finalizacion_envase = pdFecha
             bulj.hora_finalizacion         = STRING(TIME, "HH:MM:SS")
             bulj.hora_fin_envase           = STRING(TIME, "HH:MM:SS").
      /*paso a lote terminado los tambores nuevos*/
      FOR EACH buTi WHERE buTi.id_empresa    = piEmp
                      AND buTi.id_sucursal   = piSuc
                      AND buTi.id_tipotambor = piTip
                      AND buTi.nromov        = piNro.
        RUN setEstadoTambor (buTi.id_empresa, 
                             buTi.id_sucursal, 
                             buTi.id_tipotambor, 
                             buTi.nromov, 
                             buTi.id_tambor, 
                             4). /*lote terminado*/
      END.
      /*paso al estado reprocesado los tambores marcados como origen de este lote*/
      FOR EACH buTi WHERE buTi.nromov_destino = piNro.
        RUN setEstadoTambor (buTi.id_empresa, 
                             buTi.id_sucursal, 
                             buTi.id_tipotambor, 
                             buTi.nromov, 
                             buTi.id_tambor, 
                             8). /*tambor reprocesado*/
        
      END.
    END.    
  END.

  /*programa de correccion de inconsistencias*/

  RUN pInconsistenciasStockLotesProcesos.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFechaReAperturaLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFechaReAperturaLote Procedure 
PROCEDURE setFechaReAperturaLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iEstado AS INTEGER    NO-UNDO.
  
  DEFINE BUFFER buLj FOR lotes_jugo.

  FIND FIRST buLj WHERE bulj.id_empresa    = piEmp 
                    AND bulj.id_sucursal   = piSuc
                    AND bulj.id_tipotambor = piTip
                    AND bulj.nromov        = piNro
                  NO-ERROR.
  IF AVAILABLE bulj THEN DO:
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR": 
      ASSIGN bulj.fecha_finalizacion        = ?
             bulj.fecha_finalizacion_envase = ?
             bulj.hora_finalizacion         = ?
             bulj.hora_fin_envase           = ?
             .
      /*cambio el estado de los tambores del lote*/
      FOR EACH tambores_industria OF buLj
          NO-LOCK.
        RUN setEstadoTambor (tambores_industria.id_empresa,
                             tambores_industria.id_sucursal,
                             tambores_industria.id_tipotambor,
                             tambores_industria.nromov,
                             tambores_industria.id_tambor,
                             7).
      END.

      /*cambio el estado de los tambores que se reprocesaron para formar el lote*/
      FOR EACH tambores_industria 
          WHERE tambores_industria.id_tipotambor_destino = buLJ.id_tipotambor
            AND tambores_industria.nromov_destino        = buLJ.nromov
          NO-LOCK.

        iEstado = 2. /*para sobrantes arrastres u otros*/
        IF tambores_industria.id_tipotambor = 1 THEN
          iEstado = 5.
        IF tambores_industria.id_tipotambor = 3 THEN
          iEstado = 4.

        RUN setEstadoTambor (tambores_industria.id_empresa,
                             tambores_industria.id_sucursal,
                             tambores_industria.id_tipotambor,
                             tambores_industria.nromov,
                             tambores_industria.id_tambor,
                             iEstado). 
        
      END.


    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKilosLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKilosLote Procedure 
PROCEDURE setKilosLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil AS DECIMAL    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro
                                AND tambores_industria.id_tambor    >= piDes
                                AND tambores_industria.id_tambor    <= piHas.
    ASSIGN tambores_industria.kilos_tambor = pdKil.    
  END.

  RELEASE tambores_industria.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoteDestinoToTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLoteDestinoToTambor Procedure 
PROCEDURE setLoteDestinoToTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOriEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriTbo AS INTEGER    NO-UNDO.

  DEFINE INPUT  PARAMETER piDesEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesNro AS INTEGER    NO-UNDO.
  
  DEFINE INPUT  PARAMETER plRegMov AS LOGICAL    NO-UNDO.

  
  DEFINE VARIABLE iLocUbi AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEstado AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFecRep AS DATE       NO-UNDO.

  IF piDesNro = 0 THEN  /*desvincula tambores*/
    ASSIGN iEstado = 2
           iLocUbi = 4
           dFecRep = ?
           .

  IF piDesTip = 6 THEN /*si el destino es un lote de aceite*/
    ASSIGN iEstado = 3
           iLocUbi = 10
           dFecRep = TODAY
           .

  IF piDesTip = 3 THEN /*si el destino es un lote de jugo*/
    ASSIGN iEstado = 3
           iLocUbi = 4
           dFecRep = TODAY
           .

  IF piDesTip = 10 THEN /*si el destino es una carga*/
    ASSIGN iEstado = 10
           iLocUbi = 4
           dFecRep = TODAY
           .


  IF piOriTip = 6 THEN 
    ASSIGN iEstado = 6.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piOriEmp
                                    AND tambores_industria.id_sucursal   = piOriSuc
                                    AND tambores_industria.id_tipotambor = piOriTip
                                    AND tambores_industria.nromov        = piOriNro
                                    AND tambores_industria.id_tambor     = piOriTbo
                                  NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      ASSIGN tambores_industria.id_empresa_destino    = piDesEmp
             tambores_industria.id_sucursal_destino   = piDesSuc
             tambores_industria.id_tipotambor_destino = piDesTip
             tambores_industria.nromov_destino        = piDesNro
             /*tambores_industria.id_sucursal_ubicacion = piDesSuc*/ /*confirmar*/
             tambores_industria.id_locacion_ubicacion = iLocUbi
             tambores_industria.id_estado             = iEstado
             tambores_industria.fecha_reproceso       = dFecRep. /*importante para listado de reprocesos*/

      IF plRegMov THEN DO:
        /*movimiento de stock sobre los tambores de la etapa anterior*/
        RUN y_gstkcre.p (piOriEmp,
                         piOriSuc,
                         piOriTip,
                         piOriNro,
                         piOriTbo,
                         piOriTbo,
                         16) "tambores_industria".
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
          RETURN "ADM-ERROR".
        END.
      END.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoteDestinoToTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLoteDestinoToTambores Procedure 
PROCEDURE setLoteDestinoToTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOriEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOriNro AS INTEGER    NO-UNDO.
  
  DEFINE INPUT  PARAMETER piDesEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDesNro AS INTEGER    NO-UNDO.
  
  DEFINE INPUT  PARAMETER plRegMov AS LOGICAL    NO-UNDO.

  
  DEFINE VARIABLE lFirst  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iDesde  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFecRep AS DATE       NO-UNDO.

  IF piDesNro = 0 THEN
    dFecRep = DATE("").
  ELSE 
    dFecRep = TODAY.
  

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":  
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa            = piOriEmp
                                  AND tambores_industria.id_sucursal           = piOriSuc
                                  AND tambores_industria.id_tipotambor         = piOriTip
                                  AND tambores_industria.nromov                = piOriNro
                                  AND tambores_industria.id_sucursal_ubicacion = piOriSuc
                                  AND tambores_industria.id_locacion_ubicacion = 4.
  
      IF NOT lFirst THEN DO:
        iDesde = tambores_industria.id_tambor.
        lFirst = TRUE.
      END.
      iHasta = tambores_industria.id_tambor.
      
      ASSIGN tambores_industria.id_empresa_destino    = piDesEmp
             tambores_industria.id_sucursal_destino   = piDesSuc
             tambores_industria.id_tipotambor_destino = piDesTip
             tambores_industria.nromov_destino        = piDesNro
             tambores_industria.id_sucursal_ubicacion = piDesSuc
             tambores_industria.id_locacion_ubicacion = 10
             tambores_industria.id_estado             = 8 /*reprocesado*/
             tambores_industria.fecha_reproceso       = dFecRep. /*importante para listado de reprocesos*/
      
    END.
  
    IF plRegMov THEN DO:
      /*movimiento de stock sobre los tambores de la etapa anterior*/
      RUN y_gstkcre.p (piOriEmp,
                       piOriSuc,
                       piOriTip,
                       piOriNro,
                       iDesde,
                       iHasta,
                       14) "lotes_aceite".
      IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "ADM-ERROR".
      END.
    END.
  END. /*transaction*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoteJugoToTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLoteJugoToTambor Procedure 
PROCEDURE setLoteJugoToTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prLote  AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piEmp   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE pcMsg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcDat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcPrg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcUsr AS CHARACTER  NO-UNDO.


  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = prLote
                    NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    RUN setDestinoToTambor(piEmp,
                           piSuc,                      /*tambores_seleccionados*/
                           piTip, 
                           piNro, 
                           lotes_jugo.id_empresa, 
                           lotes_jugo.id_sucursal,      /*lote a asociar*/
                           lotes_jugo.id_tipotambor, 
                           lotes_jugo.nromov, 
                           piTam, 
                           10, 
                           TRUE).                  /*registra o no movimientos*/
    IF RETURN-VALUE <> "" THEN DO:
      pcMsg = "Error al asignar lote a tambores".
      pcDat = "lote nromov:" + STRING(lotes_jugo.nromov) + " prod: " + STRING(piNro) + " tbor: " + STRING(piTam).
      pcPrg = "libTamboresIndustria.setLoteJugoToTambor".
      pcUsr = USERID("userdb").
      RUN logIndustria.p (pcMsg, pcDat, pcPrg, pcUsr).
      MESSAGE "Error al procesar tambores" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMovimientoCamaraTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMovimientoCamaraTambor Procedure 
PROCEDURE setMovimientoCamaraTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTbo AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEmpCam  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucCam  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCamara  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcFila    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcColumna AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  FIND FIRST buTam WHERE buTam.id_empresa     = piEmp
                     AND buTam.id_sucursal    = piSuc
                     AND buTam.id_tipotambor  = piTip
                     AND buTam.nromov         = piNro
                     AND buTam.id_tambor      = piTbo
                   NO-ERROR.
  IF AVAILABLE buTam THEN DO:
    ASSIGN buTam.id_empresa_camara  = piEmpCam
           buTam.id_sucursal_camara = piSucCam
           buTam.id_camara          = piCamara
           buTam.nro_fila_camara    = pcFila
           buTam.nro_columna_camara = pcColumna.
  END.
  
  RELEASE buTam.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNroPrecinto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNroPrecinto Procedure 
PROCEDURE setNroPrecinto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcPre AS CHARACTER  NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = piEmp
                                AND tambores_industria.id_sucursal    = piSuc
                                AND tambores_industria.id_tipotambor  = piTip
                                AND tambores_industria.nromov         = piNro
                                AND tambores_industria.id_tambor     >= piDes
                                AND tambores_industria.id_tambor     <= piHas.
    
    ASSIGN tambores_industria.nro_precinto     = pcPre.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOEToTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOEToTambor Procedure 
PROCEDURE setOEToTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOE  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piItm AS INTEGER    NO-UNDO.
  

  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fBrx AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.


  FOR FIRST tambores_industria
      WHERE tambores_industria.id_empresa     = piEmp
        AND tambores_industria.id_sucursal    = piSuc
        AND tambores_industria.id_tipotambor  = piTip
        AND tambores_industria.nromov         = piNro
        AND tambores_industria.id_tambor      = piTam.

    FIND FIRST items_orden_entrega
         WHERE items_orden_entrega.id_orden_entrega = piOE
           AND items_orden_entrega.ITEM_oe          = piItm
         NO-ERROR.

    IF AVAILABLE items_orden_entrega AND piOE <> 0 THEN DO: /*vincula oe y contrato*/

      IF piTip = 3 THEN DO:
        cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
        fBrx = IF cAnl <> "" THEN DECIMAL(ENTRY(4, cAnl, CHR(1))) ELSE 0.
      END.

      ASSIGN tambores_industria.id_orden_entrega   = piOE
             tambores_industria.ITEM_oe            = piItm
             tambores_industria.id_contrato_of     = items_orden_entrega.id_contrato
             tambores_industria.id_tipocontrato_of = items_orden_entrega.id_tipo_contrato
             tambores_industria.anio_of            = items_orden_entrega.anio
             tambores_industria.item_of            = items_orden_entrega.ITEM
             items_orden_entrega.grados_brix       = fBrx
             .

    END.
    
    ELSE  /*desvincula oe y contrato*/
      
      ASSIGN tambores_industria.id_orden_entrega   = 0
             tambores_industria.ITEM_oe            = 0
             /*tambores_industria.id_contrato_of     = ""
             tambores_industria.id_tipocontrato_of = 0
             tambores_industria.anio_of            = 0
             tambores_industria.item_of            = 0*/
             .


  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelProcesoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelProcesoTambor Procedure 
PROCEDURE setRelProcesoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroPro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEmp    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam    AS INTEGER    NO-UNDO.

  FIND FIRST r_proceso_tambor WHERE r_proceso_tambor.id_empresa_proceso     = piEmpPro
                                AND r_proceso_tambor.id_sucursal_proceso    = piSucPro
                                AND r_proceso_tambor.id_tipotambor_proceso  = piTipPro
                                AND r_proceso_tambor.nromov_proceso         = piNroPro
                                AND r_proceso_tambor.id_empresa             = piEmp
                                AND r_proceso_tambor.id_sucursal            = piSuc
                                AND r_proceso_tambor.id_tipotambor          = piTip
                                AND r_proceso_tambor.nromov                 = piNro
                                AND r_proceso_tambor.id_tambor              = piTam
                              NO-LOCK NO-ERROR.
  IF NOT AVAILABLE r_proceso_tambor THEN DO:
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
      CREATE r_proceso_tambor.
      ASSIGN r_proceso_tambor.id_empresa_proceso    = piEmpPro
             r_proceso_tambor.id_sucursal_proceso   = piSucPro
             r_proceso_tambor.id_tipotambor_proceso = piTipPro
             r_proceso_tambor.nromov_proceso        = piNroPro
             r_proceso_tambor.id_empresa            = piEmp
             r_proceso_tambor.id_sucursal           = piSuc
             r_proceso_tambor.id_tipotambor         = piTip
             r_proceso_tambor.nromov                = piNro
             r_proceso_tambor.id_tambor             = piTam
             r_proceso_tambor.c_usuario             = USERID("userdb")
             r_proceso_tambor.c_fecha               = TODAY
             r_proceso_tambor.c_hora                = STRING(TIME,"HH:MM:SS").
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelRemitoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelRemitoTambor Procedure 
PROCEDURE setRelRemitoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piRem AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSRe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piMov AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piFac AS INTEGER    NO-UNDO.

  FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_empresa         = piEmp
                               AND r_tambor_remito.id_sucursal        = piSuc 
                               AND r_tambor_remito.id_tipotambor      = piTip 
                               AND r_tambor_remito.nromov             = piNro
                               AND r_tambor_remito.id_tambor          = piTam
                               AND r_tambor_remito.id_sucursal_remito = piSre
                               AND r_tambor_remito.id_tipo_movsto     = piMov
                               AND r_tambor_remito.nro_remito         = piRem
                               AND r_tambor_remito.ITEM_factura       = piFac
                             NO-ERROR.
  IF NOT AVAILABLE r_tambor_remito THEN DO:
    /*DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":*/
      /*grabo la relacion tambor_remito*/
      CREATE r_tambor_remito.
      ASSIGN r_tambor_remito.id_sucursal_remito = piSRe
             r_tambor_remito.id_tipo_movsto     = piMov
             r_tambor_remito.nro_remito         = piRem
             r_tambor_remito.ITEM_factura       = piFac
             r_tambor_remito.id_empresa         = piEmp
             r_tambor_remito.id_sucursal        = piSuc
             r_tambor_remito.id_tipotambor      = piTip
             r_tambor_remito.nromov             = piNro
             r_tambor_remito.id_tambor          = piTam
             r_tambor_remito.fecha              = TODAY
             r_tambor_remito.c_usuario          = USERID("userdb")
             r_tambor_remito.c_fecha            = TODAY
             r_tambor_remito.c_hora             = STRING(TIME, "HH:MM:SS").
    /*END.*/ /*do transaction*/
 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTipoPrecinto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTipoPrecinto Procedure 
PROCEDURE setTipoPrecinto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTPr AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = piEmp
                                AND tambores_industria.id_sucursal    = piSuc
                                AND tambores_industria.id_tipotambor  = piTip
                                AND tambores_industria.nromov         = piNro
                                AND tambores_industria.id_tambor     >= piDes
                                AND tambores_industria.id_tambor     <= piHas.
    
    ASSIGN tambores_industria.id_tipo_precinto = piTPr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setValoresAnalisis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setValoresAnalisis Procedure 
PROCEDURE setValoresAnalisis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBx2 AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBxc AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdAcP AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdAcG AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdLit AS DECIMAL    NO-UNDO.


  IF piTip = 3 THEN DO:
    FIND FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa     = piEmp
                                   AND inspecciones_lote.id_sucursal    = piSuc
                                   AND inspecciones_lote.id_tipotambor  = piTip
                                   AND inspecciones_lote.nromov         = piNro
                                 NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN 
      ASSIGN inspecciones_lote.hora           = STRING(TIME)
             inspecciones_lote.final          = TRUE
             inspecciones_lote.bx_20_20       = pdBx2
             inspecciones_lote.bx_correg      = pdBxc
             inspecciones_lote.acidez_w_w     = pdAcP
             inspecciones_lote.acidez_w_v     = pdAcG
             inspecciones_lote.litros         = pdLit
             inspecciones_lote.ratio          = pdBx2 / pdAcP.
    ELSE DO:
      CREATE inspecciones_lote.
      ASSIGN inspecciones_lote.id_empresa     = piEmp
             inspecciones_lote.id_sucursal    = piSuc
             inspecciones_lote.id_tipotambor  = piTip
             inspecciones_lote.nromov         = piNro
             inspecciones_lote.id_inspeccion  = 1
             inspecciones_lote.hora           = STRING(TIME)
             inspecciones_lote.final          = TRUE
             inspecciones_lote.bx_20_20       = pdBx2
             inspecciones_lote.bx_correg      = pdBxc
             inspecciones_lote.acidez_w_v     = pdAcG
             inspecciones_lote.acidez_w_w     = pdAcP
             inspecciones_lote.litros         = pdLit
             inspecciones_lote.ratio          = pdBx2 / pdAcP.
    END.


  END.

  IF piTip = 1 THEN DO:
    FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa     = piEmp
                                 AND produccion_jugo.id_sucursal    = piSuc
                                 AND produccion_jugo.id_tipotambor  = piTip
                                 AND produccion_jugo.nromov         = piNro
                               NO-ERROR.
    IF AVAILABLE produccion_jugo THEN DO:
      ASSIGN produccion_jugo.bx_20_20   = pdBx2             
             produccion_jugo.bx_correg  = pdBxc
             produccion_jugo.acidez_w_v = pdAcG
             produccion_jugo.acidez_w_w = pdAcP
             produccion_jugo.litros     = pdLit
             produccion_jugo.ratio      = pdBx2 / pdAcP.
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferMoliendaExterna) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferMoliendaExterna Procedure 
PROCEDURE transferMoliendaExterna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibRem AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cNros   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil    AS DECIMAL    NO-UNDO.

  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibRem = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
  
  cNros = DYNAMIC-FUNCTION('getNextNroRemito' IN hLibRem, piSuc, 123).

  IF cNros <> ? THEN DO:
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
      ASSIGN iTip = 123
             iNro = INTEGER(ENTRY(1, cNros, CHR(1))).

      CREATE remitos.
      ASSIGN remitos.id_sucursal      = piSuc
             remitos.id_tipo_movsto   = iTip
             remitos.nro              = iNro
             remitos.id_operacion     = 311
             remitos.nro_comprobante  = "00" + ENTRY(3, cNros, CHR(1)) + STRING(INTEGER(ENTRY(2, cNros, CHR(1))), "99999999")
             remitos.id_lugdes        = 20
             remitos.mercado          = 0
             remitos.id_destino       = 201
             remitos.id_cliente       = 6256
             remitos.id_vapor         = 0
             remitos.id_proveedor     = 1684
             remitos.id_ciaseg        = 17
             remitos.fecha            = TODAY
             remitos.observaciones    = "rto generado automaticamente p/transferncia de litoral a famailla"
             remitos.c_usuario        = USERID("userdb")
             remitos.c_fecha          = TODAY
             remitos.c_hora           = STRING(TIME,"HH:MM:SS")
             .
  
      IF piTip <> 11 THEN DO:

        FOR FIRST tambores_industria
          WHERE tambores_industria.id_empresa     = piEmp
            AND tambores_industria.id_sucursal    = piSuc
            AND tambores_industria.id_tipotambor  = piTip
            AND tambores_industria.nromov         = piNro
          NO-LOCK.
          iDes = tambores_industria.id_tambor.
        END.
      

        FOR EACH tambores_industria
            WHERE tambores_industria.id_empresa     = piEmp
              AND tambores_industria.id_sucursal    = piSuc
              AND tambores_industria.id_tipotambor  = piTip
              AND tambores_industria.nromov         = piNro
            NO-LOCK.
    
          ASSIGN iArt = tambores_industria.id_articulo
                 iCal = tambores_industria.id_calidad
                 iEnv = tambores_industria.id_envase
                 iHas = tambores_industria.id_tambor
                 iLot = tambores_industria.id_lote
                 iAno = tambores_industria.anio
                 iCan = iCan + 1.
                 fKil = fKil + tambores_industria.kilos_tambor.
                 .
    
          /*grabo clave remito en tambor*/
          RUN setClaveRemitoTambor (piEmp, piSuc, piTip, piNro, tambores_industria.id_tambor, 
                                    iNro, iTip, piSuc, 1).
  
          /*relacion item_factura con tambor (necesaria para el stock a fecha)*/
          RUN setRelRemitoTambor (piEmp, piSuc, piTip, piNro, tambores_industria.id_tambor, 
                                  iNro, piSuc, iTip, 1).
    
          
        END. /*for each tambores_industria*/
      END. /*if piTip <> 11*/


  
      cLot = STRING(iLot, "9999") + "/" + SUBSTRING(STRING(iAno), 3, 2).
  
      CREATE items_factura.
      ASSIGN items_factura.id_sucursal    = piSuc
             items_factura.id_tipo_movsto = iTip
             items_factura.nro            = iNro
             items_factura.ITEM           = 1
             items_factura.id_tipotambor  = piTip
             items_factura.id_articulo    = iArt
             items_factura.id_calidad     = iCal
             items_factura.id_envase      = iEnv
             items_factura.desde_lote     = iDes 
             items_factura.hasta_lote     = iHas
             items_factura.cantidad       = iCan
             items_factura.peso           = fKil / iCan
             items_factura.nro_lote       = cLot
             items_factura.c_usuario      = USERID("userdb")
             items_factura.c_fecha        = TODAY
             items_factura.c_hora         = STRING(TIME,"HH:MM:SS")
             .

      /*recalcula peso remito*/
      RUN recalcKilosRemito IN hLibRem (piSuc, iTip, iNro).
  
      /*proceso remito*/                  
      RUN processRemito IN hLibRem (piSuc, iTip, iNro, FALSE).
    
    END. /*do transaction*/
  END. /*if cNros <> ?*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateLoteUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateLoteUbicacion Procedure 
PROCEDURE updateLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     le paso el lote, borro lotes_ubicacion, contabilizo 
               tambores_industria y creo en lotes_ubicacion
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piUbi AS INTEGER    NO-UNDO. /*useless*/

  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE BUFFER lotes_ubi FOR lotes_ubicacion.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR".
    /*crear en lotes_ubicacion*/  
    FOR EACH lotes_ubicacion WHERE lotes_ubicacion.id_empresa    = piEmp
                               AND lotes_ubicacion.id_sucursal   = piSuc
                               AND lotes_ubicacion.id_tipotambor = piTip
                               AND lotes_ubicacion.nromov        = piNro.
      DELETE lotes_ubicacion.      
    END.

    FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                BREAK BY tambores_industria.id_sucursal_ubicacion.
      iCount = iCount + 1.
      IF LAST-OF(tambores_industria.id_sucursal_ubicacion)  THEN DO:
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        /*crear lotes_ubicacion*/
        CREATE  lotes_ubi.
        ASSIGN  lotes_ubi.id_empresa            = piEmp
                lotes_ubi.id_sucursal           = piSuc
                lotes_ubi.id_tipotambor         = piTip
                lotes_ubi.nromov                = piNro
                lotes_ubi.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
                lotes_ubi.id_contrato           = tambores_industria.id_contrato_of
                lotes_ubi.id_tipo_contrato      = tambores_industria.id_tipocontrato_of
                lotes_ubi.anio_contrato         = tambores_industria.anio_of
                lotes_ubi.ITEM_contrato         = tambores_industria.ITEM_of
                lotes_ubi.id_lote               = tambores_industria.id_lote
                lotes_ubi.anio                  = tambores_industria.anio
                lotes_ubi.cantidad              = iCount
                lotes_ubi.id_articulo           = tambores_industria.id_articulo
                lotes_ubi.lote                  = STRING(tambores_industria.id_lote,"999999") + STRING(tambores_industria.anio,"9999")
                lotes_ubi.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                lotes_ubi.envase                = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE".
        iCount = 0.
                
      END.
    END.
    
  
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getAcidezGPL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAcidezGPL Procedure 
FUNCTION getAcidezGPL RETURNS DECIMAL
  (pdBx2020 AS DECIMAL, 
   pdAcPorc AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dPe  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCor AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.


  dCor = getCoefCorreccionAcidez(pdAcPorc).
  dRet = pdBx2020 + dCor.
  dPe  = getCoefPesoEspecifico(dRet).

  dRet = ROUND(pdAcPorc * dPe * 10, 2).

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAporteGplNoRepro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAporteGplNoRepro Procedure 
FUNCTION getAporteGplNoRepro RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  dada una carga devuelve la cantidad de litros de componentes que aportan gpl
            solidos solubles y kilos400 que no son tambores
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAux AS DECIMAL    NO-UNDO.
  
  FOR FIRST cargas WHERE cargas.id_empresa    = piEmp
                     AND cargas.id_sucursal   = piSuc
                     AND cargas.id_tipotambor = piTip
                     AND cargas.nromov        = piNro
                   NO-LOCK.
    /*componentes no tambores*/
    FOR EACH composicion_carga WHERE composicion_carga.nromov      = cargas.nromov 
                                 AND composicion_carga.bx_20_20   <> 0
                                 AND composicion_carga.acidez_w_w <> 0
                               NO-LOCK.
      dBrx = getBrixCorregido(composicion_carga.bx_20_20, composicion_carga.acidez_w_w).
      dAux = composicion_carga.cantidad.      
      dSol = dSol + getSolidosSolubles(dBrx, dAux).
      dKi4 = dKi4 + getKilos400Gpl(composicion_carga.bx_20_20, 
                                   composicion_carga.acidez_w_w,
                                   dAux, 
                                   FALSE).
      dLit = dLit + dAux.

    END.
  END.


  RETURN STRING(dLit) + CHR(1) + STRING(dSol) + CHR(1) + STRING(dKi4).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAporteGplRepro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAporteGplRepro Procedure 
FUNCTION getAporteGplRepro RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  dada una carga devuelve los litros de tambores que se volcaron en la carga
    Notes:  y los solidos solubles que se volcaron
            y los kilos base 400gpl
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAux AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLib.

  FOR FIRST cargas WHERE cargas.id_empresa    = piEmp
                     AND cargas.id_sucursal   = piSuc
                     AND cargas.id_tipotambor = piTip
                     AND cargas.nromov        = piNro
                   NO-LOCK.

    /*tambores reprocesados*/
    FOR EACH tambores_industria WHERE tambores_industria.nromov_destino = cargas.nromov
                                BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        
        cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa, 
                                                              tambores_industria.id_sucursal,
                                                              tambores_industria.id_tipotambor,
                                                              tambores_industria.nromov).
        IF cAnl <> "" AND cAnl <> ? THEN DO:
          dAux = (k / getCoefPesoEspecifico(DECIMAL(ENTRY(4, cAnl, CHR(1))))).
          dSol = dSol + getSolidosSolubles(DECIMAL(ENTRY(4, cAnl, CHR(1))), dAux).
          dLit = dLit + dAux.
          dKi4 = dKi4 + getKilos400Gpl(DECIMAL(ENTRY(3, cAnl, CHR(1))), 
                                       DECIMAL(ENTRY(2, cAnl, CHR(1))),
                                       dAux, 
                                       FALSE).
          IF tambores_industria.id_articulo = 42 THEN DO: /*jugo dulce*/
            dLit = k.
            dKi4 = k.
          END.

        END.
        i    = 0.
        k    = 0.
        dAux = 0.
      END.
    END.
  END.

  RETURN STRING(dLit) + CHR(1) + STRING(dSol) + CHR(1) + STRING(dKi4) .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getArticulosMPJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulosMPJugo Procedure 
FUNCTION getArticulosMPJugo RETURNS CHARACTER
  (INPUT prRow    AS ROWID, 
   INPUT pcFuente AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF pcFuente = "carga" THEN DO:
    FIND FIRST cargas WHERE ROWID(carga) = prRow
                          NO-LOCK NO-ERROR.
    IF AVAILABLE cargas THEN DO:
      FOR EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_lote = cargas.id_articulo
                                      NO-LOCK.
        cRet = cRet + STRING(origenes_materia_prima.id_articulo_mp) + ",".
        
      END.
      cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
    END.
  END.

  IF pcFuente = "jugo" THEN DO:
    FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = prRow
                          NO-LOCK NO-ERROR.
    IF AVAILABLE lotes_jugo THEN DO:
      FOR EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_lote = lotes_jugo.id_articulo
                                      NO-LOCK.
        cRet = cRet + STRING(origenes_materia_prima.id_articulo_mp) + ",".
        
      END.
      cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
    END.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrix2020) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrix2020 Procedure 
FUNCTION getBrix2020 RETURNS DECIMAL
  (pdRatio AS DECIMAL, 
   pdAcPor AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN pdRatio * pdAcPor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrixCorregido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrixCorregido Procedure 
FUNCTION getBrixCorregido RETURNS DECIMAL
  (pdBx2020 AS DECIMAL, 
   pdAcidez AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  dRet = pdBx2020 + getCoefCorreccionAcidez(pdAcidez).
  
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantEnvases) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantEnvases Procedure 
FUNCTION getCantEnvases RETURNS CHARACTER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buLj FOR lotes_jugo.

  FIND FIRST buLj WHERE buLj.id_empresa    = piEmp
                    AND buLj.id_sucursal   = piSuc
                    AND buLj.id_tipotambor = piTip
                    AND buLj.nromov        = piNro
                  NO-LOCK NO-ERROR.
  IF AVAILABLE buLj THEN 
    cRet = STRING(buLj.cantidad_envases_nuevo) + "," + STRING(buLj.cantidad_tambores_recup).
  ELSE
    cRet = "0,0".


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadTambores Procedure 
FUNCTION getCantidadTambores RETURNS CHARACTER
  (pdKilos  AS DECIMAL, 
   piEnvase AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la cantidad de tambores segun lo kilos del envase parametro y los kilos del ultimo tambor
    Notes:  ej 25 + chr(1) + 250 + chr(10)
               1  + chr(1) + 180
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCoc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRto AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRto AS INTEGER    NO-UNDO.

  FOR FIRST r_kilos_envase WHERE r_kilos_envase.id_envase = piEnvase 
                           NO-LOCK.
    cKil = STRING(r_kilos_envase.kilos_envase).
    cCoc = STRING(TRUNCATE(pdKilos / r_kilos_envase.kilos_envase, 0)).
    cRto = STRING(pdKilos MODULO r_kilos_envase.kilos_envase).
  END.

  IF DECIMAL(cRto) > 0 THEN
    iRto = 1.
  ELSE 
    iRto = 0.

  cRet = cCoc + CHR(1) + cKil + CHR(10)
       + STRING(iRto)  + CHR(1) + cRto.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getClaveTamborFromRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClaveTamborFromRowid Procedure 
FUNCTION getClaveTamborFromRowid RETURNS CHARACTER
  (prTambor AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buTam FOR tambores_industria.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND FIRST buTam WHERE ROWID(buTam) = prTambor
                   NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN
    cRet = STRING(buTam.id_empresa)    + "," +
           STRING(buTam.id_sucursal)   + "," +
           STRING(buTam.id_tipotambor) + "," + 
           STRING(buTam.nromov)        + "," +
           STRING(buTam.id_tambor).
  ELSE 
    cRet = "0,0,0,0,0".


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefConversion400) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoefConversion400 Procedure 
FUNCTION getCoefConversion400 RETURNS DECIMAL
  (piTipoTambor AS INTEGER,
   piArticulo   AS INTEGER,
   piCalidad    AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = piArticulo
                                   AND r_productos_calidad.id_calidad  = piCalidad
                                 NO-LOCK NO-ERROR. 
  IF AVAILABLE r_productos_calidad THEN DO:
    dRet = r_productos_calidad.coeficiente.
  END.
  ELSE DO: /*esto esta feo, lo hago asi segun la prochi*/

    IF piTipoTambor = 2 OR
       piTipoTambor = 6 OR
       piTipoTambor = 8 THEN /*aceite*/
      dRet = 1.

    IF piArticulo = 712 AND piCalidad = 0 THEN
      dRet = 1.
    
    IF piArticulo = 64 AND piCalidad = 0 THEN 
      dRet = 1.
    
    IF piArticulo = 66 AND piCalidad = 0 THEN 
      dRet = 1.
    
    IF piCalidad = 602 THEN /*aceite*/
      dRet = 1.
    
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefCorreccionAcidez) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoefCorreccionAcidez Procedure 
FUNCTION getCoefCorreccionAcidez RETURNS DECIMAL
  (pdAcidez AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dCorr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dX1 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dX2 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY1 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY2 AS DECIMAL    NO-UNDO.

  FOR LAST correccion_acidez WHERE correccion_acidez.acidez <= ROUND(pdAcidez, 1) BY correccion_acidez.acidez.
    dX1 = correccion_acidez.acidez.
    dY1 = correccion_acidez.correccion.    
  END.
  FIND NEXT correccion_acidez NO-LOCK NO-ERROR.
  IF AVAILABLE correccion_acidez THEN DO:  
    dX2 = correccion_acidez.acidez.
    dY2 = correccion_acidez.correccion.    
  END.
  ELSE DO:
    dX2 = dX1.
    dY2 = dY1.    
  END.

  dCorr = getInterpolacion(pdAcidez, dX1, dY1, dX2, dY2).

  RETURN dCorr.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefPesoEspecifico) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoefPesoEspecifico Procedure 
FUNCTION getCoefPesoEspecifico RETURNS DECIMAL
  (pdBrix AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dX1  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dX2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY1  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.


  FOR LAST brix WHERE brix.brix <= ROUND(pdBrix, 1) BY brix.brix.
    dX1 = brix.brix.
    dY1 = brix.pe.    
  END.
  FIND NEXT brix NO-LOCK NO-ERROR.
  IF AVAILABLE brix THEN DO:
    dX2 = brix.brix.
    dY2 = brix.pe.
  END.
  ELSE DO:
    dX2 = dX1.
    dY2 = dY1.
  END.
  

  dRet = getInterpolacion(pdBrix, dX1, dY1, dX2, dY2).
  

  RETURN dRet.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefSolidosSolubles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoefSolidosSolubles Procedure 
FUNCTION getCoefSolidosSolubles RETURNS DECIMAL
  (pdBrix AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dX1 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dX2 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY1 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dY2 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FOR LAST brix WHERE brix.brix <= ROUND(pdBrix, 2) BY brix.brix.
    dX1 = brix.brix.
    dY1 = brix.solido_soluble.
  END.
  
  FIND NEXT brix NO-LOCK NO-ERROR.
  IF AVAILABLE brix THEN DO:
    ASSIGN dX2 = brix.brix.
           dY2 = brix.solido_soluble.
  END.
  ELSE DO:
    ASSIGN dX2 = dX1
           dY2 = dY1.
  END.
  
  dRet = getInterpolacion(pdBrix, dX1, dY1, dX2, dY2).
    
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposeNroLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getComposeNroLote Procedure 
FUNCTION getComposeNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER,
   piLot AS INTEGER,
   piAno AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = STRING(getFliaProducto(piArt), "999")  + "." + 
         STRING(piSuc, "999")                   + "." + 
         STRING(piArt, "999")                   + "." + 
         STRING(piLot, "9999")                  + "/" + 
         SUBSTRING(STRING(piAno), 3, 2).


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getComposicionCarga Procedure 
FUNCTION getComposicionCarga RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  cada linea de la cadena entry tiene Articulo chr(1) cantidad chr(1) color chr(1) chr(10)
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cCga AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.  
  DEFINE VARIABLE cPer AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCol AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE o    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE p    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLCg AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


  RUN libCommonFunctions.p PERSISTENT SET hLib.
  DEFINE BUFFER buRep FOR tambores_industria.

  FOR FIRST cargas WHERE cargas.id_empresa    = piEmp
                     AND cargas.id_sucursal   = piSuc 
                     AND cargas.id_tipotambor = piTip
                     AND cargas.nromov        = piNro 
                   NO-LOCK.
    dLCg = cargas.litros.
  END.


  FOR EACH composicion_carga WHERE composicion_carga.id_empresa     = piEmp
                               AND composicion_carga.id_sucursal    = piSuc
                               AND composicion_carga.id_tipotambor  = piTip
                               AND composicion_carga.nromov         = piNro
                             NO-LOCK.
    FOR FIRST productos_terminados WHERE productos_terminados.id_articulo = composicion_carga.id_articulo NO-LOCK.
      cArt = productos_terminados.descripcion.
    END.
    iCol = DYNAMIC-FUNCTION('getProductColor' IN hLib, composicion_carga.id_articulo).
    cCga = cCga + cArt + CHR(1) + STRING(composicion_carga.cantidad, ">>>>>9.99") + CHR(1) + STRING(iCol)+ CHR(10).
    dTot = dTot + composicion_carga.cantidad.
  END.

  /*tambores que se reprocesaron en esta carga*/
  FOR EACH buRep WHERE buRep.id_empresa_destino     = piEmp
                   AND buRep.id_sucursal_destino    = piSuc
                   AND buRep.id_tipotambor_destino  = piTip
                   AND buRep.nromov_destino         = piNro
                 NO-LOCK.      
    q = getLitrosTambor(buRep.id_empresa,
                        buRep.id_sucursal,
                        buRep.id_tipotambor,
                        buRep.nromov,
                        buRep.kilos_tambor).
    k = ROUND(k + q, 2).
  END.
  
  /*agrego a la lista los litros de jugo de tambores que se reprocesaron en la carga*/
  cCga = cCga + "Tbs. Reprocesados" + CHR(1) + STRING(ROUND(k, 2)) + CHR(1) + "12".
  dTot = dTot +  k.

  /*agrego una linea con el faltante para completar los litros de la carga*/
  IF dLCg > dTot THEN DO:    
    cCga = cCga +  CHR(10) + "Lts. Faltante" + CHR(1) + STRING(ROUND(dLCg - dTot, 2)) + CHR(1) + "13".
  END.

 /*primero recorro la lista para calcular el porcentual sobre la carga de cada componentes*/
  DO j = 1 TO NUM-ENTRIES(cCga, CHR(10)):
    cRow = TRIM(ENTRY(j, cCga, CHR(10))).
    cLeg = TRIM(ENTRY(1, cRow, CHR(1))).
    cVal = TRIM(ENTRY(2, cRow, CHR(1))).
    p    = DECIMAL(cVal).
    q    = ROUND((p * 100) / dTot, 2).
    cRet = cRet + cLeg + CHR(1) + STRING(p) + CHR(1) + STRING(q) + CHR(1) + ENTRY(3, cRow, CHR(1)) + CHR(10).
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionProceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getComposicionProceso Procedure 
FUNCTION getComposicionProceso RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVal AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPor AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.

  FOR EACH cargas WHERE cargas.id_empresa_proceso     = piEmp
                    AND cargas.id_sucursal_proceso    = piSuc
                    AND cargas.id_tipotambor_proceso  = piTip
                    AND cargas.nromov_proceso         = piNro
                  NO-LOCK.
    dSol = getSolidosSolubles(cargas.bx_correg, DECIMAL(cargas.litros)).
    cRow = cRow + "Carga "  + STRING(cargas.id_carga) + CHR(1) + 
                  getDescArticulo(cargas.id_articulo) + CHR(1) + 
                  STRING(dSol)                        + CHR(10).    
    dTot = dTot + dSol.
  END.

  /*calculo porcentaje para ponerlo en la leyenda*/
  DO i = 1 TO NUM-ENTRIES(cRow, CHR(10)) - 1:
    cAux = ENTRY(i, cRow, CHR(10)).
    dVal = DECIMAL(ENTRY(3, cAux, CHR(1))).
    dPor = dVal * 100 / dTot.
    dPor = ROUND(dPor, 2).
    cRet = cRet + cAux + CHR(1) + STRING(dPor) + CHR(10). 
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComposicionTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getComposicionTambor Procedure 
FUNCTION getComposicionTambor RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   piTbo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE l    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCga AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPer AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buRel FOR r_carga_tambor.

  /*tambores que surgieron a partir de esta carga*/

  FOR FIRST r_carga_tambor WHERE r_carga_tambor.id_empresa    = piEmp
                             AND r_carga_tambor.id_sucursal   = piSuc
                             AND r_carga_tambor.id_tipotambor = piTip
                             AND r_carga_tambor.nromov        = piNro
                             AND r_carga_tambor.id_tambor     = piTbo
                           NO-LOCK.
    cCga = getComposicionCarga(r_carga_tambor.id_empresa_carga,
                               r_carga_tambor.id_sucursal_carga,
                               r_carga_tambor.id_tipotambor_carga,
                               r_carga_tambor.nromov_carga).    
    
  END.

  FOR EACH buRel WHERE buRel.id_empresa     = piEmp
                   AND buRel.id_sucursal    = piSuc
                   AND buRel.id_tipotambor  = piTip
                   AND buRel.nromov         = piNro
                 NO-LOCK.
    FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = buRel.id_empresa
                                   AND tambores_industria.id_sucursal   = buRel.id_sucursal
                                   AND tambores_industria.id_tipotambor = buRel.id_tipotambor
                                   AND tambores_industria.nromov        = buRel.nromov
                                   AND tambores_industria.id_tambor     = buRel.id_tambor
                                  NO-LOCK.
      l = l + getLitrosTambor(tambores_industria.id_empresa,
                              tambores_industria.id_sucursal,
                              tambores_industria.id_tipotambor,
                              tambores_industria.nromov,
                              tambores_industria.kilos_tambor).
    END.
    i = i + 1.
    
  END.


  DO j = 1 TO NUM-ENTRIES(cCga, CHR(10)):
    cRow = ENTRY(j, cCga, CHR(10)).
    cLeg = ENTRY(1, cRow, CHR(1)).
    cVal = ENTRY(2, cRow, CHR(1)).
    cPer = ENTRY(3, cRow, CHR(1)).

    dLit = ROUND((l * DECIMAL(cPer) / 100) / i, 2).
    cRet = cRet + cLeg + CHR(1) + STRING(dLit) + CHR(1) + cPer + chr(1) + cVal + CHR(10).
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlCalidadLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlCalidadLote Procedure 
FUNCTION getControlCalidadLote RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  CASE piTip:
    WHEN 3 THEN DO:
      /*jugo*/
      FOR FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmp
                             AND lotes_jugo.id_sucursal   = piSuc
                             AND lotes_jugo.id_tipotambor = piTip 
                             AND lotes_jugo.nromov        = piNro
                           NO-LOCK.
        IF lotes_jugo.CONTROL_calidad AND lotes_jugo.microbiologia THEN
          lRet = TRUE.
      END.
    END.

    /*aceite*/
    WHEN 6 THEN DO:
      FOR FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmp
                               AND lotes_aceite.id_sucursal   = piSuc
                               AND lotes_aceite.id_tipotambor = piTip 
                               AND lotes_aceite.nromov        = piNro
                             NO-LOCK.
      IF lotes_aceite.CONTROL_calidad THEN
        lRet = TRUE.
      
      END.
    END.

    /*foldeado*/
    WHEN 7 THEN DO:
      FOR FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmp
                               AND lotes_aceite.id_sucursal   = piSuc
                               AND lotes_aceite.id_tipotambor = piTip 
                               AND lotes_aceite.nromov        = piNro
                             NO-LOCK.
      IF lotes_aceite.CONTROL_calidad THEN
        lRet = TRUE.
      
      END.
    END.
    /*
    /*cascara*/
    WHEN 11 THEN DO:
       no se realiza un control de calidad sobre los lotes de cascara, o 
         no se lo registra en el sistema
      FOR FIRST lotes_cascara WHERE lotes_cascara.id_empresa    = piEmp
                                AND lotes_cascara.id_sucursal   = piSuc
                                AND lotes_cascara.id_tipotambor = piTip 
                                AND lotes_cascara.nromov        = piNro
                              NO-LOCK.
      IF lotes_cascara.CONTROL_calidad THEN
        lRet = TRUE.
      END.
    END.
    */

    OTHERWISE
      lRet = TRUE.
      
  END CASE.



  RETURN lRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlTamborLeido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlTamborLeido Procedure 
FUNCTION getControlTamborLeido RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   pcAct AS CHARACTER,    /*accion*/
   pcArg AS CHARACTER) :  /*parametros adicionales*/
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlVolumen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlVolumen Procedure 
FUNCTION getControlVolumen RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iKilLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iKilRep AS INTEGER    NO-UNDO.


  /*esto por ahora, es valido solo para claro*/
  FOR FIRST tambores_industria
      WHERE tambores_industria.id_empresa    = piEmp
        AND tambores_industria.id_sucursal   = piSuc
        AND tambores_industria.id_tipotambor = piTip
        AND tambores_industria.nromov        = piNro
      NO-LOCK.
    IF tambores_industria.id_articulo <> 53 THEN RETURN TRUE.         
  END.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa    = piEmp
        AND tambores_industria.id_sucursal   = piSuc
        AND tambores_industria.id_tipotambor = piTip
        AND tambores_industria.nromov        = piNro
      NO-LOCK.
    iKilLot = iKilLot + tambores_industria.kilos_tambor.
  END.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa_destino    = piEmp
        AND tambores_industria.id_sucursal_destino   = piSuc
        AND tambores_industria.id_tipotambor_destino = piTip
        AND tambores_industria.nromov_destino        = piNro
      NO-LOCK.
    iKilRep = iKilRep + tambores_industria.kilos_tambor.
  END.

  IF iKilRep > iKilLot THEN
    lRet = TRUE.
  ELSE
    lRet = FALSE.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCumplimentoContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCumplimentoContrato Procedure 
FUNCTION getCumplimentoContrato RETURNS CHARACTER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPed AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCom AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dpCo AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dpDe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dpFa AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH items_contratos WHERE items_contratos.id_contrato = pcCon
                           NO-LOCK.
    iPed = iPed + items_contratos.cantidad.
  END.

  FOR EACH tambores_industria WHERE tambores_industria.id_contrato        = pcCon
                                AND tambores_industria.id_tipocontrato_of = piTip
                                AND tambores_industria.anio_of            = piAno
                                AND tambores_industria.id_orden_entrega  <> 0
                              NO-LOCK.
    iDes = iDes + 1.                                
  END.

  FOR EACH tambores_industria WHERE tambores_industria.id_contrato        = pcCon
                                AND tambores_industria.id_tipocontrato_of = piTip
                                AND tambores_industria.anio_of            = piAno
                                AND tambores_industria.id_orden_entrega   = 0
                              NO-LOCK.
    iCom = iCom + 1.                                
  END.

  iFal = iPed - (iDes + iCom).


  IF iPed > (iCom + iDes) THEN /*faltan tambores para cumplir el contrato*/

  IF iPed = (iCom + iDes) AND iCom > 0 THEN /*el contrato esta cumplido pero todavia no se despacharon todos los tbs*/

  IF iPed = iDes THEN /*contrato completamente cumplido*/

  IF iPed < (iCom + iDes) THEN /*el contrato todavia no tiene partes creadas*/


  dpDe = (iDes * 100) / iPed.
  dpCo = (iCom * 100) / iPed.
  dpFa = (iFal * 100) / iPed.

  DEFINE VARIABLE dPor AS DECIMAL    NO-UNDO.
  dPor = iDes * 100.
  dPor = dPor / iPed.


  cRet = "Tambores Faltantes"     + CHR(1) + STRING(iFal) + CHR(1) + STRING(dpFa) + CHR(1) + "Falt" + CHR(10) +
         "Tambores Despachados"   + CHR(1) + STRING(iDes) + CHR(1) + STRING(dPor) + CHR(1) + "Desp" + CHR(10) +         
         "Tambores Comprometidos" + CHR(1) + STRING(iCom) + CHR(1) + STRING(dpCo) + CHR(1) + "Comp".
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosTambor Procedure 
FUNCTION getDatosTambor RETURNS CHARACTER
  (prTambor AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve un string tipo entry con datos principales del tambor
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND FIRST tambores_industria WHERE ROWID(tambores_industria) = prTambor
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.

    cRet = STRING(tambores_industria.id_empresa)    + CHR(1) + 
           STRING(tambores_industria.id_sucursal)   + CHR(1) + 
           STRING(tambores_industria.id_tipotambor) + CHR(1) + 
           STRING(tambores_industria.nromov)        + CHR(1) + 
           STRING(tambores_industria.id_tambor)     + CHR(1) + 
           STRING(tambores_industria.id_lote)       + CHR(1) + 
           STRING(tambores_industria.anio)          + CHR(1) + 
           productos_terminados.descripcion         + CHR(1) + 
           calidades.descripcion                    + CHR(1) + 
           envases_prod.descripcion                 + CHR(1) + 
           STRING(tambores_industria.id_articulo)   + CHR(1) + 
           STRING(tambores_industria.id_calidad)    + CHR(1) + 
           STRING(tambores_industria.id_envase)     + CHR(1) + 
           STRING(tambores_industria.kilos_tambor)  + CHR(1) + 
           STRING(tambores_industria.tara).

  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescArticulo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescArticulo Procedure 
FUNCTION getDescArticulo RETURNS CHARACTER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = piArticulo
                                  NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN
    RETURN productos_terminados.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescCalidad Procedure 
FUNCTION getDescCalidad RETURNS CHARACTER
  (piCalidad AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND FIRST calidades WHERE calidades.id_calidad = piCalidad
                       NO-LOCK NO-ERROR.
  IF AVAILABLE calidades THEN
    RETURN calidades.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescEnvase Procedure 
FUNCTION getDescEnvase RETURNS CHARACTER
  (piEnvase AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST envases_prod WHERE envases_prod.id_envase = piEnvase
                          NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescSucursal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescSucursal Procedure 
FUNCTION getDescSucursal RETURNS CHARACTER
  (piSucursal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST sucursales WHERE sucursales.id_sucursal = piSucursal
                       NO-LOCK.
    cRet =  sucursales.abreviatura.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescTipoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescTipoTambor Procedure 
FUNCTION getDescTipoTambor RETURNS CHARACTER
  (piTipoTambor AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  CASE piTipoTambor:
    WHEN 1 THEN
      RETURN "Produccion Jugo".
    WHEN 2 THEN
      RETURN "Produccion Aceite".
    WHEN 3 THEN 
      RETURN "Lotes Jugo".
    WHEN 4 THEN 
      RETURN "Sobrante Jugo".
    WHEN 5 THEN
      RETURN "Arrastre Jugo".
    WHEN 6 THEN
      RETURN "Lotes Aceite".
    WHEN 7 THEN
      RETURN "Foldeado".
    WHEN 8 THEN
      RETURN "Sobrante Aceite".
    WHEN 9 THEN
      RETURN "Prod. Terceros".
    WHEN 10 THEN
      RETURN "Cargas".
    WHEN 11 THEN
      RETURN "Lotes Cascara".
    WHEN 12 THEN
      RETURN "Produccion Cascara".
  END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFactorInterpolacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFactorInterpolacion Procedure 
FUNCTION getFactorInterpolacion RETURNS DECIMAL
  (pdMax AS DECIMAL,
   pdMin AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDiff AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dFact AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSum  AS DECIMAL    NO-UNDO.

  dDiff = pdMax - pdMin.
  dSum  = pdMax + pdMin.
  dFact = (dDiff * 100) / dSum.

  RETURN dFact.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFliaProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFliaProducto Procedure 
FUNCTION getFliaProducto RETURNS INTEGER
  (piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve el rubro al cual pertenece el articulo
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPos AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cBus AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOil AS CHARACTER  NO-UNDO. /*aceite*/
  DEFINE VARIABLE cTur AS CHARACTER  NO-UNDO. /*jugo turbio*/
  DEFINE VARIABLE cCla AS CHARACTER  NO-UNDO. /*jugo claro*/
  DEFINE VARIABLE cCas AS CHARACTER  NO-UNDO. /*cascara*/
  DEFINE VARIABLE cPul AS CHARACTER  NO-UNDO. /*pulpa limon*/
  DEFINE VARIABLE cNar AS CHARACTER  NO-UNDO. /*jugo naranja*/
  DEFINE VARIABLE cOiP AS CHARACTER  NO-UNDO. /*oil phase limon*/
  DEFINE VARIABLE cWPh AS CHARACTER  NO-UNDO. /*water phase limon*/
  DEFINE VARIABLE cTer AS CHARACTER  NO-UNDO. /*terpenos*/
  DEFINE VARIABLE cANa AS CHARACTER  NO-UNDO. /*aceites dulces, naranja, mandarina, pomelo, lima*/


  cOil = "27,29,37,50,51,59,72,75,76,93,98,101,501,509,510,511,512,513,514,515,516,517,518,519,520,602,762,763,". 
  cTur = "33,52,66,88,521,523,524,523,524,600,630,802,882,952,962,966,".
  cCla = "34,35,53,63,601,532,533,534,535,953,".
  cCas = "38,39,40,54,55,69,554,954,".
  cPul = "30,70,71,711,971,".
  cNar = "42,43,46,49,62,63,70,401,424,621,624,712,942,946,".
  cOiP = "57,90,571,577,578,".
  cWPh = "58,581,582,587,588,".
  cTer = "74,741,742,743,744,745,".
  cANa = "31,32,47,48,41,44,45,61,64,65,67,68,73,96,97,411,441,471,481,502,503,504,508,590,610,611,670,671,681,682,731,961,".
 

  cBus = STRING(piArt) + ",".
  iRet = piArt.

  IF INDEX(cOil, cBus) > 0 THEN
    iRet = 51.

  IF INDEX(cTur, cBus) > 0 THEN
    iRet = 52.

  IF INDEX(cCla, cBus) > 0 THEN
    iRet = 53.

  IF INDEX(cCas, cBus) > 0 THEN
    iRet = 54.

  IF INDEX(cPul, cBus) > 0 THEN
    iRet = 71.

  IF INDEX(cOip, cBus) > 0 THEN
    iRet = 57.

  IF INDEX(cNar, cBus) > 0 THEN
    iRet = 401.

  IF INDEX(cWPh, cBus) > 0 THEN
    iRet = 58.

  IF INDEX(cTer, cBus) > 0 THEN
    iRet = 74.

  IF INDEX(cANa, cBus) > 0 THEN
    iRet = 56.

 
  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalonesLote Procedure 
FUNCTION getGalonesLote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, '..\industria\libReportes.p').
  DELETE OBJECT hLibCom.


  cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa    = piEmp
        AND tambores_industria.id_sucursal   = piSuc
        AND tambores_industria.id_tipotambor = piTip
        AND tambores_industria.nromov        = piNro.

    fKil = fKil + tambores_industria.kilos_tambor.
  END.


  IF cAnl <> ? AND cAnl <> "" AND cAnl <> "Aceite" THEN
    fBrx = DECIMAL(ENTRY(4, cAnl, CHR(1))).
  ELSE
    RETURN 0.00.

  fLit = fKil / getCoefPesoEspecifico(fBrx).

  fRet = fLit / 3.785.

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInterpolacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInterpolacion Procedure 
FUNCTION getInterpolacion RETURNS DECIMAL
  (pdX  AS DECIMAL,
   pdX1 AS DECIMAL,
   pdY1 AS DECIMAL,
   pdX2 AS DECIMAL,
   pdY2 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  utiliza un metodo de interpolacion lineal de la forma  
    Notes:  f(x) = y2 - [(y2 - y1) * (x2 - x) / (x2 - x1)]
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFx AS DECIMAL    NO-UNDO.

  IF (pdY2 <> pdY1) AND (pdX1 <> pdX2) THEN 
    dFx = pdY2 - ((pdY2 - pdY1) * (pdX2 - pdX) / (pdX2 - pdX1)).
  ELSE
    dFx = pdY2.
  
  RETURN dFx.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400 Procedure 
FUNCTION getKilos400 RETURNS DECIMAL
  (piTipoTambor AS INTEGER, 
   piArticulo   AS INTEGER, 
   piCalidad    AS INTEGER, 
   pdKilos      AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKilos AS DECIMAL    NO-UNDO.

  FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = piArticulo
                                   AND r_productos_calidad.id_calidad  = piCalidad
                                 NO-LOCK NO-ERROR. 
  IF AVAILABLE r_productos_calidad THEN DO:
    dKilos = pdKilos * r_productos_calidad.coeficiente.
  END.
  ELSE DO: /*esto esta feo, lo hago asi segun la prochi*/

    IF piTipoTambor = 2 OR
       piTipoTambor = 6 OR
       piTipoTambor = 8 THEN /*aceite*/
      dKilos = pdKilos.

    IF piArticulo = 712 AND piCalidad = 0 THEN
      dKilos = pdKilos.
    
    IF piArticulo = 64 AND piCalidad = 0 THEN 
      dKilos = pdKilos.
    
    IF piArticulo = 66 AND piCalidad = 0 THEN 
      dKilos = pdKilos.
    
    IF piCalidad = 602 THEN /*aceite*/
      dKilos = pdKilos.
    
  END.

  RETURN dKilos.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400Gpl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400Gpl Procedure 
FUNCTION getKilos400Gpl RETURNS DECIMAL
  (pdBx2020 AS DECIMAL,
   pdAcPorc AS DECIMAL,
   pdLitros AS DECIMAL, 
   plShowDt AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve los kgs en base 400 gpl en funcion de los valores de analisis
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRat AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGpl AS DECIMAL    NO-UNDO.
  
  DEFINE VARIABLE dAc4  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBx20 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPe4  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBxc4 AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE cOp   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMs   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl  AS CHARACTER FORMAT "x(75)" NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dRet  AS DECIMAL    NO-UNDO.

  crlf = CHR(13) + CHR(10).

  IF pdBx2020 = 0 AND pdAcPorc = 0 THEN DO:
    dRet = 0.
    /*dRet = pdLitros???*/
    RETURN dRet.
  END.

  dRat = pdBx2020 / pdAcPorc.  

  dAc4  = 31.00.              
  DO WHILE (dGpl <= 399.99):    
    dBx20 = dAc4 * dRat.
    dGpl  = getAcidezGPL(dBx20, dAc4).
    dAc4  = dAc4 + 0.01.
  END.
  dAc4 = dAc4 - 0.01.

  IF dBx20 > 89.90 THEN DO: /*significa que no esta en tabla - generalmente es un jugo dulce - directamente devuelvo los kilos sin conversion a 400*/
    dBxc4 = getBrixCorregido(pdBx2020, pdAcPorc).
    dRet  = getKilosFromBxCorreg(dBxc4, pdLitros).
    RETURN dRet.
  END.

  dBxc4 = dBx20 + getCoefCorreccionAcidez(dAc4).
  dPe4  = getCoefPesoEspecifico(dBxc4).

  dRet  = (pdLitros * getCoefSolidosSolubles(pdBx2020)) / getCoefSolidosSolubles(dBx20) .
  dRet  = dRet * dPe4.
  
  cOp = "[(" + STRING(pdLitros) + " * getCoefSolidosSolubles(" + STRING(pdBx2020) + ")) / getCoefSolidosSolubles("+ STRING(dBx20) + ") ] * getCoefPesoEspecifico(" + STRING(dBxc4) + "".
  cMs = "[(" + STRING(pdLitros) + " * " + STRING(getCoefSolidosSolubles(pdBx2020)) + ") / " + STRING(getCoefSolidosSolubles(dBx20)) + " ] * " + STRING(getCoefPesoEspecifico(dBxc4)) + "".

  
  /*debug mode trucho*/
  IF plShowDt THEN  DO:
    /*guardo en un archivo los valores obtenidos*/

    cFile = "..\industria\analisis.conf".

    cAnl = "bx2020400=" + STRING(ROUND(dBx20,2)) + crlf + 
           "ac%400=" + STRING(ROUND(dAc4,2)) + crlf + 
           "coefss2020=" + STRING(ROUND(getCoefSolidosSolubles(pdBx2020), 2)) + crlf + 
           "coefss2020400=" + STRING(ROUND(getCoefSolidosSolubles(dBx20), 2)) + crlf + 
           "bxcorreg400=" + STRING(ROUND(dBxc4,2)) + crlf + 
           "coefpespbxcorreg400=" + STRING(ROUND(getCoefPesoEspecifico(dBxc4), 2)) + crlf.
  
    OUTPUT TO VALUE(cFile) .
      PUT UNFORMATTED cAnl.
    OUTPUT CLOSE.
                                
    /*
    MESSAGE "K400: " + string(dRet)  SKIP 
            "Bx2020 400: " + string(dBx20) SKIP 
            "BcCorr 400: " + string(dBxc4) SKIP 
            "Ac % 400: " + string(dAc4)  SKIP SKIP
            getCoefSolidosSolubles(dBx20) SKIP 
            cOp SKIP 
            cMs VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400GplCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400GplCarga Procedure 
FUNCTION getKilos400GplCarga RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLib.
  

  FOR FIRST cargas WHERE cargas.id_empresa    = piEmp
                     AND cargas.id_sucursal   = piSuc
                     AND cargas.id_tipotambor = piTip
                     AND cargas.nromov        = piNro
                   NO-LOCK.
    
    /*componentes no tambores*/
    FOR EACH composicion_carga WHERE composicion_carga.nromov      = cargas.nromov 
                                 AND composicion_carga.bx_20_20   <> 0
                                 AND composicion_carga.acidez_w_w <> 0
                               NO-LOCK.
      
      dKi4 = dKi4 + getKilos400Gpl(composicion_carga.bx_20_20,
                                   composicion_carga.acidez_w_w, 
                                   composicion_carga.cantidad, 
                                   FALSE).
    END.
    

    /*tambores reprocesados*/
    FOR EACH tambores_industria WHERE tambores_industria.nromov_destino = cargas.nromov
                                BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa, 
                                                              tambores_industria.id_sucursal,
                                                              tambores_industria.id_tipotambor,
                                                              tambores_industria.nromov).
        IF cAnl <> "" AND cAnl <> ? THEN DO:
          dLit = k / getCoefPesoEspecifico(DECIMAL(ENTRY(4, cAnl, CHR(1)))).
        END.
  
        dKi4 = dKi4 + getKilos400Gpl(DECIMAL(ENTRY(3, cAnl, CHR(1))),
                                     DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                     dLit, 
                                     FALSE).

        i = 0.
        k = 0.
      END.
      
    END.

  END.

  RETURN dKi4.

END FUNCTION.


/*
/*tambores reprocesados*/
    FOR EACH tambores_industria WHERE tambores_industria.nromov_destino = cargas.nromov
                                NO-LOCK.
      cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa, 
                                                            tambores_industria.id_sucursal,
                                                            tambores_industria.id_tipotambor,
                                                            tambores_industria.nromov).
      IF cAnl <> "" AND cAnl <> ? THEN DO:
        dLit = tambores_industria.kilos_tambor / getCoefPesoEspecifico(DECIMAL(ENTRY(4, cAnl, CHR(1)))).
      END.

      dKi4 = dKi4 + getKilos400Gpl(DECIMAL(ENTRY(3, cAnl, CHR(1))),
                                   DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                   dLit, 
                                   FALSE).
    END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400GplTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400GplTambor Procedure 
FUNCTION getKilos400GplTambor RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   piTam AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLib.
  cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).

  IF cAnl <> "" AND cAnl <> ? THEN DO:
    FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                   AND tambores_industria.id_sucursal   = piSuc
                                   AND tambores_industria.id_tipotambor = piTip
                                   AND tambores_industria.nromov        = piNro
                                   AND tambores_industria.id_tambor     = piTam
                                 NO-LOCK.
      dKil = tambores_industria.kilos_tambor.
    END.
    dLit = dKil / getCoefPesoEspecifico(DECIMAL(ENTRY(4, cAnl, CHR(1)))).
    dKi4 = getKilos400GPL(DECIMAL(ENTRY(3, cAnl, CHR(1))),
                          DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                          dLit, 
                          FALSE).

    
  END.
  
  RETURN dKi4.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilos400Lote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400Lote Procedure 
FUNCTION getKilos400Lote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.

  FOR FIRST tambores_industria
      WHERE tambores_industria.id_empresa    = piEmp
        AND tambores_industria.id_sucursal   = piSuc
        AND tambores_industria.id_tipotambor = piTip
        AND tambores_industria.nromov        = piNro
      NO-LOCK.
    ASSIGN iArt = tambores_industria.id_articulo
           iCal = tambores_industria.id_calidad.
  END.



  fKil = getKilosLote(piEmp, piSuc, piTip, piNro).

  fRet = getKilos400(piTip, iArt, iCal, fKil).


  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosFromAcidez) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosFromAcidez Procedure 
FUNCTION getKilosFromAcidez RETURNS DECIMAL
  (pdAcidez AS DECIMAL,
   pdBx2020 AS DECIMAL,
   pdLitros AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  fomrmula Kgs = litros * peso_especifico(bx_corregido)  
            formula bx_correg = bx_20_20 + correccion(acidez)
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dCorr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrix AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPeso AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet  AS DECIMAL    NO-UNDO.
  

  IF pdAcidez <> 0 AND pdBx2020 <> 0 THEN DO:  
    dCorr = getCoefCorreccionAcidez(pdAcidez).
    dBrix = pdBx2020 + dCorr.
  END.

  dPeso = getCoefPesoEspecifico(dBrix).
  dRet  = pdLitros * dPeso.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosFromBxCorreg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosFromBxCorreg Procedure 
FUNCTION getKilosFromBxCorreg RETURNS DECIMAL
  (pdBxc AS DECIMAL, 
   pdLit AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dPeso AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet  AS DECIMAL    NO-UNDO.

  dPeso = getCoefPesoEspecifico(pdBxc).
  dRet  = pdLit * dPeso.  

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosLote Procedure 
FUNCTION getKilosLote RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa    = piEmp
        AND tambores_industria.id_sucursal   = piSuc
        AND tambores_industria.id_tipotambor = piTip
        AND tambores_industria.nromov        = piNro.

    fKil = fKil + tambores_industria.kilos_tambor.
  END.

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLitrosTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLitrosTambor Procedure 
FUNCTION getLitrosTambor RETURNS DECIMAL
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER,   
   pdKil AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAnal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dBrix AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.
  
  
  cAnal = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
  IF cAnal = "Aceite" THEN RETURN 0.00.

  dBrix = DECIMAL(ENTRY(4, cAnal, CHR(1))).
  IF dBrix <> 0 THEN
    dLit  = pdKil / getCoefPesoEspecifico(dBrix).
  ELSE
    dLit = 0.

  
  RETURN dLit.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteExistente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteExistente Procedure 
FUNCTION getLoteExistente RETURNS LOGICAL
  (piSuc AS INTEGER, 
   piTip AS INTEGER,
   piLot AS INTEGER,
   piAno AS INTEGER,
   piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buTam FOR tambores_industria. 

  FIND FIRST buTam WHERE buTam.id_lote        = piLot
                     AND buTam.anio           = piAno
                     AND buTam.id_articulo    = piArt
                     /*AND buTam.id_tipotambor  = piTip*/
                     /*AND buTam.id_sucursal    = piSuc*/ /*pedido por roque 02/12/2005*/
                   NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN 
    RETURN TRUE.
  ELSE 
    RETURN FALSE.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneIngredientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneIngredientes Procedure 
FUNCTION getLoteTieneIngredientes RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve si verdadero si algun tambor de este lote tiene remitos, falso si no los tiene  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST ingredientes_lote WHERE ingredientes_lote.id_empresa    = piEmp
                                 AND ingredientes_lote.id_sucursal   = piSuc
                                 AND ingredientes_lote.id_tipotambor = piTip
                                 AND ingredientes_lote.nromov        = piNro
                               NO-LOCK NO-ERROR.
  IF AVAILABLE ingredientes_lote THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneInspeccionFinal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneInspeccionFinal Procedure 
FUNCTION getLoteTieneInspeccionFinal RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve si verdadero si algun tambor de este lote tiene remitos, falso si no los tiene  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa    = piEmp
                                 AND inspecciones_lote.id_sucursal   = piSuc
                                 AND inspecciones_lote.id_tipotambor = piTip
                                 AND inspecciones_lote.nromov        = piNro
                                 AND inspecciones_lote.final         = TRUE
                               NO-LOCK NO-ERROR.
  IF AVAILABLE inspecciones_lote THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.
      
    
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneInsumos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneInsumos Procedure 
FUNCTION getLoteTieneInsumos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve si verdadero si algun tambor de este lote tiene remitos, falso si no los tiene  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST insumos_lote WHERE insumos_lote.id_empresa    = piEmp
                            AND insumos_lote.id_sucursal   = piSuc
                            AND insumos_lote.id_tipotambor = piTip
                            AND insumos_lote.nromov        = piNro
                          NO-LOCK NO-ERROR.
  IF AVAILABLE insumos_lote THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneOE Procedure 
FUNCTION getLoteTieneOE RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa     = piEmp
                                  AND tambores_industria.id_sucursal    = piSuc
                                  AND tambores_industria.id_tipotambor  = piTip
                                  AND tambores_industria.nromov         = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneRemitos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneRemitos Procedure 
FUNCTION getLoteTieneRemitos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve si verdadero si algun tambor de este lote tiene remitos, falso si no los tiene  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_empresa     = piEmp
                               AND r_tambor_remito.id_sucursal    = piSuc
                               AND r_tambor_remito.id_tipotambor  = piTip
                               AND r_tambor_remito.nromov         = piNro
                             NO-LOCK NO-ERROR.
  IF AVAILABLE r_tambor_remito THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteTieneReprocesos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteTieneReprocesos Procedure 
FUNCTION getLoteTieneReprocesos RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa      = piEmp
                                  AND tambores_industria.id_sucursal     = piSuc
                                  AND tambores_industria.id_tipotambor   = piTip
                                  AND tambores_industria.nromov          = piNro
                                  AND tambores_industria.nromov_destino <> 0
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroLote Procedure 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER, 
   piAno AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrd AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.


  FOR EACH tambores_industria 
      WHERE tambores_industria.id_sucursal = piSuc
        AND tambores_industria.id_articulo = piArt
        AND tambores_industria.anio        = piAno
      BY tambores_industria.id_lote DESC.
    iLot = tambores_industria.id_lote.
    LEAVE.
  END.

  iLot = iLot + 1.

  

  ASSIGN cPrd = STRING(getFliaProducto(piArt), "999")
         cSuc = STRING(piSuc, "999")
         cArt = STRING(piArt, "999")
         cLot = STRING(iLot, "9999")
         cAno = SUBSTRING(STRING(piAno), 3, 2)
         . 

  
  cRet = cPrd + "." + 
         cSuc + "." +
         cArt + "." + 
         cLot + "/" + 
         cAno.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroProcesoAceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroProcesoAceite Procedure 
FUNCTION getNextNroProcesoAceite RETURNS INTEGER
  (piSuc AS INTEGER,
   piArt AS INTEGER,
   piAno AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPro AS INTEGER    NO-UNDO.

  FOR EACH lotes_ind
      WHERE lotes_ind.id_sucursal = piSuc
  /*      AND lotes_ind.id_articulo = piArt*/
        AND lotes_ind.anio        = piAno
  /*    BY lotes_ind.id_proceso DESC*/.
  /*  iPro = lotes_ind.id_proceso.*/
    LEAVE.
  END.

  iPro = iPro + 1.

  RETURN iPro.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrigenesCargados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrigenesCargados Procedure 
FUNCTION getOrigenesCargados RETURNS LOGICAL
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE BUFFER buTi FOR tambores_industria.

  FIND FIRST buTi WHERE buTi.id_empresa_destino    = piEmp
                    AND buTi.id_sucursal_destino   = piSuc
                    AND buTi.id_tipotambor_destino = piTip
                    AND buTi.nromov_destino        = piNro
                  NO-LOCK NO-ERROR.
  IF AVAILABLE buTi THEN 
    RETURN TRUE.
  ELSE 
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParamsFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParamsFile Procedure 
FUNCTION getParamsFile RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro  AS INTEGER    NO-UNDO.

  cFile = "..\industria\paramsfile.conf".
  
  INPUT FROM VALUE(cFile) NO-ECHO .
  REPEAT :
    IMPORT UNFORMATTED cLine.
    IF LENGTH(cLine) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    iPos = INDEX(cLine, "#").
    IF iPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.

    cKey = ENTRY(2, cLine, ":").
  END.
  OUTPUT CLOSE.

  RETURN cKey.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPerdidaClarificado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPerdidaClarificado Procedure 
FUNCTION getPerdidaClarificado RETURNS CHARACTER
  (pdDesde AS DATE,
   pdHasta AS DATE, 
   piSuc   AS INTEGER, 
   plRecal AS LOGICAL) : /*true recalcula, false asume que antes de esta funcion se llenaron las tablas perdida*/
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dBx2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcp  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSS1  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSS2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi1  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dK41  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dK42  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBxc  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBx3  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAc3  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLi3  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iCnt  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dlmt  AS CHARACTER  NO-UNDO.

  dlmt = CHR(1).

  IF plRecal THEN DO:  
    RUN libReportes.p PERSISTENT SET hLib.  
    RUN callReportePerdida IN hLib (pdDesde,
                                    pdHasta,
                                    piSuc, 
                                    FALSE).
  END.

  FOR EACH perdida_cargas NO-LOCK.    
    ASSIGN iCnt = iCnt + 1
           dBx2 = dBx2 + perdida_cargas.brix_20_20
           dAcp = dAcp + perdida_cargas.acidez_p_p
           dLit = dLit + perdida_cargas.litros
           dKi1 = dKi1 + perdida_cargas.kilos
           dK41 = dK41 + perdida_cargas.kilos_400
           dSS1 = dSS1 + perdida_cargas.solidos.
  END.



  ASSIGN dBx2 = dBx2 / iCnt
         dAcp = dAcp / iCnt
         dBxc = getBrixCorregido(dBx2, dAcp)
         /*dSS1 = getSolidosSolubles(dBxc, dLit)*/
         /*dKi1 = getKilosFromAcidez(dAcp, dBx2, dLit)*/
         /*dK41 = getKilos400Gpl(dBx2, dAcp, dLit, FALSE)*/.
         

  iCnt = 0.
  FOR EACH perdida_pto_env NO-LOCK.
    dSS2 = dSS2 + perdida_pto_env.solidos.    
    dKi2 = dKi2 + perdida_pto_env.kilos.
    dK42 = dK42 + perdida_pto_env.kilos_400.
    dBx3 = dBx3 + perdida_pto_env.brix_20_20.
    dAc3 = dAc3 + perdida_pto_env.acidez_p_p.
    dLi3 = dLi3 + perdida_pto_env.litros.
    iCnt = iCnt + 1.
  END.

  dBx3 = dBx3 / iCnt.
  dAc3 = dAc3 / iCnt.


  cRet = STRING(dSS1) + dlmt + STRING(dKi1) + dlmt + STRING(dK41) + dlmt + STRING(dBx2) + dlmt + STRING(dAcp) + dlmt + STRING(dLit) +   CHR(10)
       + STRING(dSS2) + dlmt + STRING(dKi2) + dlmt + STRING(dK42) + dlmt + STRING(dBx3) + dlmt + STRING(dAcp) + dlmt + STRING(dLi3).

 

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuimicoName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoName Procedure 
FUNCTION getQuimicoName RETURNS CHARACTER
  (piQuimico AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "NOT-FOUND".
  FIND FIRST quimicos WHERE quimicos.id_quimico = piQuimico
                      NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN
    cRet =  quimicos.nombre.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRangos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRangos Procedure 
FUNCTION getRangos RETURNS CHARACTER
  (pcRowIds AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcAux   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fTar    AS DECIMAL    NO-UNDO.

  cRows = pcRowIds.

  FOR EACH ttSelected.
    DELETE ttSelected.
  END.
  FOR EACH ttRangos.
    DELETE ttRangos.
  END.

  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:

    cFields = getDatosTambor(TO-ROWID(ENTRY(iRow, cRows))).
    
    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del tambor".
  
    CREATE ttSelected.
    ASSIGN ttSelected.id_empresa    = INTEGER(ENTRY(1, cFields, CHR(1)))
           ttSelected.id_sucursal   = INTEGER(ENTRY(2, cFields, CHR(1)))
           ttSelected.id_tipotambor = INTEGER(ENTRY(3, cFields, CHR(1)))
           ttSelected.nromov        = INTEGER(ENTRY(4, cFields, CHR(1)))
           ttSelected.id_tambor     = INTEGER(ENTRY(5, cFields, CHR(1)))
           ttSelected.id_articulo   = INTEGER(ENTRY(11, cFields, CHR(1)))
           ttSelected.id_calidad    = INTEGER(ENTRY(12, cFields, CHR(1)))
           ttSelected.id_envase     = INTEGER(ENTRY(13, cFields, CHR(1)))
           ttSelected.kilos_tambor  = DECIMAL(REPLACE(ENTRY(14, cFields, CHR(1)), ".", ","))
           ttSelected.tara          = DECIMAL(REPLACE(ENTRY(15, cFields, CHR(1)), ".", ","))
           ttSelected.id_lote       = INTEGER(ENTRY(6, cFields, CHR(1)))
           ttSelected.anio          = INTEGER(ENTRY(7, cFields, CHR(1)))
           .

  END. /*do iRow = 1 to  num-entries ...*/

  FOR EACH ttSelected
      BREAK BY ttSelected.nromov 
      BY ttSelected.id_tambor.
    
    ASSIGN fKil = fKil + ttSelected.kilos_tambor
           fTar = fTar + ttSelected.tara.
    
    IF FIRST-OF(ttSelected.nromov) THEN
      i = ttSelected.id_tambor.

    IF LAST-OF(ttSelected.nromov) THEN DO:      
      CREATE ttRangos.
      BUFFER-COPY ttSelected EXCEPT kilos_tambor tara TO ttRangos.
      ASSIGN ttRangos.id_tambor_desde = i
             ttRangos.id_tambor_hasta = ttSelected.id_tambor
             ttRangos.kilos_tambor    = fKil
             ttRangos.tara            = fTar             
             fKil                     = 0
             fTar                     = 0
             .
    END.

  END.  /*each ttSelected*/

  pcAux = "".
  FOR EACH ttRangos.

    pcAux = pcAux + 
            STRING(ttRangos.id_empresa)      + CHR(1) + 
            STRING(ttRangos.id_sucursal)     + CHR(1) + 
            STRING(ttRangos.id_tipotambor)   + CHR(1) + 
            STRING(ttRangos.nromov)          + CHR(1) + 
            STRING(ttRangos.id_tambor_desde) + CHR(1) + 
            STRING(ttRangos.id_tambor_hasta) + CHR(1) + 
            STRING(ttRangos.id_articulo)     + CHR(1) + 
            STRING(ttRangos.id_calidad)      + CHR(1) + 
            STRING(ttRangos.id_envase)       + CHR(1) + 
            STRING(ttRangos.kilos_tambor)    + CHR(1) + 
            STRING(ttRangos.tara)            + CHR(1) + 
            STRING(ttRangos.id_lote)         + CHR(1) +
            STRING(ttRangos.anio)            + CHR(10).
    
  END.
  pcAux = SUBSTRING(pcAux, 1, LENGTH(pcAux) - 1).




  RETURN pcAux.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRatio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRatio Procedure 
FUNCTION getRatio RETURNS DECIMAL
  (pdBrix   AS DECIMAL,
   pdAcidez AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  
  dRet = pdBrix / pdAcidez.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSolidosSolubles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSolidosSolubles Procedure 
FUNCTION getSolidosSolubles RETURNS DECIMAL
  (pdBrix   AS DECIMAL, /*corregido*/
   pdLitros AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoef AS DECIMAL    NO-UNDO.

  dCoef = getCoefSolidosSolubles(pdBrix).
  dRet  = pdLitros * dCoef.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStockFechaSucursalTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStockFechaSucursalTambor Procedure 
FUNCTION getStockFechaSucursalTambor RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER, 
   piTbo AS INTEGER, 
   pdFec AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la sucursal en la que se encontraba el tambor en la fecha
            parametro
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iUbi    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFec    AS DATE       NO-UNDO.
  
  
  ASSIGN iEmp = piEmp
         iSuc = piSuc
         iTip = piTip
         iNro = piNro
         iTbo = piTbo
         dFec = pdFec.

  
  FIND LAST stock_historico_tambores WHERE stock_historico_tambores.id_empresa    = iEmp
                                       AND stock_historico_tambores.id_sucursal   = iSuc
                                       AND stock_historico_tambores.id_tipotambor = iTip
                                       AND stock_historico_tambores.nromov        = iNro
                                       AND stock_historico_tambores.signo         = "-"
                                       AND stock_historico_tambores.fecha        <= dFec
                                       AND stock_historico_tambores.tambor_desde <= iTbo                                      
                                       AND stock_historico_tambores.tambor_hasta >= iTbo.
  IF AVAILABLE stock_historico_tambores THEN DO:
    /*DISP fecha id_suc_ori id_suc_des tambor_desde tambor_hasta datos_adicionales c_fecha WITH WIDTH 150.*/
    iUbi = stock_historico_tambores.id_suc_des.
  END.

  RETURN iUbi.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTaraEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTaraEnvase Procedure 
FUNCTION getTaraEnvase RETURNS DECIMAL
  (piEnvase AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST envases_prod WHERE envases_prod.id_envase = piEnvase NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.tara.
  ELSE
    RETURN 0.00.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUltimoTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUltimoTambor Procedure 
FUNCTION getUltimoTambor RETURNS INTEGER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve el nro del ultimo tambor del lote.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buTam FOR tambores_industria.

  FIND LAST buTam WHERE buTam.id_empresa    = piEmp
                    AND buTam.id_sucursal   = piSuc
                    AND buTam.id_tipotambor = piTip
                    AND buTam.nromov        = piNro
                  NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN 
    RETURN buTam.id_tambor.
  ELSE
    RETURN 0.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUsuariosLista) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUsuariosLista Procedure 
FUNCTION getUsuariosLista RETURNS CHARACTER
  (piLista AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH usuarios_listas WHERE usuarios_listas.id_lista = piLista
                           NO-LOCK.
    cRet = cRet + usuarios_listas.email + ",".            
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValoresAnalisis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValoresAnalisis Procedure 
FUNCTION getValoresAnalisis RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcidez AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRatio  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.

  IF piTip = 4 THEN DO: /*tambor de sobrante*/
    FOR FIRST sobrante WHERE sobrante.id_empresa             = piEmp
                         AND sobrante.id_sucursal            = piSuc
                         AND sobrante.id_tipotambor_sobrante = piTip
                         AND sobrante.nromov_sobrante        = piNro
                       NO-LOCK.
      FOR FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa    = sobrante.id_empresa
                                    AND inspecciones_lote.id_sucursal   = sobrante.id_sucursal
                                    AND inspecciones_lote.id_tipotambor = sobrante.id_tipotambor
                                    AND inspecciones_lote.nromov        = sobrante.nromov
                                    AND inspecciones_lote.final         = TRUE 
                                  NO-LOCK .
        ASSIGN dBrix   = inspecciones_lote.bx_correg
               dAcidez = inspecciones_lote.acidez_w_w
               dRatio  = inspecciones_lote.ratio.
      END.
    END.
  END.

  IF piTip = 5 THEN DO: /*tambor de arrastre*/
    ASSIGN dBrix   = 0
           dAcidez = 0
           dRatio  = 0.
  END.
  
  IF piTip = 3 THEN DO: /*tambor de lote*/
    FOR FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa    = piEmp
                                  AND inspecciones_lote.id_sucursal   = piSuc
                                  AND inspecciones_lote.id_tipotambor = piTip
                                  AND inspecciones_lote.nromov        = piNro
                                  AND inspecciones_lote.final         = TRUE 
                                NO-LOCK .
      ASSIGN dBrix   = inspecciones_lote.bx_correg
             dAcidez = inspecciones_lote.acidez_w_w
             dRatio  = inspecciones_lote.ratio.
    END.
  END.
  IF piTip = 1 THEN DO: /*tambor de produccion*/
    FOR FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = piEmp
                                AND produccion_jugo.id_sucursal   = piSuc
                                AND produccion_jugo.id_tipotambor = piTip
                                AND produccion_jugo.nromov        = piNro
                              NO-LOCK .
      ASSIGN dBrix   = produccion_jugo.bx_correg
             dAcidez = produccion_jugo.acidez_w_w
             dRatio  = produccion_jugo.ratio.


    END.

  END.

  cRet = STRING(dBrix)   + CHR(1) + 
         STRING(dAcidez) + CHR(1) + 
         STRING(dRatio).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

