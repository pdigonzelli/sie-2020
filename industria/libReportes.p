&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Oricription :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE dDesde AS DATE       NO-UNDO.
DEFINE VARIABLE dHasta AS DATE       NO-UNDO.



DEFINE TEMP-TABLE ttOrdenReporte
  FIELD id_tipotambor AS INTEGER
  FIELD id_articulo   AS INTEGER
  FIELD id_orden      AS INTEGER
  FIELD id_estado     AS INTEGER.


DEFINE TEMP-TABLE ttStockFecha
  FIELD id_empresa            AS INTEGER
  FIELD id_sucursal           AS INTEGER
  FIELD id_tipotambor         AS INTEGER
  FIELD nromov                AS INTEGER
  FIELD id_tambor             AS INTEGER
  FIELD id_articulo           AS INTEGER
  FIELD id_calidad            AS INTEGER
  FIELD id_envase             AS INTEGER
  FIELD id_lote               AS INTEGER
  FIELD anio                  AS INTEGER
  FIELD id_sucursal_remito    AS INTEGER
  FIELD id_tipo_movsto        AS INTEGER
  FIELD nro_remito            AS INTEGER
  FIELD ITEM_factura          AS INTEGER
  FIELD id_sucursal_ubicacion AS INTEGER
  FIELD id_estado             AS INTEGER
  FIELD nromov_destino        AS INTEGER
  FIELD kilos                 AS DECIMAL
  FIELD fecha                 AS DATE.


DEFINE TEMP-TABLE ttReproceso 
    RCODE-INFORMATION
    FIELD id_lote                   AS INTEGER  COLUMN-LABEL "Lote"
    FIELD anio                      AS INTEGER  COLUMN-LABEL "Anio"
    FIELD tambores                  AS INTEGER  COLUMN-LABEL "Tambores"
    FIELD kilos                     AS DECIMAL  COLUMN-LABEL "Kgs"
    FIELD kilos_400                 AS DECIMAL  COLUMN-LABEL "Kgs 400"
    FIELD articulo                  AS CHAR     COLUMN-LABEL "Articulo"
    FIELD calidad                   AS CHARACT  COLUMN-LABEL "Calidad"
    FIELD envase                    AS CHARACT  COLUMN-LABEL "Envase"
    FIELD fecha                     AS DATE     COLUMN-LABEL "Fecha"
    FIELD condicion                 AS CHARACT  COLUMN-LABEL "Condicion"    
    FIELD id_sucursal               AS INTEGER  COLUMN-LABEL "Suc"
    FIELD tipotambor                AS CHARACT  COLUMN-LABEL "Tipo Tambor"       
    /*FIELD id_empresa                AS INTEGER  COLUMN-LABEL "Empresa"    
    FIELD id_tipotambor             AS INTEGER  COLUMN-LABEL "Ttambor"
    FIELD id_articulo               AS INTEGER  COLUMN-LABEL "Art."    
    FIELD id_calidad                AS INTEGER  COLUMN-LABEL "Cal"    
    FIELD id_envase                 AS INTEGER  COLUMN-LABEL "Env"*/
    
    
    /*FIELD lotmov                    AS CHARACT  COLUMN-LABEL "LotMov"*/
    
    FIELD id_lote_origen            AS INTEGER  COLUMN-LABEL "Lote Ori"
    FIELD anio_origen               AS INTEGER  COLUMN-LABEL "Anio Ori"
    FIELD tambores_origen           AS INTEGER  COLUMN-LABEL "Tambores Ori"
    FIELD kilos_origen              AS DECIMAL  COLUMN-LABEL "Kgs Ori"
    FIELD kilos_400_origen          AS DECIMAL  COLUMN-LABEL "Kgs 400 Ori"    
    FIELD articulo_origen           AS CHAR     COLUMN-LABEL "Articulo Ori"
    FIELD calidad_origen            AS CHARACT  COLUMN-LABEL "Calidad Ori"  
    FIELD envase_origen             AS CHARACT  COLUMN-LABEL "Envase Ori"
    FIELD fecha_origen              AS DATE     COLUMN-LABEL "Fecha Ori"  
    FIELD tipotambor_origen         AS CHARACT  COLUMN-LABEL "Tipo Tambor Ori"    
    FIELD nromov                    AS INTEGER  COLUMN-LABEL "Nromov"
    FIELD nromov_origen             AS INTEGER  COLUMN-LABEL "nromov_origen"
    /*FIELD id_empresa_origen         AS INTEGER  COLUMN-LABEL "Empresa Ori"
    FIELD id_sucursal_origen        AS INTEGER  COLUMN-LABEL "Suc Ori"
    FIELD id_tipotambor_origen      AS INTEGER  COLUMN-LABEL "Ttambor Ori"
    FIELD id_articulo_origen        AS INTEGER  COLUMN-LABEL "Art. Ori"    
    FIELD id_calidad_origen         AS INTEGER  COLUMN-LABEL "Cal Ori"
    FIELD id_envase_origen          AS INTEGER  COLUMN-LABEL "Env Ori"        
    FIELD kilos_sobrante            AS DECIMAL  COLUMN-LABEL "Kgs Sob "
    FIELD merma                     AS DECIMAL  COLUMN-LABEL "Merma"
    FIELD porcentaje                AS INTEGER  COLUMN-LABEL "%".*/.
    /*FIELD lotmov_origen             AS CHARACT  COLUMN-LABEL "LotMov Ori".*/




DEFINE TEMP-TABLE ttOes
  RCODE-INFORMATION
  FIELD id_orden_entrega  AS INTEGER    COLUMN-LABEL "OE"
  FIELD ITEM_oe           AS INTEGER    COLUMN-LABEL "Parte"
  FIELD semana            AS INTEGER    COLUMN-LABEL "Semana"
  FIELD vapor             AS CHARACTER  COLUMN-LABEL "Vapor"
  FIELD cliente           AS CHARACTER  COLUMN-LABEL "Cliente"
  FIELD articulo          AS CHARACTER  COLUMN-LABEL "Articulo"
  FIELD calidad           AS CHARACTER  COLUMN-LABEL "Calidad"
  FIELD tambores          AS INTEGER    COLUMN-LABEL "Tambores"
  FIELD nro_of            AS INTEGER    COLUMN-LABEL "Nro OF"
  FIELD estado            AS CHARACTER  COLUMN-LABEL "Estado"
  FIELD fecha_embarque    AS DATE       COLUMN-LABEL "Fecha Embarque"
  FIELD agencia           AS CHARACTER  COLUMN-LABEL "Agencia"
  FIELD incoterm          AS CHARACTER  COLUMN-LABEL "Incoterm"
  FIELD contenedores      AS INTEGER    COLUMN-LABEL "Cant. Contenedores"
  .

DEFINE TEMP-TABLE ttEstadosContrato
  FIELD id_contrato       AS CHARACTER
  FIELD id_tipo_contrato  AS INTEGER
  FIELD anio_contrato     AS INTEGER
  FIELD ITEM_contrato     AS INTEGER
  FIELD tambores_pedidos  AS INTEGER
  FIELD nromov_contrato   AS INTEGER  
  FIELD tambores_contrato AS INTEGER
  FIELD kilos_contrato    AS DECIMAL
  FIELD id_orden_entrega  AS INTEGER
  FIELD ITEM_oe           AS INTEGER
  FIELD nromov_oe         AS INTEGER
  FIELD tambores_oe       AS INTEGER
  FIELD kilos_oe          AS DECIMAL.

DEFINE TEMP-TABLE ttTamboresContrato
  FIELD id_contrato       AS CHARACTER  
  FIELD ITEM_contrato     AS INTEGER
  FIELD anio              AS INTEGER
  FIELD id_tipo_cont      AS INTEGER
  FIELD nromov            AS INTEGER
  FIELD tambores_pedidos  AS INTEGER
  FIELD tambores          AS INTEGER
  FIELD kilos             AS DECIMAL.

DEFINE TEMP-TABLE ttTamboresOE
  FIELD id_contrato       AS CHARACTER
  FIELD ITEM_contrato     AS INTEGER
  FIELD anio              AS INTEGER
  FIELD id_tipo_cont      AS INTEGER
  FIELD id_orden_entrega  AS INTEGER
  FIELD ITEM_oe           AS INTEGER
  FIELD nromov            AS INTEGER
  FIELD tambores_pedidos  AS INTEGER
  FIELD tambores          AS INTEGER
  FIELD kilos             AS DECIMAL.

DEFINE TEMP-TABLE ttContratos
  FIELD id_contrato       AS CHARACTER
  FIELD id_tipo_contrato  AS INTEGER
  FIELD anio              AS INTEGER
  FIELD ITEM_contrato     AS INTEGER
  FIELD tambores_pedidos  AS INTEGER.

DEFINE TEMP-TABLE ttContratosFecha
  FIELD id_contrato       AS CHARACTER
  FIELD id_tipo_contrato  AS INTEGER
  FIELD anio              AS INTEGER
  FIELD ITEM_contrato     AS INTEGER.

DEFINE TEMP-TABLE ttMapaCamara
  FIELD camara    AS CHARACTER
  FIELD fila      AS CHARACTER
  FIELD columna   AS CHARACTER
  FIELD id_lote   AS INTEGER
  FIELD anio      AS INTEGER
  FIELD articulo  AS CHARACTER
  FIELD tbs       AS INTEGER.

DEFINE TEMP-TABLE ttAnalisis
    RCODE-INFORMATION
    FIELD id_lote     AS INTEGER COLUMN-LABEL "Lote"
    FIELD anio        AS INTEGER COLUMN-LABEL "Anio"
    FIELD id_articulo AS INTEGER COLUMN-LABEL "CodArt"
    FIELD producto    AS CHARACTER COLUMN-LABEL "Producto"
    FIELD calidad     AS CHARACTER COLUMN-LABEL "Calidad"
    FIELD envase      AS CHARACTER COLUMN-LABEL "Envase"
    FIELD acidez_w_w  AS DECIMAL COLUMN-LABEL "Acidez W/W"
    FIELD acidez_w_v  AS DECIMAL COLUMN-LABEL "Acide W/V"
    FIELD bx_20       AS DECIMAL COLUMN-LABEL "Bx 20"
    FIELD bx_corr     AS DECIMAL COLUMN-LABEL "Bx Corr"
    FIELD ftu         AS DECIMAL COLUMN-LABEL "FTU"
    FIELD litros      AS DECIMAL COLUMN-LABEL "Litros"
    FIELD pulpa       AS DECIMAL COLUMN-LABEL "% Pulpa"
    FIELD ratio       AS DECIMAL COLUMN-LABEL "Ratio".

DEFINE TEMP-TABLE ttFacturaRemito
  RCODE-INFORMATION 
  FIELD fecha_remito  AS DATE COLUMN-LABEL "Fecha Remito"
  FIELD nro_remito    AS CHARACTER COLUMN-LABEL "Nro Remito"
  FIELD parte         AS INTEGER COLUMN-LABEL "Parte"
  FIELD lote          AS INTEGER COLUMN-LABEL "Lote"
  FIELD anio          AS INTEGER COLUMN-LABEL "Año"
  FIELD cantidad      AS INTEGER COLUMN-LABEL "Cantidad"
  FIELD kilos_neto    AS DECIMAL COLUMN-LABEL "Kgs Neto"
  FIELD kilos_400     AS DECIMAL COLUMN-LABEL "Kgs 400"
  FIELD articulo      AS CHARACTER COLUMN-LABEL "Producto"
  FIELD calidad       AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase        AS CHARACTER COLUMN-LABEL "Envase"
  FIELD destino       AS CHARACTER COLUMN-LABEL "Destino"
  FIELD lugdes        AS CHARACTER COLUMN-LABEL "Lugar Descarga"
  FIELD transporte    AS CHARACTER COLUMN-LABEL "Transporte"
  FIELD vapor         AS CHARACTER COLUMN-LABEL "Vapor" 
  FIELD cliente       AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD orden_fab     AS INTEGER COLUMN-LABEL "OF"
  FIELD permiso       AS CHARACTER COLUMN-LABEL "Permiso Embarque"
  FIELD estado        AS CHARACTER COLUMN-LABEL "Estado"
  FIELD nro_factura   AS CHARACTER COLUMN-LABEL "Nro Factura"
  FIELD fecha_factura AS CHARACTER COLUMN-LABEL "Fecha Factura"
  FIELD kilos_fact    AS DECIMAL COLUMN-LABEL "Kgs Fact"
  FIELD kilos_400_fac AS DECIMAL COLUMN-LABEL "Kgs 400 Fact"
  FIELD cliente_final AS CHARACTER COLUMN-LABEL "Cliente Final"
  FIELD id_suc_des    AS INTEGER COLUMN-LABEL "Suc Destino"
  .


DEFINE TEMP-TABLE ttDestinosMuestras
  RCODE-INFORMATION
  FIELD nombre        AS CHARACTER COLUMN-LABEL "Destinatario"
  FIELD razon_social  AS CHARACTER COLUMN-LABEL "Razon Social"
  FIELD direccion     AS CHARACTER COLUMN-LABEL "Direccion"
  FIELD localidad     AS CHARACTER COLUMN-LABEL "Localidad"
  FIELD provincia     AS CHARACTER COLUMN-LABEL "Provincia"
  FIELD pais          AS CHARACTER COLUMN-LABEL "Pais"
  FIELD codigo_postal AS CHARACTER COLUMN-LABEL "Cod Pos"
  FIELD tel           AS CHARACTER COLUMN-LABEL "Telefono"
  FIELD email         AS CHARACTER COLUMN-LABEL "eMail"
  FIELD cantidad      AS INTEGER COLUMN-LABEL "Qty Muestras".

DEFINE TEMP-TABLE ttBrix
  RCODE-INFORMATION
  FIELD brix AS DECIMAL COLUMN-LABEL "Brix Corregido"
  FIELD pesp AS DECIMAL COLUMN-LABEL "Peso Especifico"
  FIELD sols AS DECIMAL COLUMN-LABEL "Solido Soluble".

DEFINE TEMP-TABLE ttConsumos
  RCODE-INFORMATION
  FIELD id_articulo AS INTEGER    COLUMN-LABEL "CodArt"
  FIELD articulo    AS CHARACTER  COLUMN-LABEL "Producto"
  FIELD id_envase   AS INTEGER    COLUMN-LABEL "CodEnv"
  FIELD envase      AS CHARACTER  COLUMN-LABEL "Envase"
  FIELD cantidad    AS INTEGER    COLUMN-LABEL "Cantidad".

DEFINE TEMP-TABLE ttAuxCompo
  FIELD nromov_proceso  AS INTEGER
  FIELD nromov_carga    AS INTEGER
  FIELD id_articulo     AS INTEGER
  FIELD cantidad        AS DECIMAL
  FIELD bx              AS DECIMAL
  FIELD ac              AS DECIMAL
  FIELD bx_corr         AS DECIMAL
  FIELD ac_gpl          AS DECIMAL
  FIELD id_unidad       AS INTEGER
  FIELD kilos           AS DECIMAL
  FIELD k400            AS DECIMAL
  .
DEFINE TEMP-TABLE ttAuxRepro
  FIELD nromov_proceso  AS INTEGER
  FIELD nromov_lote     AS INTEGER
  FIELD id_articulo     AS INTEGER
  FIELD tambores        AS INTEGER
  FIELD kilos           AS DECIMAL
  FIELD k400            AS DECIMAL
  FIELD bx              AS DECIMAL
  FIELD ac              AS DECIMAL
  FIELD bx_corr         AS DECIMAL
  FIELD ac_gpl          AS DECIMAL
  .

DEFINE TEMP-TABLE ttConsumosProcesos
  RCODE-INFORMATION
  FIELD id_proceso    AS INTEGER    COLUMN-LABEL "Proceso"
  FIELD anio          AS INTEGER    COLUMN-LABEL "Anio"
  FIELD fecha         AS DATE       COLUMN-LABEL "Fecha" 
  FIELD descripcion   AS CHARACTER  COLUMN-LABEL "Referencia"
  FIELD id_arti_comp  AS INTEGER    COLUMN-LABEL "ArticuloVolcado"
  FIELD producto_comp AS CHARACTER  COLUMN-LABEL "ProductoVolcado"
  FIELD cantidad      AS DECIMAL    COLUMN-LABEL "Cantidad"
  FIELD unidad        AS CHARACTER  COLUMN-LABEL "Unidad"
  FIELD kilos         AS DECIMAL    COLUMN-LABEL "Kilos Nominales"
  FIELD kilos400      AS DECIMAL    COLUMN-LABEL "Kilos 400"
  FIELD tambores      AS INTEGER    COLUMN-LABEL "Tambores"
  FIELD bx            AS DECIMAL    COLUMN-LABEL "Brix"
  FIELD ac            AS DECIMAL    COLUMN-LABEL "Acidez"
  FIELD bx_corr       AS DECIMAL    COLUMN-LABEL "Brix Corregido"
  FIELD gpl           AS DECIMAL    COLUMN-LABEL "Acidez GPL"
  .

DEFINE TEMP-TABLE ttResultadoProcesos
  FIELD id_proceso    AS INTEGER    COLUMN-LABEL "Proceso"
  FIELD anio          AS INTEGER    COLUMN-LABEL "Anio"
  FIELD fecha         AS DATE       COLUMN-LABEL "Fecha" 
  FIELD descripcion   AS CHARACTER  COLUMN-LABEL "Referencia"
  FIELD id_arti_comp  AS INTEGER    COLUMN-LABEL "ArticuloVolcado"
  FIELD producto_comp AS CHARACTER  COLUMN-LABEL "ProductoVolcado"
  FIELD cantidad      AS DECIMAL    COLUMN-LABEL "Cantidad"
  FIELD unidad        AS CHARACTER  COLUMN-LABEL "Unidad"
  FIELD kilos         AS DECIMAL    COLUMN-LABEL "Kilos Nominales"
  FIELD kilos400      AS DECIMAL    COLUMN-LABEL "Kilos 400"
  FIELD tambores      AS INTEGER    COLUMN-LABEL "Tambores"
  FIELD bx            AS DECIMAL    COLUMN-LABEL "Brix"
  FIELD ac            AS DECIMAL    COLUMN-LABEL "Acidez"
  FIELD bx_corr       AS DECIMAL    COLUMN-LABEL "Brix Corregido"
  FIELD gpl           AS DECIMAL    COLUMN-LABEL "Acidez GPL"
  .

DEFINE TEMP-TABLE ttIngresoBalanza
  RCODE-INFORMATION
  FIELD fecha         AS DATE       COLUMN-LABEL "Fecha"
  FIELD nro_pesada    AS INTEGER    COLUMN-LABEL "Nro Pesada"
  FIELD producto      AS CHARACTER  COLUMN-LABEL "Producto"
  FIELD hora_entrada  AS CHARACTER  COLUMN-LABEL "Hs Entrada"
  FIELD hora_salida   AS CHARACTER  COLUMN-LABEL "Hs Salida"
  FIELD peso          AS DECIMAL    COLUMN-LABEL "Peso"
  FIELD linea         AS CHARACTER  COLUMN-LABEL "Linea"
  FIELD origen        AS CHARACTER  COLUMN-LABEL "Origen"
  FIELD proveedor     AS CHARACTER  COLUMN-LABEL "Proveedor"
  .

DEFINE TEMP-TABLE ttProdLoteCascara
  RCODE-INFORMATION
  FIELD id_produccion       AS INTEGER      COLUMN-LABEL "Produccion"
  FIELD anio_prod           AS INTEGER      COLUMN-LABEL "Anio Prod"
  FIELD cantidad_prod       AS INTEGER      COLUMN-LABEL "Cantidad Prod"
  FIELD kilos_prod          AS DECIMAL      COLUMN-LABEL "Kgs Prod"
  FIELD fecha_asoc          AS DATE         COLUMN-LABEL "Fecha Asoc"
  FIELD cantidad_asoc       AS INTEGER      COLUMN-LABEL "Cantidad Asoc"
  FIELD kilos_asoc          AS DECIMAL      COLUMN-LABEL "Kgs Asoc"
  FIELD id_lote             AS INTEGER      COLUMN-LABEL "Lote"
  FIELD anio_lote           AS INTEGER      COLUMN-LABEL "Anio Lote"
  FIELD cantidad_lote       AS INTEGER      COLUMN-LABEL "Cantidad Lote"
  FIELD kilos_lote          AS DECIMAL      COLUMN-LABEL "Kgs Lote"
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

&IF DEFINED(EXCLUDE-getBancoOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBancoOE Procedure 
FUNCTION getBancoOE RETURNS CHARACTER
  (piCli AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCabeceraTieneFechaCierre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCabeceraTieneFechaCierre Procedure 
FUNCTION getCabeceraTieneFechaCierre RETURNS LOGICAL
  (prTambor AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadesLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadesLote Procedure 
FUNCTION getCantidadesLote RETURNS CHARACTER
  (piEmpresa    AS INTEGER, 
   piSucursal   AS INTEGER, 
   piTipoTambor AS INTEGER, 
   piNromov     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadKilosSucUbi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadKilosSucUbi Procedure 
FUNCTION getCantidadKilosSucUbi RETURNS CHARACTER
  (piEmpresa    AS INTEGER, 
   piSucursal   AS INTEGER, 
   piTipoTambor AS INTEGER, 
   piNromov     AS INTEGER, 
   piSucUbi     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosContratoLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosContratoLote Procedure 
FUNCTION getDatosContratoLote RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

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

&IF DEFINED(EXCLUDE-getDescArticuloIngles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescArticuloIngles Procedure 
FUNCTION getDescArticuloIngles RETURNS CHARACTER
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

&IF DEFINED(EXCLUDE-getDescCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescCliente Procedure 
FUNCTION getDescCliente RETURNS CHARACTER
  (piCliente AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescCondicionLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescCondicionLote Procedure 
FUNCTION getDescCondicionLote RETURNS CHARACTER
  (iEmp AS INTEGER,
   iSuc AS INTEGER,
   iTip AS INTEGER,
   iNro AS INTEGER)  FORWARD.

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

&IF DEFINED(EXCLUDE-getDescEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescEstado Procedure 
FUNCTION getDescEstado RETURNS CHARACTER
  (piEstado AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescEstados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescEstados Procedure 
FUNCTION getDescEstados RETURNS CHARACTER
  (piEstado     AS INTEGER, 
   piTipoTambor AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescLugarDescarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescLugarDescarga Procedure 
FUNCTION getDescLugarDescarga RETURNS CHARACTER
  (piLugDes AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescOrdenFabricacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescOrdenFabricacion Procedure 
FUNCTION getDescOrdenFabricacion RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piTipo     AS INTEGER,
   piAnio     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescProveedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescProveedor Procedure 
FUNCTION getDescProveedor RETURNS CHARACTER
  (piProveedor AS INTEGER)  FORWARD.

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

&IF DEFINED(EXCLUDE-getDescTanque) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescTanque Procedure 
FUNCTION getDescTanque RETURNS CHARACTER
  (piTanque AS INTEGER)  FORWARD.

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

&IF DEFINED(EXCLUDE-getDescUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescUnidad Procedure 
FUNCTION getDescUnidad RETURNS CHARACTER
  (piUnidad AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExisteCabecera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExisteCabecera Procedure 
FUNCTION getExisteCabecera RETURNS LOGICAL
  (prLote AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExisteDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExisteDestino Procedure 
FUNCTION getExisteDestino RETURNS LOGICAL
  (piNroMov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalonesTambor Procedure 
FUNCTION getGalonesTambor RETURNS DECIMAL
  (pdBrixCorreg AS DECIMAL,
   pdPeso       AS DECIMAL)  FORWARD.

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

&IF DEFINED(EXCLUDE-getKilosFromAnalisis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosFromAnalisis Procedure 
FUNCTION getKilosFromAnalisis RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteCerradoEnPeriodo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteCerradoEnPeriodo Procedure 
FUNCTION getLoteCerradoEnPeriodo RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   pdDesde AS DATE,
   pdHasta AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteCerradoEnPeriodod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteCerradoEnPeriodod Procedure 
FUNCTION getLoteCerradoEnPeriodod RETURNS LOGICAL
  (prLote  AS ROWID,
   pdDesde AS DATE,
   pdHasta AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMerma) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMerma Procedure 
FUNCTION getMerma RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER, 
   pdKil AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOF Procedure 
FUNCTION getOF RETURNS INTEGER
  (piContrato AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrdenReporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdenReporte Procedure 
FUNCTION getOrdenReporte RETURNS INTEGER
  (piTipoTambor AS INTEGER, 
   piArticulo   AS INTEGER, 
   piEstado     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRNPA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRNPA Procedure 
FUNCTION getRNPA RETURNS CHARACTER
  (piArt AS INTEGER, 
   piCal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresCuadrante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTamboresCuadrante Procedure 
FUNCTION getTamboresCuadrante RETURNS CHARACTER
  (piSuc AS INTEGER,
   piCam AS INTEGER,
   piArt AS INTEGER,
   pcRow AS CHARACTER,
   pcCol AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTaraEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTaraEnvase Procedure 
FUNCTION getTaraEnvase RETURNS DECIMAL
  (piEnv AS INTEGER)  FORWARD.

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

&IF DEFINED(EXCLUDE-getValoresAnalisisProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValoresAnalisisProduccion Procedure 
FUNCTION getValoresAnalisisProduccion RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValoresLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValoresLote Procedure 
FUNCTION getValoresLote RETURNS CHARACTER
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
         HEIGHT             = 14.67
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addClienteDespachos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addClienteDespachos Procedure 
PROCEDURE addClienteDespachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcCon  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piAno  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCant AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOrdE AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPteO AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdCKil AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piONro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piOTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdOKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iCEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iONro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKil  AS DECIMAL    NO-UNDO.


  DEFINE BUFFER buTam FOR tambores_industria.
  DEFINE BUFFER buArt FOR productos_terminados.

  
  FIND FIRST buTam WHERE buTam.nromov = piCNro NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN 
    ASSIGN iCEmp = buTam.id_empresa
           iCSuc = buTam.id_sucursal
           iCTip = buTam.id_tipotambor
           iCNro = buTam.nromov
           iCLot = buTam.id_lote
           iCAno = buTam.anio
           iArt  = buTam.id_articulo
           dKil  = buTam.kilos_tambor.

  FIND FIRST tambores_industria WHERE tambores_industria.nromov = piONro NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN 
    ASSIGN iOEmp = tambores_industria.id_empresa
           iOSuc = tambores_industria.id_sucursal
           iOTip = tambores_industria.id_tipotambor
           iONro = tambores_industria.nromov
           iOLot = tambores_industria.id_lote
           iOAno = tambores_industria.anio
           dKil  = tambores_industria.kilos_tambor.

  FIND FIRST contratos WHERE contratos.id_contrato      = pcCon
                         AND contratos.id_tipo_contrato = piTip
                         AND contratos.anio             = piAno
                       NO-LOCK NO-ERROR.

  FIND FIRST items_contratos WHERE items_contratos.id_contrato      = pcCon
                               AND items_contratos.id_tipo_contrato = piTip
                               AND items_contratos.anio             = piAno
                               AND items_contratos.ITEM             = piPte
                             NO-LOCK NO-ERROR.

  FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.  
  FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
  FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
  FIND FIRST buArt OF buTam NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.

  
  CREATE cliente_despachos.
  ASSIGN cliente_despachos.id_contrato            = pcCon
         cliente_despachos.id_tipo_contrato       = piTip
         cliente_despachos.anio_contrato          = piAno
         cliente_despachos.ITEM_contrato          = IF AVAILABLE items_contratos THEN items_contratos.ITEM ELSE 0
         cliente_despachos.tambores_contrato      = piCant
         cliente_despachos.id_orden_entrega       = piOrdE
         cliente_despachos.ITEM_oe                = piPteO
         cliente_despachos.orden_fabricacion      = IF AVAILABLE contratos THEN contratos.orden_fabricacion ELSE 0
         cliente_despachos.id_cliente             = IF AVAILABLE contratos THEN contratos.id_cliente ELSE 0
         cliente_despachos.id_articulo            = IF AVAILABLE items_contratos THEN items_contratos.id_articulo ELSE iArt
         cliente_despachos.fecha                  = IF AVAILABLE items_contratos THEN items_contratos.fecha ELSE DATE("")
         cliente_despachos.id_empresa_contrato    = iCEmp
         cliente_despachos.id_sucursal_contrato   = iCSuc
         cliente_despachos.id_tipotambor_contrato = iCTip
         cliente_despachos.nromov_contrato        = iCNro
         cliente_despachos.id_lote_contrato       = iCLot
         cliente_despachos.anio_lote_contrato     = iCAno
         cliente_despachos.tambores_lote_contrato = piCTam
         cliente_despachos.kilos_lote_contrato    = pdCKil
         cliente_despachos.id_empresa_despacho    = iOEmp
         cliente_despachos.id_sucursal_despacho   = iOSuc
         cliente_despachos.id_tipotambor_despacho = iOTip
         cliente_despachos.nromov_despacho        = iONro
         cliente_despachos.id_lote_despacho       = iOLot
         cliente_despachos.anio_lote_despacho     = iOAno
         cliente_despachos.tambores_lote_despacho = piOTam
         cliente_despachos.kilos_lote_despacho    = pdOKil
         cliente_despachos.kilos_faltante         = dKil
         cliente_despachos.cliente                = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NOT FOUND"
         cliente_despachos.articulo               = IF AVAILABLE items_contratos THEN productos_terminados.descripcion ELSE (IF AVAILABLE buArt THEN buArt.descripcion ELSE "NONE")
         cliente_despachos.calidad                = IF AVAILABLE calidades THEN calidades.abreviatura ELSE ""
         cliente_despachos.envase                 = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NOT FOUND".




  RELEASE buTam.
  RELEASE buArt.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addDespachoIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addDespachoIndustria Procedure 
PROCEDURE addDespachoIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prItemFactura AS ROWID      NO-UNDO.

  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  
  DEFINE BUFFER buItem FOR items_factura.
  DEFINE BUFFER buRemi FOR remitos.

  FOR FIRST buItem WHERE ROWID(buItem) = prItemFactura NO-LOCK.
    FIND FIRST buRemi OF buItem NO-LOCK NO-ERROR.
    FIND FIRST sucursales OF buRemi NO-LOCK NO-ERROR.
    FIND FIRST clientes OF buRemi NO-LOCK NO-ERROR.
    FIND FIRST clientes OF buRemi NO-LOCK NO-ERROR .
    FIND FIRST destinos OF buRemi NO-LOCK NO-ERROR .
    FIND FIRST lugar_descarga OF buRemi NO-LOCK NO-ERROR.
    FIND FIRST proveedores OF buRemi NO-LOCK NO-ERROR. 
    FIND FIRST envases_prod OF buItem NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF buItem NO-LOCK NO-ERROR.
    FIND FIRST calidades OF buItem NO-LOCK NO-ERROR.

    FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = buItem.id_sucursal
                                 AND r_tambor_remito.id_tipo_movsto     = buItem.id_tipo_movsto
                                 AND r_tambor_remito.nro_remito         = buItem.nro
                                 AND r_tambor_remito.ITEM_factura       = buItem.ITEM
                               NO-LOCK NO-ERROR.
    IF AVAILABLE r_tambor_remito THEN DO:
      FIND FIRST tambores_industria WHERE tambores_industria.id_empresa     = r_tambor_remito.id_empresa
                                      AND tambores_industria.id_sucursal    = r_tambor_remito.id_sucursal
                                      AND tambores_industria.id_tipotambor  = r_tambor_remito.id_tipotambor
                                      AND tambores_industria.nromov         = r_tambor_remito.nromov
                                      AND tambores_industria.id_tambor      = r_tambor_remito.id_tambor                                      
                                    NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN DO:

        IF tambores_industria.codigo_lote <> "" THEN
          cLot = tambores_industria.codigo_lote.
        ELSE
          cLot = STRING(tambores_industria.id_lote) + "/" + STRING(INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))).

        FIND FIRST contratos WHERE contratos.id_contrato      = tambores_industria.id_contrato_of
                               AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                               AND contratos.anio             = tambores_industria.anio_of
                              NO-LOCK NO-ERROR.
      END.
    END.
        
    dKil = buItem.cantidad * buItem.peso.
    dKi4 = getKilos400(buItem.id_tipotambor, 
                       buItem.id_articulo, 
                       tambores_industria.id_calidad, 
                       dKil).   
    IF lugar_descarga.id_sucursal <> 95 AND lugar_descarga.id_sucursal <> 96 THEN DO:    
   
      CREATE despachos_industria.
      ASSIGN despachos_industria.id_sucursal       = buRemi.id_sucursal
             despachos_industria.sucursal          = comercial.sucursales.abreviatura
             despachos_industria.fecha             = buRemi.fecha
             despachos_industria.nro_comprobante   = buRemi.nro_comprobante
             despachos_industria.id_cliente        = buRemi.id_cliente
             despachos_industria.cliente           = clientes.nombre
             despachos_industria.id_destino        = buRemi.id_destino
             despachos_industria.destino           = destinos.abreviatura
             despachos_industria.id_proveedor      = buRemi.id_proveedor
             despachos_industria.proveedor         = proveedores.nombre
             despachos_industria.chofer            = buRemi.chofer
             despachos_industria.chasis            = buRemi.pat_chasis
             despachos_industria.acoplado          = buRemi.pat_acopla
             despachos_industria.id_envase         = buItem.id_envase
             despachos_industria.envase            = envases_prod.abreviatura
             despachos_industria.id_articulo       = buItem.id_articulo
             despachos_industria.articulo          = productos_terminados.abreviatura
             despachos_industria.id_lote           = tambores_industria.id_lote
             despachos_industria.anio_lote         = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))
             despachos_industria.tambores          = buItem.cantidad
             despachos_industria.kilos             = dKil
             despachos_industria.kilos_400         = dKi4
             despachos_industria.nro_per_embarque  = buRemi.nro_per_embarque
             despachos_industria.id_lugdes         = buRemi.id_lugdes
             despachos_industria.lugdes            = lugar_descarga.descripcion
             despachos_industria.id_orden_entrega  = buRemi.id_orden_entrega
             despachos_industria.id_calidad        = buItem.id_calidad
             despachos_industria.calidad           = calidades.abreviatura           
             despachos_general.orden_fabricacion = IF AVAILABLE contratos THEN STRING(contratos.orden_fabricacion) ELSE ""
             despachos_industria.id_contrato       = IF AVAILABLE contratos THEN contratos.id_contrato ELSE ""
             despachos_industria.codigo_lote       = cLot
             .
    END.
           

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPerdidaCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPerdidaCarga Procedure 
PROCEDURE addPerdidaCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prCarga AS ROWID      NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBxc AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buCga FOR cargas.
  DEFINE BUFFER buLot FOR lotes_jugo.
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.


  FOR FIRST buCga WHERE ROWID(buCga) = prCarga NO-LOCK.
    dBxc = buCga.bx_20_20 + DYNAMIC-FUNCTION('getCoefCorreccionAcidez' IN hLib, buCga.acidez_w_w).
    dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, dBxc, buCga.litros).
    dKil = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, buCga.acidez_w_w, buCga.bx_20_20, buCga.litros).
    dKi4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, buCga.acidez_w_w, buCga.bx_20_20, buCga.litros, FALSE).
    
    CREATE perdida_carga.
    ASSIGN perdida_carga.id_empresa     = buCga.id_empresa
           perdida_carga.id_sucursal    = buCga.id_sucursal
           perdida_carga.id_tipotambor  = buCga.id_tipotambor
           perdida_carga.nromov         = buCga.nromov
           perdida_carga.id_carga       = buCga.id_carga
           perdida_carga.anio           = buCga.anio
           perdida_carga.fecha          = buCga.fecha
           perdida_carga.brix_20_20     = buCga.bx_20_20
           perdida_carga.acidez_p_p     = buCga.acidez_w_v
           perdida_carga.kilos          = dKil
           perdida_cargas.kilos_400     = dKi4
           perdida_cargas.litros        = buCga.litros
           perdida_carga.solidos        = dSol
           perdida_carga.id_proceso     = buCga.id_proceso.

           
  END.


  
  FOR FIRST buLot WHERE ROWID(buLot) = prCarga NO-LOCK.
    cAnl = getValoresAnalisis(buLot.id_empresa,
                              buLot.id_sucursal,
                              buLot.id_tipotambor,
                              buLot.nromov).
    IF cAnl <> "" AND cAnl <> ? THEN DO:
      dBxc = DECIMAL(ENTRY(3, cAnl, CHR(1))).
      dSol = 0.
      dKil = getKilosFromAnalisis(buLot.id_empresa, buLot.id_sucursal, buLot.id_tipotambor, buLot.nromov).
      dKi4 = getKilos400(buLot.id_tipotambor, buLot.id_articulo, buLot.id_calidad, dKil).
    END.
    
    CREATE perdida_carga.
    ASSIGN perdida_carga.id_empresa     = buLot.id_empresa
           perdida_carga.id_sucursal    = buLot.id_sucursal
           perdida_carga.id_tipotambor  = buLot.id_tipotambor
           perdida_carga.nromov         = buLot.nromov
           perdida_carga.id_carga       = buLot.id_lote
           perdida_carga.anio           = buLot.anio
           perdida_carga.fecha          = buLot.fecha
           perdida_carga.brix_20_20     = DECIMAL(ENTRY(3, cAnl, CHR(1)))
           perdida_carga.acidez_p_p     = DECIMAL(ENTRY(1, cAnl, CHR(1)))
           perdida_carga.kilos          = dKil
           perdida_cargas.kilos_400     = dKi4
           perdida_cargas.litros        = DECIMAL(ENTRY(6, cAnl, CHR(1)))
           perdida_carga.solidos        = dSol
           perdida_carga.id_proceso     = 0.


  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPerdidaComposicionCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPerdidaComposicionCarga Procedure 
PROCEDURE addPerdidaComposicionCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prCompo AS ROWID      NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBxc AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buCompo FOR composicion_carga.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  FOR FIRST buCompo WHERE ROWID(buCompo) = prCompo 
                    NO-LOCK.
    IF buCompo.id_articulo = 500 THEN DO:
      dKil = buCompo.cantidad.
      dKi4 = 0.

    END.
    ELSE DO:    
      dBxc = buCompo.bx_20_20 + DYNAMIC-FUNCTION('getCoefCorreccionAcidez' IN hLib, acidez_w_w).
      dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, dBxc, buCompo.cantidad).
      dKil = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, buCompo.acidez_w_w,
                                                            buCompo.bx_20_20, 
                                                            buCompo.cantidad).
      /*
      dKi4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, buCompo.acidez_w_w,
                                                        buCompo.bx_20_20, 
                                                        buCompo.cantidad,
                                                        FALSE).*/
    END.

    CREATE perdida_composicion.
    ASSIGN perdida_composicion.id_empresa     = buCompo.id_empresa
           perdida_composicion.id_sucursal    = buCompo.id_sucursal
           perdida_composicion.id_tipotambor  = buCompo.id_tipotambor
           perdida_composicion.nromov         = buCompo.nromov
           perdida_composicion.id_articulo    = buCompo.id_articulo
           perdida_composicion.cantidad       = buCompo.cantidad
           perdida_composicion.kilos_400      = dKi4
           perdida_composicion.kilos          = dKil
           perdida_composicion.brix_20_20     = buCompo.bx_20_20
           perdida_composicion.acidez_p_p     = buCompo.acidez_w_w
           perdida_composicion.litros         = buCompo.cantidad
           perdida_composicion.solidos        = dSol.

  END.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPerdidaPuntoEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPerdidaPuntoEnvase Procedure 
PROCEDURE addPerdidaPuntoEnvase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prPunto AS ROWID      NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE l    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  DEFINE BUFFER buPunto FOR punto_envase.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  FOR FIRST buPunto WHERE ROWID(buPunto) = prPunto 
                    NO-LOCK.
    /*tomo solo los tambores de produccion que no estan reprocesados en lotes*/
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa       = buPunto.id_empresa
                                  AND tambores_industria.id_sucursal      = buPunto.id_sucursal
                                  AND tambores_industria.id_tipotambor    = buPunto.id_tipotambor
                                  AND tambores_industria.nromov           = buPunto.nromov
                                  AND tambores_industria.nromov_destino   = 0 
                                BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
      l = l + DYNAMIC-FUNCTION('getLitrosTambor' IN hLib, tambores_industria.id_empresa,
                                                          tambores_industria.id_sucursal,
                                                          tambores_industria.id_tipotambor,
                                                          tambores_industria.nromov,
                                                          tambores_industria.kilos_tambor ).
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        dKil = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, buPunto.acidez_w_w,
                                                              buPunto.bx_20_20, 
                                                              l).
        
        dKi4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, buPunto.acidez_w_w,
                                                          buPunto.bx_20_20, 
                                                          l,
                                                          FALSE).

        CREATE perdida_pto_envase.
        ASSIGN perdida_pto_envase.id_empresa            = buPunto.id_empresa 
               perdida_pto_envase.id_sucursal           = buPunto.id_sucursal       
               perdida_pto_envase.id_tipotambor         = buPunto.id_tipotambor
               perdida_pto_envase.nromov                = buPunto.nromov
               perdida_pto_envase.id_empresa_proceso    = buPunto.id_empresa_proceso
               perdida_pto_envase.id_sucursal_proceso   = buPunto.id_sucursal_proceso
               perdida_pto_envase.id_tipotambor_proceso = buPunto.id_tipotambor_proceso
               perdida_pto_envase.nromov_proceso        = buPunto.nromov_proceso
               perdida_pto_envase.brix_20_20            = buPunto.bx_20_20
               perdida_pto_envase.acidez_p_p            = buPunto.acidez_w_w
               perdida_pto_envase.fecha                 = buPunto.fecha
               perdida_pto_envase.kilos                 = dKil
               perdida_pto_envase.kilos_400             = dKi4
               perdida_pto_envase.litros                = l
               perdida_pto_envase.solidos               = buPunto.sol_totales.
        /*agregar campo cantidad de tambores*/

        i = 0.
        k = 0.
        l = 0.

      END.
      
    END.
    
    
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPerdidaTamboresRepro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPerdidaTamboresRepro Procedure 
PROCEDURE addPerdidaTamboresRepro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prTambor AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piCant   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdLitros AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilos  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER plCal400 AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cRmm AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFun AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAci AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBxc AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  FOR FIRST buTam WHERE ROWID(buTam) = prTambor NO-LOCK.
    /*IF buTam.nromov = 67484 THEN DO:
      DEBUGGER:INITIATE().
      DEBUGGER:SET-BREAK().
      
    END.*/
    IF buTam.id_tipotambor = 1 THEN
      cAnl = getValoresAnalisisProduccion(buTam.id_empresa,
                                          buTam.id_sucursal,
                                          buTam.id_tipotambor,
                                          buTam.nromov).
    IF buTam.id_tipotambor = 3 OR 
       buTam.id_tipotambor = 4 THEN
      cAnl = getValoresAnalisis(buTam.id_empresa,
                                buTam.id_sucursal,
                                buTam.id_tipotambor,
                                buTam.nromov).

    IF cAnl <> "" AND cAnl <> ? AND cAnl <> "Aceite" THEN DO:
      dKil = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                                            DECIMAL(ENTRY(3, cAnl, CHR(1))), 
                                                            pdLitros).
      IF plCal400 THEN DO:
        dKi4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                                          DECIMAL(ENTRY(3, cAnl, CHR(1))), 
                                                          pdLitros, 
                                                          FALSE).
      END.
      ELSE DO:
        dKil = pdKilos.
        dKi4 = getKilos400(buTam.id_tipotambor,
                           buTam.id_articulo,
                           buTam.id_calidad,
                           dKil).
      END.
      ASSIGN dBrx = DECIMAL(ENTRY(3, cAnl, CHR(1)))
             dAci = DECIMAL(ENTRY(2, cAnl, CHR(1)))
             dBxc = dBrx + DYNAMIC-FUNCTION('getCoefCorreccionAcidez' IN hLib, dAci)
             dPes = DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLib, dBxc)
             dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, dBxc, dKil / dPes).
    END.
    ELSE DO:
      dKil = pdKilos.
      dKi4 = pdKilos.
      dBrx = 0.
      dAci = 0.
      dBxc = 0.
      dPes = 0.
    END.

    /*repro mismo mes*/
    IF MONTH(buTam.fecha) = MONTH(buTam.fecha_reproceso) THEN
      cRmm = "RMM".
    ELSE
      cRmm = "".
    
                                                          
    CREATE perdida_tbs_repro.
    ASSIGN perdida_tbs_repro.id_empresa             = buTam.id_empresa
           perdida_tbs_repro.id_sucursal            = buTam.id_sucursal
           perdida_tbs_repro.id_tipotambor          = buTam.id_tipotambor
           perdida_tbs_repro.nromov                 = buTam.nromov
           perdida_tbs_repro.id_lote                = buTam.id_lote
           perdida_tbs_repro.anio                   = buTam.anio
           perdida_tbs_repro.tambores               = piCant
           perdida_tbs_repro.kilos                  = dKil
           perdida_tbs_repro.kilos_400              = dKi4
           perdida_tbs_repro.brix_20_20             = dBrx
           perdida_tbs_repro.acidez_p_p             = dAci
           perdida_tbs_repro.fecha                  = buTam.fecha
           perdida_tbs_repro.id_empresa_destino     = buTam.id_empresa_destino
           perdida_tbs_repro.id_sucursal_destino    = buTam.id_sucursal_destino
           perdida_tbs_repro.id_tipotambor_destino  = buTam.id_tipotambor_destino
           perdida_tbs_repro.nromov_destino         = buTam.nromov_destino
           perdida_tbs_repro.litros                 = (dKil / dPes)
           perdida_tbs_repro.solidos                = dSol
           perdida_tbs_repro.repro_mismo_mes        = cRmm.

    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addProduccionIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addProduccionIndustria Procedure 
PROCEDURE addProduccionIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prTambor AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piCant   AS INTEGER    NO-UNDO. /*optional*/
  DEFINE INPUT  PARAMETER pdKilos  AS DECIMAL    NO-UNDO. /*optional*/
  

  DEFINE BUFFER buTi FOR tambores_industria.

  DEFINE VARIABLE cQtys AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUbi  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOrF  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEst  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.

  DEFINE VARIABLE dFec AS DATE       NO-UNDO.

  FIND FIRST buTi WHERE ROWID(buTi) = prTambor
                  NO-LOCK NO-ERROR.
  IF AVAILABLE buTi THEN DO:
    
    FIND FIRST contratos WHERE buTi.id_contrato = contratos.id_contrato NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN
      cCli = getDescCliente(contratos.id_cliente).

    IF piCant <> 0 AND pdKilos <> 0 THEN 
      cQtys = STRING(piCant) + CHR(1) + STRING(pdKilos).
    ELSE    
      cQtys = getCantidadKilosSucUbi(buTi.id_empresa, 
                                     buTi.id_sucursal, 
                                     buTi.id_tipotambor, 
                                     buTi.nromov, 
                                     buTi.id_sucursal).

    /*traigo la fecha de cierre del lote*/
    dFec = buTi.fecha.
    IF buTi.id_tipotambor = 3 THEN DO:
      FOR FIRST lotes_jugo 
             OF buTi
          NO-LOCK.
        dFec = lotes_jugo.fecha_finalizacion.
      END.
    END.
      
    ASSIGN cArt = getDescArticulo(buTi.id_articulo)     
           cCal = getDescCalidad(buTi.id_calidad) 
           cEnv = getDescEnvase(buTi.id_envase)   
           cSuc = getDescSucursal(buTi.id_sucursal)
           iCan = INTEGER(ENTRY(1, cQtys, CHR(1)))
           dKil = DECIMAL(ENTRY(2, cQtys, CHR(1)))
           dKi4 = getKilos400(buTi.id_tipotambor, buTi.id_articulo, buTi.id_calidad, dKil)
           cTip = getDescEstado(buTi.id_estado) + " - " + getDescTipoTambor(buTi.id_tipotambor)
           cUbi = getDescSucursal(buTi.id_sucursal_ubicacion)
           cOrF = getDescOrdenFabricacion(buTi.id_contrato, buTi.id_tipocontrato_of, buTi.anio_of)
           iLot = IF buTi.id_tipotambor = 2 THEN buTi.id_tambor ELSE buTi.id_lote    /*para las producciones de aceite el nro de lote es el nro de tambor*/
           cEst = getDescEstado(buTi.id_estado)
           .

    IF buTi.codigo_lote <> "" THEN
      cLot = buTi.codigo_lote.
    ELSE
      cLot = STRING(buTi.id_lote) + "/" + STRING(INTEGER(SUBSTRING(STRING(buTi.anio),3,2))).


    CREATE produccion_industria.
    ASSIGN produccion_industria.id_empresa              = buTi.id_empresa
           produccion_industria.id_sucursal             = buTi.id_sucursal
           produccion_industria.fecha                   = dFec
           produccion_industria.id_lote                 = iLot
           produccion_industria.anio_lote               = buTi.anio - 2000
           produccion_industria.tambores                = iCan
           produccion_industria.kilos                   = dKil
           produccion_industria.kilos_tambor            = buTi.kilos_tambor
           produccion_industria.id_articulo             = buTi.id_articulo
           produccion_industria.id_calidad              = buTi.id_calidad
           produccion_industria.id_envase               = buTi.id_envase
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.id_cliente              = IF AVAILABLE contratos THEN contratos.id_cliente ELSE 0
           produccion_industria.cliente                 = cCli
           produccion_industria.sucursal                = cSuc
           produccion_industria.articulo                = cArt
           produccion_industria.calidad                 = cCal
           produccion_industria.envase                  = cEnv
           produccion_industria.kilos_400               = dKi4
           produccion_general.orden_fabricacion       = cOrF
           produccion_industria.id_contrato             = buTi.id_contrato_of
           produccion_industria.c_usuario               = cEst /*buTi.c_usuario*/
           produccion_industria.c_fecha                 = buTi.c_fecha /*buTi.c_fecha*/
           produccion_industria.c_hora                  = buTi.c_hora
           produccion_industria.codigo_lote             = cLot
           produccion_industria.nromov                  = buTi.nromov
           .
  END. /*find first buTi*/



  /** produccion cascara**/
  FIND FIRST produccion_cascara WHERE ROWID(produccion_cascara) = prTambor
                                 NO-LOCK NO-ERROR.
  IF AVAILABLE produccion_cascara THEN DO:
    ASSIGN cArt = getDescArticulo(produccion_cascara.id_articulo)     
           cCal = getDescCalidad(626) 
           cEnv = getDescEnvase(14)   
           cSuc = getDescSucursal(produccion_cascara.id_sucursal)
           iCan = produccion_cascara.cantidad
           dKil = iCan * 50
           dKi4 = 0
           cTip = ""
           cUbi = getDescSucursal(produccion_cascara.id_sucursal)
           cOrF = "".    

    CREATE produccion_industria.
    ASSIGN produccion_industria.id_empresa              = produccion_cascara.id_empresa
           produccion_industria.id_sucursal             = produccion_cascara.id_sucursal
           produccion_industria.fecha                   = produccion_cascara.fecha
           produccion_industria.id_lote                 = produccion_cascara.id_prod
           produccion_industria.anio_lote               = produccion_cascara.anio - 2000
           produccion_industria.tambores                = iCan
           produccion_industria.kilos                   = dKil
           produccion_industria.kilos_tambor            = 50
           produccion_industria.id_articulo             = produccion_cascara.id_articulo
           produccion_industria.id_calidad              = 626
           produccion_industria.id_envase               = 14
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.sucursal                = cSuc
           produccion_industria.articulo                = cArt
           produccion_industria.calidad                 = cArt
           produccion_industria.envase                  = cEnv
           produccion_industria.kilos_400               = dKi4
           produccion_general.orden_fabricacion       = cOrF
           produccion_industria.c_fecha                 = produccion_cascara.c_fecha
           produccion_industria.c_hora                  = produccion_cascara.c_hora
           produccion_industria.c_usuario               = "STOCK"
           produccion_industria.codigo_lote             = cLot
           .

  END. /*find first produccion_cascara*/


  /*** lotes_cascara ***/
  FIND FIRST lotes_cascara WHERE ROWID(lotes_cascara) = prTambor
                           NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_cascara THEN DO:
    FOR EACH r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = lotes_cascara.id_empresa
                                         AND r_produccion_cascara_lote.id_sucursal_lote   = lotes_cascara.id_sucursal
                                         AND r_produccion_cascara_lote.id_tipotambor_lote = lotes_cascara.id_tipotambor
                                         AND r_produccion_cascara_lote.nromov_lote        = lotes_cascara.nromov
                                       NO-LOCK.
      FIND FIRST produccion_cascara WHERE r_produccion_cascara_lote.id_produccion_prod = produccion_cascara.id_prod
                                      AND r_produccion_cascara_lote.id_sucursal_prod   = produccion_cascara.id_sucursal
                                      /*AND r_produccion_cascara_lote.id_turno           = produccion_cascara.id_turno*/
                                      AND produccion_cascara.fecha >= dDesde 
                                      AND produccion_cascara.fecha <= dHasta
                                    NO-LOCK NO-ERROR.
      IF AVAILABLE produccion_cascara THEN
        iCan = iCan + r_produccion_cascara_lote.cantidad.      
      
          /*iCan = iCan + r_produccion_cascara_lote.cantidad.*/
    END.
    


    ASSIGN cArt = getDescArticulo(lotes_cascara.id_articulo)     
           cCal = getDescCalidad(lotes_cascara.id_calidad) 
           cEnv = getDescEnvase(14)   
           cSuc = getDescSucursal(lotes_cascara.id_sucursal)
           dKil = iCan * 50
           dKi4 = 0
           cTip = ""
           cUbi = getDescSucursal(lotes_cascara.id_sucursal)
           cOrF = ""
           cLot = lotes_cascara.codigo_lote
           .


    CREATE produccion_industria.
    ASSIGN produccion_industria.id_empresa              = lotes_cascara.id_empresa
           produccion_industria.id_sucursal             = lotes_cascara.id_sucursal
           produccion_industria.fecha                   = lotes_cascara.fecha
           produccion_industria.id_lote                 = lotes_cascara.id_lote
           produccion_industria.anio_lote               = lotes_cascara.anio - 2000
           produccion_industria.tambores                = iCan
           produccion_industria.kilos                   = dKil
           produccion_industria.kilos_tambor            = 50
           produccion_industria.id_articulo             = lotes_cascara.id_articulo
           produccion_industria.id_calidad              = lotes_cascara.id_calidad
           produccion_industria.id_envase               = 14
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.sucursal                = cSuc
           produccion_industria.articulo                = STRING(lotes_cascara.id_lote_cliente) + " - " + cArt
           produccion_industria.calidad                 = cCal
           produccion_industria.envase                  = cEnv
           produccion_industria.kilos_400               = dKi4
           produccion_general.orden_fabricacion       = cOrF
           produccion_industria.c_fecha                 = lotes_cascara.c_fecha
           produccion_industria.c_hora                  = lotes_cascara.c_hora
           produccion_industria.c_usuario               = "STOCK"
           produccion_industria.codigo_lote             = IF cLot = "" THEN STRING(lotes_cascara.id_lote) + "/" + STRING(lotes_cascara.anio) ELSE cLot
           .
    iCan = 0.

  END.

  RELEASE produccion_industria.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addStockTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addStockTambores Procedure 
PROCEDURE addStockTambores PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     CUIDADO!!! esta activada la casilla Private
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prTambor AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piCant   AS INTEGER    NO-UNDO. /*optional*/
  DEFINE INPUT  PARAMETER pdKilos  AS DECIMAL    NO-UNDO. /*optional*/
  DEFINE INPUT  PARAMETER plCtrlH  AS LOGICAL    NO-UNDO. /*controla o no la fecha de cierre de su cabecera*/
  
  
  DEFINE BUFFER buTi FOR tambores_industria.
  
  DEFINE VARIABLE cQtys AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUbi  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cListaArticulo AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cListaOrden    AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOrd AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  
  
  /*si no existe cabecera no agrega el registro*/
  IF NOT getExisteCabecera(prTambor) THEN 
    RETURN.

  


  FIND FIRST buTi WHERE ROWID(buTi) = prTambor
                  NO-LOCK NO-ERROR.
  IF AVAILABLE buTi THEN DO:
    cAno = STRING(buTi.anio).
    cAno = SUBSTRING(cAno, 3,4).

    IF piCant <> 0 AND pdKilos <> 0 THEN 
      cQtys = STRING(piCant) + CHR(1) + STRING(pdKilos).
    ELSE    
      cQtys = getCantidadKilosSucUbi(buTi.id_empresa, 
                                     buTi.id_sucursal, 
                                     buTi.id_tipotambor, 
                                     buTi.nromov, 
                                     buTi.id_sucursal_ubicacion).

      
    ASSIGN cArt = getDescArticulo(buTi.id_articulo)     
           cCal = getDescCalidad(buTi.id_calidad) 
           cEnv = getDescEnvase(buTi.id_envase)   
           cSuc = getDescSucursal(buTi.id_sucursal)
           iCan = INTEGER(ENTRY(1, cQtys, CHR(1)))
           dKil = DECIMAL(ENTRY(2, cQtys, CHR(1)))
           dKi4 = getKilos400(buTi.id_tipotambor, buTi.id_articulo, buTi.id_calidad, dKil)
           cTip = getDescEstado(buTi.id_estado) + " - " + getDescTipoTambor(buTi.id_tipotambor)
           iOrd = getOrdenReporte(buTi.id_tipotambor, buTi.id_articulo, buTi.id_estado) 
           cUbi = getDescSucursal(buTi.id_sucursal_ubicacion)
           iAno = INTEGER(cAno)
           iLot = IF buTI.id_tipotambor = 2 THEN buTi.id_tambor ELSE buTi.id_lote.

    IF cArt = "" THEN RETURN. /*si no encuentra articulo no agrega el registro*/

    IF buTi.codigo_lote <> "" THEN
      cLot = buTi.codigo_lote.
    ELSE
      cLot = STRING(iLot) + "/" + STRING(cAno).
/*
    /*control para lotes o procesos cerrados
    si la cabecera del tambor pretenece a un lote abierto o a un proceso abierto, no incluye el tambor*/
    IF plCtrlH THEN DO:  
      IF (buTi.id_tipotambor_destino = 3 OR buTi.id_tipotambor_destino = 10) THEN DO:      
        IF NOT getCabeceraTieneFechaCierre(prTambor) THEN RETURN.
      END.
    END.*/
    
    
    CREATE stock_tambores.
    ASSIGN stock_tambores.id_empresa            = buTi.id_empresa 
           stock_tambores.id_sucursal           = buTi.id_sucursal 
           stock_tambores.sucursal              = cSuc
           stock_tambores.id_sucursal_ubicacion = buTi.id_sucursal_ubicacion  
           stock_tambores.id_lote               = iLot
           stock_tambores.id_tipotambor         = buTi.id_tipotambor
           stock_tambores.anio_lote             = iAno 
           stock_tambores.fecha                 = buTi.fecha
           stock_tambores.id_envase             = buTi.id_envase
           stock_tambores.envase                = cEnv
           stock_tambores.id_articulo           = buTi.id_articulo
           stock_tambores.articulo              = cArt
           stock_tambores.orden_reporte         = iOrd 
           stock_tambores.id_estado             = buTi.id_estado
           stock_tambores.estado                = cUbi 
           stock_tambores.calidad               = cCal
           stock_tambores.id_calidad            = buTi.id_calidad
           stock_tambores.tambores              = iCan
           stock_tambores.kilos                 = dKil
           stock_tambores.kilos_400             = dKi4
           stock_tambores.id_lote_deposito      = STRING(buTi.id_lote_deposito)
           stock_tambores.codigo_lote           = cLot
           .
    
    /*contratos*/
    FIND FIRST contratos WHERE contratos.id_contrato      = buTi.id_contrato_of
                           AND contratos.id_tipo_contrato = buTi.id_tipocontrato_of
                           AND contratos.anio             = buTi.anio_of
                         NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN DO:
      cCli = getDescCliente(contratos.id_cliente).
      ASSIGN stock_tambores.orden_fabricacion   = STRING(contratos.orden_fabricacion) 
             stock_tambores.id_contrato         = contratos.id_contrato
             stock_tambores.anio                = contratos.anio
             stock_tambores.id_cliente          = contratos.id_cliente
             stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
             stock_tambores.cantidad_total_of   = iCan
             stock_tambores.kilos_total_of      = iCan * buTi.kilos_tambor
             stock_tambores.anio_contrato       = INTEGER(SUBSTRING(STRING(YEAR(contrato.fecha)),3,2))
             stock_tambores.cliente             = cCli
             .

    END.
  END.

  RELEASE stock_tambores.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSubdDespacho) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addSubdDespacho Procedure 
PROCEDURE addSubdDespacho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prDespacho AS ROWID      NO-UNDO.

  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTbs AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fPul AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hOrd AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hTam   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  hOrd   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p'). 
  DELETE OBJECT hLibCom.


  FOR FIRST subd_despachos_industria
      WHERE ROWID(subd_despachos_industria) = prDespacho
      NO-LOCK.

    /* finds */
    FIND FIRST items_orden_entrega OF subd_despachos_industria NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria WHERE tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega AND tambores_industria.ITEM_oe = items_orden_entrega.ITEM_oe NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
    FIND FIRST tipos_transporte WHERE tipos_transporte.id_tipo_transporte = subd_despachos_industria.id_tipo_trasporte NO-LOCK NO-ERROR.
    FIND FIRST agencias WHERE agencia.id_agencia = subd_despachos_industria.id_naviera NO-LOCK NO-ERROR.
    FIND FIRST remitos WHERE remitos.id_sucursal = subd_despachos_industria.id_sucursal_remito AND remitos.id_tipo_movsto = subd_despachos_industria.id_tipo_movsto AND remitos.nro = subd_despachos_industria.nro_remito NO-LOCK NO-ERROR.

    IF NOT AVAILABLE tambores_industria THEN NEXT.
    IF NOT AVAILABLE tipos_transporte THEN NEXT.

    iTbs = DYNAMIC-FUNCTION('getTamboresItemOE' IN hOrd, subd_despachos_industria.id_orden_entrega, subd_despachos_industria.ITEM_oe).
    fKil = DYNAMIC-FUNCTION('getKilosItemOE' IN hOrd, subd_despachos_industria.id_orden_entrega, subd_despachos_industria.ITEM_oe).
    fKi4 = DYNAMIC-FUNCTION('getKilos400' IN hTam, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, fKil).
    fPul = 0.

    FIND FIRST inspecciones_lote WHERE tambores_industria.nromov = inspecciones_lote.nromov NO-LOCK NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN 
      fPul = inspecciones_lote.porcentaje_pulpa * 10.
    
    CREATE rptDespacho.
    ASSIGN  rptDespachos.anio_lote                            = INTEGER(SUBSTRING(STRING(tambores_industria.anio), 3, 2))
            rptDespachos.articulo                             = productos_terminados.descripcion
            rptDespachos.calidad                              = calidades.abreviatura
            rptDespachos.cantidad_transportes                 = subd_despachos_industria.cantidad_transportes
            rptDespachos.cliente                              = clientes.razon_social
            rptDespachos.envase                               = envases_prod.abreviatura
            rptDespachos.fecha_despacho                       = subd_despachos_industria.fecha_despacho 
            rptDespachos.fecha_llegada                        = subd_despachos_industria.fecha_arribo
            rptDespachos.id_articulo                          = tambores_industria.id_articulo 
            rptDespachos.id_calidad                           = tambores_industria.id_calidad
            rptDespachos.id_cliente                           = contratos.id_cliente
            rptDespachos.id_contrato                          = contratos.id_contrato
            rptDespachos.id_empresa                           = tambores_industria.id_empresa
            rptDespachos.id_envase                            = tambores_industria.id_envase
            rptDespachos.id_lote                              = tambores_industria.id_lote
            rptDespachos.id_orden_entrega                     = tambores_industria.id_orden_entrega
            rptDespachos.id_sucursal                          = tambores_industria.id_sucursal
            rptDespachos.id_sucursal_remito                   = subd_despachos_industria.id_sucursal_remito
            rptDespachos.id_tipotambor                        = tambores_industria.id_tipotambor 
            rptDespachos.id_tipo_movsto                       = subd_despachos_industria.id_tipo_movsto
            rptDespachos.id_tipo_transporte                   = subd_despachos_industria.id_tipo_trasporte
            rptDespachos.item_oe                              = subd_despachos_industria.ITEM_oe
            rptDespachos.kilos                                = fKil
            rptDespachos.kilos_400                            = fKi4
            rptDespachos.litros                               = IF AVAILABLE inspecciones_lote THEN inspecciones_lote.litros ELSE 0
            rptDespachos.nromov                               = tambores_industria.nromov
            rptDespachos.nro_comprobante                      = IF AVAILABLE remitos THEN remitos.nro_comp ELSE ""
            rptDespachos.nro_remito                           = subd_despachos_industria.nro_remito
            rptDespachos.orden_fabricacion                    = STRING(contratos.orden_fabricacion)
            rptDespachos.semana                               = subd_despachos_industria.semana
            rptDespachos.tambores                             = iTbs
            rptDespachos.tipo_transporte                      = tipos_transporte.descripcion
            rptDespachos.aux_1                                = STRING(fPul)
            .



  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTamboresCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTamboresCamara Procedure 
PROCEDURE addTamboresCamara :
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
  DEFINE INPUT  PARAMETER piLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAni AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEnv AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcRow AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.

  FIND FIRST sucursales WHERE sucursales.id_sucursal = piSuc NO-LOCK NO-ERROR.
  FIND FIRST calidades WHERE calidades.id_calidad = piCal NO-LOCK NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = piEnv NO-LOCK NO-ERROR.
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = piArt NO-LOCK NO-ERROR.
  FIND FIRST camara WHERE camara.id_empresa  = piEmp
                      AND camara.id_sucursal = piSuc
                      AND camara.id_camara   = piCam
                    NO-LOCK NO-ERROR.

  CREATE tambores_camara.
  ASSIGN tambores_camara.id_empresa            = piEmp
         tambores_camara.id_sucursal           = piSuc
         tambores_camara.id_tipotambor         = piTip
         tambores_camara.nromov                = piNro
         tambores_camara.id_tambor             = piTbo
         tambores_camara.id_lote               = piLot
         tambores_camara.anio_lote             = piAni
         tambores_camara.id_articulo           = piArt
         tambores_camara.id_calidad            = piCal
         tambores_camara.id_envase             = piEnv
         tambores_camara.id_sucursal_ubicacion = piSuc
         tambores_camara.id_empresa_camara     = piEmp
         tambores_camara.id_sucursal_camara    = piSuc
         tambores_camara.nro_fila              = pcRow
         tambores_camara.nro_columna           = pcCol
         tambores_camara.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-PRODUCT"
         tambores_camara.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-CALIDAD"
         tambores_camara.envase                = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NO-ENVASE"
         tambores_camara.camara                = IF AVAILABLE camara THEN camara.descripcion ELSE "NO-CAMARA"
         tambores_camara.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NO-SUC".





  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTamboresFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTamboresFecha Procedure 
PROCEDURE addTamboresFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt    AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.


  /*descarto tambores creados despues de la fecha parametro*/
  FOR EACH buTam WHERE buTam.id_sucursal = piSucUbi
                   AND buTam.fecha      >= pdDesde
                   AND buTam.fecha      <= pdHasta
                   AND (IF piLote <> 0 THEN buTam.id_lote     = piLote ELSE TRUE)
                   AND (IF piAnio <> 0 THEN buTam.anio        = piAnio ELSE TRUE)
                   AND (IF piArt  <> 0 THEN buTam.id_articulo = piArt  ELSE TRUE)
                 NO-LOCK.
    CREATE stock_fecha.
    ASSIGN stock_fecha.id_empresa            = buTam.id_empresa
           stock_fecha.id_sucursal           = buTam.id_sucursal
           stock_fecha.id_tipotambor         = buTam.id_tipotambor
           stock_fecha.nromov                = buTam.nromov
           stock_fecha.id_tambor             = buTam.id_tambor
           stock_fecha.id_lote               = buTam.id_lote
           stock_fecha.anio                  = buTam.anio
           stock_fecha.kilos                 = buTam.kilos_tambor
           stock_fecha.id_articulo           = buTam.id_articulo
           stock_fecha.id_calidad            = buTam.id_calidad
           stock_fecha.id_envase             = buTam.id_envase
           stock_fecha.id_sucursal_remito    = buTam.id_sucursal_remito
           stock_fecha.id_tipo_movsto        = buTam.id_tipo_movsto
           stock_fecha.nro_remito            = buTam.nro_remito
           stock_fecha.ITEM_factura          = buTam.ITEM_factura
           stock_fecha.id_sucursal_ubicacion = buTam.id_sucursal_ubicacion
           stock_fecha.id_estado             = buTam.id_estado
           stock_fecha.nromov_destino        = buTam.nromov_destino.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTamboresRemitosEntrada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTamboresRemitosEntrada Procedure 
PROCEDURE addTamboresRemitosEntrada :
/*------------------------------------------------------------------------------
  Purpose:     agrego tambores que llegaron desde otra sucursal via remitos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt   AS INTEGER    NO-UNDO.


  FOR EACH lugar_descarga WHERE id_sucursal = piSuc
                          NO-LOCK, 
      EACH remitos WHERE lugar_descarga.id_lugdes = remitos.id_lugdes
                     AND remitos.fecha           >= pdDesde
                     AND remitos.fecha           <= pdHasta
                   NO-LOCK,
      EACH items_factura OF remitos WHERE (IF piArt <> 0 THEN items_factura.id_articulo = piArt  ELSE TRUE)
                                    NO-LOCK,
      EACH r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                             AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                             AND r_tambor_remito.nro_remito         = items_factura.nro
                             AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                             AND r_tambor_remito.fecha             >= pdDesde
                             AND r_tambor_remito.fecha             <= pdHasta
                           NO-LOCK.

    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = r_tambor_remito.id_empresa
                                    AND tambores_industria.id_sucursal   = r_tambor_remito.id_sucursal
                                    AND tambores_industria.id_tipotambor = r_tambor_remito.id_tipotambor
                                    AND tambores_industria.nromov        = r_tambor_remito.nromov
                                    AND tambores_industria.id_tambor     = r_tambor_remito.id_tambor
                                    AND tambores_industria.fecha        >= pdDesde
                                    AND tambores_industria.fecha        <= pdHasta
                                    AND (IF piLote <> 0 THEN tambores_industria.id_lote     = piLote ELSE TRUE)
                                    AND (IF piAnio <> 0 THEN tambores_industria.anio        = piAnio ELSE TRUE)
                                    AND (IF piArt  <> 0 THEN tambores_industria.id_articulo = piArt  ELSE TRUE)
                                  NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:    
      /*
      MESSAGE tambores_industria.nromov 
              tambores_industria.id_tambor 
              tambores_industria.nro_remito 
              r_tambor_remito.id_sucursal VIEW-AS ALERT-BOX INFO BUTTONS OK.
              */
      CREATE stock_fecha.
      ASSIGN stock_fecha.id_empresa            = tambores_industria.id_empresa
             stock_fecha.id_sucursal           = tambores_industria.id_sucursal
             stock_fecha.id_tipotambor         = tambores_industria.id_tipotambor
             stock_fecha.nromov                = tambores_industria.nromov
             stock_fecha.id_tambor             = tambores_industria.id_tambor
             stock_fecha.kilos                 = tambores_industria.kilos_tambor
             stock_fecha.id_articulo           = tambores_industria.id_articulo
             stock_fecha.id_calidad            = tambores_industria.id_calidad
             stock_fecha.id_envase             = tambores_industria.id_envase
             stock_fecha.id_sucursal_remito    = tambores_industria.id_sucursal_remito
             stock_fecha.id_tipo_movsto        = tambores_industria.id_tipo_movsto
             stock_fecha.nro_remito            = tambores_industria.nro_remito
             stock_fecha.ITEM_factura          = tambores_industria.ITEM_factura
             stock_fecha.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
             stock_fecha.nromov_destino        = tambores_industria.nromov_destino
             stock_fecha.id_estado             = tambores_industria.id_estado.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTTOrden) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTTOrden Procedure 
PROCEDURE addTTOrden PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOrden AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipo  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEst   AS INTEGER    NO-UNDO.

  CREATE ttOrdenReporte. 
  ASSIGN ttOrdenReporte.id_orden      = piOrden
         ttOrdenReporte.id_tipotambor = piTipo
         ttOrdenReporte.id_articulo   = piArt
         ttOrdenReporte.id_estado     = piEst.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callComposicionLoteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callComposicionLoteJugo Procedure 
PROCEDURE callComposicionLoteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTqe AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOrf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCtm AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHco AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHfn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dEst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOfa AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec AS DATE       NO-UNDO.
  DEFINE VARIABLE dFco AS DATE       NO-UNDO.
  DEFINE VARIABLE dFin AS DATE       NO-UNDO.
  DEFINE VARIABLE dBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPul AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKid AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dK4d AS DECIMAL    NO-UNDO.


  RUN deleteCompoLoteJugo.

  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmp
                          AND lotes_jugo.id_sucursal   = piSuc
                          AND lotes_jugo.id_tipotambor = piTip
                          AND lotes_jugo.nromov        = piNro
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:

    ASSIGN iLot = lotes_jugo.id_lote
           iAno = lotes_jugo.anio
           iTqe = lotes_jugo.id_tanque
           dFec = lotes_jugo.fecha
           iArt = lotes_jugo.id_articulo
           cArt = getDescArticulo(lotes_jugo.id_articulo)
           cCal = getDescCalidad(lotes_jugo.id_calidad)
           cEnv = getDescEnvase(lotes_jugo.id_envase)
           dFco = lotes_jugo.fecha_comienzo
           cHco = STRING(lotes_jugo.hora_comienzo,"x(8)") 
           dFin = lotes_jugo.fecha_finalizacion
           cHfn = STRING(lotes_jugo.hora_finalizacion,"x(8)") 
           dPul = lotes_jugo.pulpa
           cAux = getCantidadesLote(piEmp, piSuc, piTip, piNro)
           dKil = DECIMAL(ENTRY(2, cAux, CHR(1)))
           dKi4 = getKilos400(piTip, lotes_jugo.id_articulo, lotes_jugo.id_calidad, dKil)
           dEst = getDescSucursal(piSuc)
           cOfa = getDatosContratoLote(piEmp, piSuc, piTip, piNro)
           cAnl = getValoresAnalisis(piEmp, piSuc, piTip, piNro).

    FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino     = lotes_jugo.id_empresa
                                  AND tambores_industria.id_sucursal_destino    = lotes_jugo.id_sucursal
                                  AND tambores_industria.id_tipotambor_destino  = lotes_jugo.id_tipotambor
                                  AND tambores_industria.nromov_destino         = lotes_jugo.nromov
                                BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:

        FIND FIRST clientes 
             WHERE clientes.id_cliente = INTEGER(ENTRY(5, cOfa, CHR(1)))
             NO-LOCK NO-ERROR.

        CREATE compo_lote_jugo.
        ASSIGN compo_lote_jugo.id_empresa             = piEmp
               compo_lote_jugo.id_sucursal            = piSuc
               compo_lote_jugo.id_tipotambor          = piTip
               compo_lote_jugo.nromov                 = piNro
               compo_lote_jugo.id_lote                = iLot
               compo_lote_jugo.anio_lote              = iAno
               compo_lote_jugo.id_articulo            = tambores_industria.id_articulo
               compo_lote_jugo.articulo               = cArt
               compo_lote_jugo.id_calidad             = tambores_industria.id_calidad
               compo_lote_jugo.calidad                = cCal
               compo_lote_jugo.id_envase              = tambores_industria.id_envase
               compo_lote_jugo.envase                 = getDescEnvase(tambores_industria.id_envase)
               compo_lote_jugo.tambores               = INTEGER(ENTRY(1, cAux, CHR(1)))
               compo_lote_jugo.kilos                  = dkil
               compo_lote_jugo.kilos_400              = dKi4
               compo_lote_jugo.tipotambor             = getDescTipoTambor(lotes_jugo.id_tipotambor)
               compo_lote_jugo.fecha_comienzo         = dFco
               compo_lote_jugo.fecha_finalizacion     = dFin
               compo_lote_jugo.hora_comienzo          = cHco
               compo_lote_jugo.hora_finalizacion      = cHfn
               compo_lote_jugo.estado                 = dEst
               compo_lote_jugo.fecha                  = dFec
               compo_lote_jugo.orden_fabricacion      = ENTRY(4, cOfa, CHR(1))
               compo_lote_jugo.brix                   = DECIMAL(ENTRY(4, cAnl, CHR(1)))
               compo_lote_jugo.pulpa                  = DECIMAL(ENTRY(8, cAnl, CHR(1)))
    
               compo_lote_jugo.id_empresa_destino     = tambores_industria.id_empresa
               compo_lote_jugo.id_sucursal_destino    = tambores_industria.id_sucursal
               compo_lote_jugo.id_tipotambor_destino  = tambores_industria.id_tipotambor
               compo_lote_jugo.nromov_destino         = tambores_industria.nromov
               compo_lote_jugo.id_lote_destino        = tambores_industria.id_lote
               compo_lote_jugo.anio_lote_destino      = tambores_industria.anio
               compo_lote_jugo.id_articulo_destino    = tambores_industria.id_articulo
               compo_lote_jugo.articulo_destino       = getDescArticulo(tambores_industria.id_articulo)
               compo_lote_jugo.id_calidad_destino     = tambores_industria.id_calidad
               compo_lote_jugo.calidad_destino        = getDescCalidad(tambores_industria.id_calidad)
               compo_lote_jugo.id_envase_destino      = tambores_industria.id_envase
               compo_lote_jugo.envase_destino         = getDescEnvase(tambores_industria.id_envase)
               compo_lote_jugo.tambores_destino       = i
               compo_lote_jugo.kilos_destino          = k 
               compo_lote_jugo.kilos_400_destino      = getKilos400(tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
               compo_lote_jugo.tipotambor_destino     = getDescTipoTambor(tambores_industria.id_tipotambor)
               compo_lote_jugo.estado_destino         = lotes_jugo.c_usuario
               compo_lote_jugo.conramarca             = IF AVAILABLE clientes THEN clientes.razon_social ELSE ""
               .
        i = 0.
        k = 0.

      END.
    END.
  END.

  RELEASE compo_lote_jugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callComposicionProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callComposicionProduccion Procedure 
PROCEDURE callComposicionProduccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTqe AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOrf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCtm AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHco AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHfn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dEst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOfa AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUsr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec AS DATE       NO-UNDO.
  DEFINE VARIABLE dFco AS DATE       NO-UNDO.
  DEFINE VARIABLE dFin AS DATE       NO-UNDO.
  DEFINE VARIABLE dBrx AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPul AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKid AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dK4d AS DECIMAL    NO-UNDO.


  FOR EACH compo_produccion_jugo.
    DELETE compo_produccion_jugo.    
  END.

  FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = piEmp
                               AND produccion_jugo.id_sucursal   = piSuc
                               AND produccion_jugo.id_tipotambor = piTip
                               AND produccion_jugo.nromov        = piNro
                             NO-LOCK NO-ERROR.
  IF AVAILABLE produccion_jugo THEN DO:
    FIND FIRST tambores_industria 
            OF produccion_jugo
         NO-LOCK NO-ERROR.

    ASSIGN iLot = produccion_jugo.id_produccion
           iAno = produccion_jugo.anio
           dFec = produccion_jugo.fecha
           iArt = produccion_jugo.id_articulo
           cArt = getDescArticulo(produccion_jugo.id_articulo)
           cCal = getDescCalidad(produccion_jugo.id_calidad)
           cEnv = getDescEnvase(produccion_jugo.id_envase_1)
           cAux = getCantidadesLote(piEmp, piSuc, piTip, piNro)
           dKil = DECIMAL(ENTRY(2, cAux, CHR(1)))
           dKi4 = getKilos400(piTip, produccion_jugo.id_articulo, produccion_jugo.id_calidad, dKil)
           dEst = getDescSucursal(piSuc)
           cOfa = getDatosContratoLote(piEmp, piSuc, piTip, piNro)
           cAnl = getValoresAnalisisProduccion(piEmp, piSuc, piTip, piNro)
           cUsr = IF produccion_jugo.c_usuario = ? THEN tambores_industria.c_usuario ELSE produccion_jugo.c_usuario
           .

    CREATE compo_produccion_jugo.
    ASSIGN compo_produccion_jugo.id_empresa      = piEmp
           compo_produccion_jugo.id_sucursal     = piSuc
           compo_produccion_jugo.id_tipotambor   = piTip
           compo_produccion_jugo.id_lote         = produccion_jugo.id_produccion
           compo_produccion_jugo.anio_lote       = produccion_jugo.anio
           compo_produccion_jugo.tambores        = INTEGER(ENTRY(1, cAux, CHR(1)))
           compo_produccion_jugo.kilos           = dKil
           compo_produccion_jugo.kilos_400       = dKi4
           compo_produccion_jugo.id_articulo     = produccion_jugo.id_articulo
           compo_produccion_jugo.id_calidad      = produccion_jugo.id_calidad
           compo_produccion_jugo.id_envase       = produccion_jugo.id_envase_1
           compo_produccion_jugo.articulo        = cArt
           compo_produccion_jugo.calidad         = cCal
           compo_produccion_jugo.envase          = cEnv
           compo_produccion_jugo.fecha           = dFec
           compo_produccion_jugo.brix_correg     = DECIMAL(ENTRY(4, cAnl, CHR(1))) 
           compo_produccion_jugo.brix_20_20      = DECIMAL(ENTRY(3, cAnl, CHR(1))) 
           compo_produccion_jugo.bisulfito       = DECIMAL(ENTRY(13, cAnl, CHR(1))) 
           compo_produccion_jugo.benzoato        = DECIMAL(ENTRY(12, cAnl, CHR(1))) 
           compo_produccion_jugo.acidez_p_p      = DECIMAL(ENTRY(2, cAnl, CHR(1))) 
           compo_produccion_jugo.acidez_w_w      = DECIMAL(ENTRY(1, cAnl, CHR(1)))
           compo_produccion_jugo.abs_8_430       = DECIMAL(ENTRY(11, cAnl, CHR(1))) 
           compo_produccion_jugo.na              = DECIMAL(ENTRY(7, cAnl, CHR(1))) 
           compo_produccion_jugo.nitrogeno       = DECIMAL(ENTRY(9, cAnl, CHR(1))) 
           compo_produccion_jugo.tipo_fruta      = LOGICAL(ENTRY(17, cAnl, CHR(1)))
           compo_produccion_jugo.pulpa           = DECIMAL(ENTRY(5, cAnl, CHR(1))) 
           compo_produccion_jugo.pulpa_8_5       = DECIMAL(ENTRY(14, cAnl, CHR(1))) 
           compo_produccion_jugo.ratio           = DECIMAL(ENTRY(15, cAnl, CHR(1))) 
           compo_produccion_jugo.t_600           = DECIMAL(ENTRY(16, cAnl, CHR(1))) 
           compo_produccion_jugo.vitamina_c      = DECIMAL(ENTRY(8, cAnl, CHR(1))) 
           compo_produccion_jugo.abs_520         = DECIMAL(ENTRY(10, cAnl, CHR(1))) 
           compo_produccion_jugo.c_usuario       = cUsr
           .
     

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callEstadosContratos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callEstadosContratos Procedure 
PROCEDURE callEstadosContratos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piCli AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.  
  DEFINE INPUT  PARAMETER pdDes AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHas AS DATE       NO-UNDO.

  DEFINE BUFFER buCon FOR contratos.
  DEFINE BUFFER buICo FOR items_contratos.
  DEFINE BUFFER buOrE FOR orden_entrega.
  DEFINE BUFFER buIOe FOR items_orden_entrega.
  DEFINE BUFFER buTSt FOR tambores_industria.
  DEFINE BUFFER buTCo FOR tambores_industria.
  DEFINE BUFFER buTOe FOR tambores_industria.

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmpC AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSucC AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTipC AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNroC AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmpO AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSucO AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTipO AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNroO AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAni  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iIte  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFlg  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lQst  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cCon  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE p     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q     AS DECIMAL    NO-UNDO.

  RUN deleteClienteDespachos.
  
  FOR EACH ttEstadosContrato.
    DELETE ttEstadosContrato.    
  END.
  FOR EACH ttContratos.
    DELETE ttContratos.
  END.
  FOR EACH ttTamboresContrato.
    DELETE ttTamboresContrato.
  END.
  FOR EACH ttTamboresOE.
    DELETE ttTamboresOE.
  END.
  FOR EACH ttContratosFecha.
    DELETE ttContratosFecha.
  END.

  MESSAGE "Desea Excluir Contratos Cumplidos?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lQst.

  /*busco los contratos cuyas partes entren en el rango de fechas parametro*/

  FOR EACH items_contratos
      WHERE items_contratos.fecha >= pdDes
        AND items_contratos.fecha <= pdHas
        AND items_contratos.ITEM  <> 0
        AND (IF piArt <> 0 THEN items_contratos.id_articulo = piArt ELSE TRUE)
       NO-LOCK,
      FIRST contratos 
         OF items_contratos 
      WHERE contratos.id_tipo_contrato  <> 100 /*contratos de fruta*/
        AND contratos.orden_fabricacion <> 0
        AND contratos.anio              <> 0  
        AND contratos.anio              >= YEAR(pdDes)
        AND (IF piCli <> 0 THEN contratos.id_cliente = piCli ELSE TRUE) 
      NO-LOCK.
    
    iIte = items_contratos.ITEM.
    CREATE ttContratosFecha.
    BUFFER-COPY items_contratos EXCEPT ITEM TO ttContratosFecha.
    ASSIGN ttContratosFecha.ITEM_contrato = items_contratos.ITEM.
    
  END.

               
  FOR EACH ttContratosFecha BREAK BY ttContratosFecha.id_contrato .   
    IF LAST-OF(ttContratosFecha.id_contrato) THEN DO:
    
    /*calculo tambores asociados solo al contrato, es decir, producidos para el cliente pero no despachados*/    
    FOR EACH buTSt WHERE buTSt.id_contrato        = ttContratosFecha.id_contrato
                     AND buTSt.anio_of            = ttContratosFecha.anio
                     AND buTSt.id_tipocontrato_of = ttContratosFecha.id_tipo_contrato
                     AND buTSt.ITEM_of            = 0
                   BREAK BY buTSt.nromov.
      i = i + 1.
      p = p + buTSt.kilos_tambor.
      
      IF LAST-OF(buTSt.nromov) THEN DO:
        
        CREATE ttTamboresContrato.
        ASSIGN ttTamboresContrato.id_contrato      = buTSt.id_contrato
               ttTamboresContrato.ITEM_contrato    = buTSt.ITEM_of
               ttTamboresContrato.id_tipo_cont     = buTSt.id_tipocontrato_of
               ttTamboresContrato.anio             = buTSt.anio_of
               ttTamboresContrato.nromov           = buTSt.nromov
               ttTamboresContrato.tambores         = i
               ttTamboresContrato.tambores_pedidos = 0
               ttTamboresContrato.kilos            = p.
        
        i = 0.
        p = 0.
      END.
    END.

    /*recorro las partes de contrato*/
    FOR EACH buICo WHERE buICo.id_contrato      = ttContratosFecha.id_contrato
                     AND buICo.id_tipo_contrato = ttContratosFecha.id_tipo_contrato 
                     AND buICo.anio             = ttContratosFecha.anio
                     AND buICo.fecha           >= pdDes
                     AND buICo.fecha           <= pdHas
                   NO-LOCK.
      ASSIGN cCon = ttContratosFecha.id_contrato
             iIte = buICo.ITEM
             iTip = buICo.id_tipo_contrato
             iAni = ttContratosFecha.anio
             iCan = buICo.cantidad.
  
      
      CREATE ttContratos.
      ASSIGN ttContratos.id_contrato      = cCon
             ttContratos.id_tipo_contrato = iTip
             ttContratos.anio             = iAni
             ttContratos.ITEM_contrato    = iIte
             ttContratos.tambores_pedidos = iCan.
             
      /*calculo tambores asociados a la parte del contrato*/    
      FOR EACH buTCo WHERE buTCo.id_contrato        = buICo.id_contrato
                       AND buTCo.anio_of            = buICo.anio
                       AND buTCo.id_tipocontrato_of = buICo.id_tipo_contrato
                       AND buTCo.ITEM_of            = buICo.ITEM                     
                     BREAK BY buTCo.nromov.
        i = i + 1.
        p = p + buTCo.kilos_tambor.
        
        IF LAST-OF(buTCo.nromov) THEN DO:
          ASSIGN iEmpC = buTCo.id_empresa
                 iSucC = buTCo.id_sucursal
                 iTipC = buTCo.id_tipotambor
                 iNroC = buTCo.nromov.
          
          CREATE ttTamboresContrato.
          ASSIGN ttTamboresContrato.id_contrato      = buTCo.id_contrato
                 ttTamboresContrato.ITEM_contrato    = buTCo.ITEM_of
                 ttTamboresContrato.nromov           = iNroC
                 ttTamboresContrato.tambores         = i
                 ttTamboresContrato.tambores_pedidos = iCan
                 ttTamboresContrato.kilos            = p.
          
          i = 0.
          p = 0.
        END.
      END.
  
  
      /*calculo tambores despachados a partir de las oes asociadas a la parte del contrato*/  
      FOR EACH buIOe WHERE buIOe.id_contrato      = buICo.id_contrato
                       AND buIOe.id_tipo_contrato = buICo.id_tipo_contrato
                       AND buIOe.ITEM             = buICo.ITEM
                     NO-LOCK.      
        FOR EACH buTOe WHERE buTOe.id_orden_entrega = buIOe.id_orden_entrega
                         AND buTOe.ITEM_oe          = buIOe.ITEM_oe
                       BREAK BY buTOe.nromov.
          j = j + 1.            
          q = q + buTOe.kilos_tambor.
          IF LAST-OF(buTOe.nromov) THEN DO:
            ASSIGN iEmpO = buTOe.id_empresa
                   iSucO = buTOe.id_sucursal
                   iTipO = buTOe.id_tipotambor
                   iNroO = buTOe.nromov.                            
  
            CREATE ttTamboresOE.
            ASSIGN ttTamboresOE.id_contrato       = buTOe.id_contrato
                   ttTamboresOE.ITEM_contrato     = buTOe.ITEM_of
                   ttTamboresOE.id_tipo_cont      = buTOe.id_tipocontrato_of
                   ttTamboresOE.anio              = buTOe.anio_of
                   ttTamboresOE.id_orden_entrega  = buTOe.id_orden_entrega
                   ttTamboresOE.ITEM_oe           = buTOe.ITEM_oe
                   ttTamboresOE.nromov            = iNroO
                   ttTamboresOE.tambores          = j
                   ttTamboresOE.tambores_pedidos  = iCan
                   ttTamboresOE.kilos             = q.
  
            j = 0.
            q = 0.
          END.
        END.
      END.
    
    END.
  END. /*last-of ttContratosFecha*/
  END.

  RELEASE buCon.
  RELEASE buICo.
  RELEASE buOrE.
  RELEASE buIOe.
  RELEASE buTCo.
  RELEASE buTOe.


  /*tambores con oe*/
  FOR EACH ttTamboresOE.
    CREATE ttEstadosContrato.
    ASSIGN ttEstadosContrato.id_contrato      = ttTamboresOE.id_contrato
           ttEstadosContrato.ITEM_contrato    = ttTamboresOE.ITEM_contrato
           ttEstadosContrato.id_tipo_contrato = ttTamboresOE.id_tipo_cont
           ttEstadosContrato.anio_contrato    = ttTamboresOE.anio
           ttEstadosContrato.tambores_pedidos = ttTamboresOE.tambores_pedidos
           ttEstadosContrato.id_orden_entrega = ttTamboresOE.id_orden_entrega
           ttEstadosContrato.ITEM_oe          = ttTamboresOE.ITEM_oe
           ttEstadosContrato.nromov_oe        = ttTamboresOE.nromov
           ttEstadosContrato.tambores_oe      = ttTamboresOE.tambores
           ttEstadosContrato.kilos_oe         = ttTamboresOE.kilos.
    /*lotes que tienen oe y contrato*/
    FIND FIRST ttTamboresContrato WHERE ttTamboresContrato.nromov = ttTamboresOE.nromov NO-ERROR.
    IF AVAILABLE ttTamboresContrato THEN DO:    /*van por el else los tambores que tienen contrato y no tienen oe*/
      ASSIGN ttEstadosContrato.nromov_contrato   = ttTamboresContrato.nromov
             ttEstadosContrato.tambores_contrato = ttTamboresContrato.tambores
             ttEstadosContrato.kilos_contrato    = ttTamboresContrato.kilos.
      DELETE ttTamboresContrato.
    END.
  END.

  
  /*contratos que no tienen oe*/
  FOR EACH ttTamboresContrato.
    CREATE ttEstadosContrato.
    ASSIGN ttEstadosContrato.id_contrato       = ttTamboresContrato.id_contrato
           ttEstadosContrato.ITEM_contrato     = ttTamboresContrato.ITEM_contrato
           ttEstadosContrato.id_tipo_contrato  = ttTamboresContrato.id_tipo_cont
           ttEstadosContrato.anio              = ttTamboresContrato.anio
           ttEstadosContrato.tambores_pedidos  = ttTamboresContrato.tambores_pedidos
           ttEstadosContrato.nromov_contrato   = ttTamboresContrato.nromov
           ttEstadosContrato.tambores_contrato = ttTamboresContrato.tambores
           ttEstadosContrato.kilos_contrato    = ttTamboresContrato.kilos.
  END.

  
  /*contratos que no tienen lotes ni oes asignados*/
  FOR EACH ttContratos.
    FIND FIRST ttEstadosContrato WHERE ttEstadosContrato.id_contrato   = ttContratos.id_contrato
                                   AND ttEstadosContrato.ITEM_contrato = ttContratos.ITEM_contrato
                                 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttEstadosContrato THEN DO:
      CREATE ttEstadosContrato.
      ASSIGN ttEstadosContrato.id_contrato      = ttContratos.id_contrato
             ttEstadosContrato.ITEM_contrato    = ttContratos.ITEM_contrato
             ttEstadosContrato.id_tipo_contrato = ttContratos.id_tipo_contrato
             ttEstadosContrato.anio             = ttContratos.anio
             ttEstadosContrato.tambores_pedidos = ttContratos.tambores_pedidos.
    END.    
  END.

  
  /*lleno tabla reporteadora*/
  FOR EACH ttEstadosContrato .

    RUN addClienteDespachos (ttEstadosContrato.id_contrato,
                             ttEstadosContrato.anio,
                             ttEstadosContrato.id_tipo_contrato,
                             ttEstadosContrato.ITEM_contrato,
                             ttEstadosContrato.tambores_pedidos,
                             ttEstadosContrato.id_orden_entrega,
                             ttEstadosContrato.ITEM_oe,
                             ttEstadosContrato.nromov_contrato,
                             ttEstadosContrato.tambores_contrato,
                             ttEstadosContrato.kilos_contrato,
                             ttEstadosContrato.nromov_oe,
                             ttEstadosContrato.tambores_oe,
                             ttEstadosContrato.kilos_oe).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callEstadosTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callEstadosTambor Procedure 
PROCEDURE callEstadosTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.

  RUN deleteStockTambores.

  RUN fillStockTamboresReproc(piSucUbi, 2). /*mp p. reproceso*/
  RUN fillStockTamboresReproc(piSucUbi, 3). /*jugo en reproceso*/
  RUN fillStockTamboresReproc(piSucUbi, 4). /*lote jugo terminado*/
  RUN fillStockTamboresReproc(piSucUbi, 5). /*produccion*/
  RUN fillStockTamboresReproc(piSucUbi, 7). /*tambor vacio etiquetado*/
  RUN fillStockTamboresReproc(piSucUbi, 8). /*reprocesado*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callMatPrimaAceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callMatPrimaAceite Procedure 
PROCEDURE callMatPrimaAceite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.

/*  
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 6, 27).*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 44).             /*aceite de naranja mandarina*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 48).             /*produccion water phase*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 50). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 73). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 76). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 411).            /*produccin aceite naranja*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 501). 
  RUN fillStockTamboresOilProdxLote(piSucUbi, 510).              /*prod aceite ralladora*/
  
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 509).            /*produccion aceite ralladora agrupada por articulo*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 511).            /*produccion aceite*/
  
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 513).            /*borra limpia*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 514).            /*borra para limpiar*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 516). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 0, 518). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 0, 519). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 0, 520). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 571).            /*produccion oil phase*/
  RUN fillStockTamboresOilProdxLote(piSucUbi, 578).              /*sob oil phase*/
  RUN fillStockTamboresOilProdxLote(piSucUbi, 588).              /*sob water phase*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 581).            /*water phase*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 582).            /*water phase concentrated*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 595).
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 616).            /*borra limpia*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 617).            /*borra para limpiar*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 681).
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 742).
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 742). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 743). 
  RUN fillStockTamboresOilProdxLote(piSucUbi, 744).              /*sob terpeno*/
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 2, 762). 
  RUN fillStockTamboresMatPrimaOil(piSucUbi, 8, 763). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callProduccionIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callProduccionIndustria Procedure 
PROCEDURE callProduccionIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pcTipo   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plRepro  AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER plLotPro AS LOGICAL    NO-UNDO.

  RUN deleteProduccionIndustria.

  IF pcTipo = "Lotes" THEN DO:
    /* lotes de jugo */
    RUN fillProduccionIndustria(piSuc, 
                                3, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).
  
    /* lotes de aciete */
    RUN fillProduccionIndustria(piSuc, 
                                6, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).
  
    RUN fillProduccionIndustria(piSuc, 
                                7, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).
  
    RUN fillProduccionIndustria(piSuc, 
                                11, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).
  END.

  IF pcTipo = "Producciones" THEN DO:
    RUN fillProduccionIndustria(piSuc, 
                                1, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).

    RUN fillProduccionIndustria(piSuc, 
                                2, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).

    RUN fillProduccionIndustria(piSuc, 
                                4, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).

     RUN fillProduccionIndustria(piSuc, 
                                5, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).

    RUN fillProduccionIndustria(piSuc, 
                                12, 
                                piArt, 
                                pdDesde, 
                                pdHasta,
                                plRepro,
                                plLotPro).
  END.

  IF pcTipo = "ProdExcel" THEN DO:
      
    RUN fillProduccionesIndustria(piSuc, 
                                  1, 
                                  piArt, 
                                  pdDesde, 
                                  pdHasta).

    RUN fillProduccionesIndustria(piSuc, 
                                  2, 
                                  piArt, 
                                  pdDesde, 
                                  pdHasta).
    
    RUN fillProduccionesIndustria(piSuc, 
                                  4, 
                                  piArt, 
                                  pdDesde, 
                                  pdHasta).

    RUN fillProduccionesIndustria(piSuc, 
                                  5, 
                                  piArt, 
                                  pdDesde, 
                                  pdHasta).
 
    RUN fillProduccionesIndustria(piSuc, 
                                  12, 
                                  piArt, 
                                  pdDesde, 
                                  pdHasta).

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteCamara Procedure 
PROCEDURE callReporteCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  RUN deleteTamboresCamara.


  FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion =  piSuc
                                AND tambores_industria.id_camara             <> 0
                                AND (IF piNro <> 0 THEN tambores_industria.nromov = piNro ELSE TRUE)
                              NO-LOCK.
    
    RUN addTamboresCamara(tambores_industria.id_empresa, 
                          tambores_industria.id_sucursal, 
                          tambores_industria.id_tipotambor,
                          tambores_industria.nromov,
                          tambores_industria.id_tambor,
                          tambores_industria.id_lote,
                          tambores_industria.anio,
                          tambores_industria.id_articulo,
                          tambores_industria.id_calidad,
                          tambores_industria.id_envase,
                          tambores_industria.id_camara,
                          tambores_industria.nro_fila,
                          tambores_industria.nro_columna).
    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteDespachos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteDespachos Procedure 
PROCEDURE callReporteDespachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde     AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta     AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLugDes    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCalidad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal  AS INTEGER    NO-UNDO.

  FOR EACH despachos_industria.
    DELETE despachos_industria.    
  END.


  FOR EACH remitos NO-LOCK WHERE remitos.fecha >= pdDesde 
                             AND remitos.fecha <= pdHasta
                             AND remitos.estado = TRUE
                             AND (remitos.id_sucursal = 95 OR remitos.id_sucursal = 96)
                             AND (IF piLugDes   <> 0 THEN remitos.id_lugdes = piLugDes     ELSE TRUE)
                             AND (IF piSucursal <> 0 THEN remitos.id_sucursal = piSucursal ELSE TRUE).
    FOR EACH items_factura OF remitos NO-LOCK WHERE items_factura.id_tipotambor <> 11
                                                AND items_factura.id_tipotambor <> 0 
                                                AND (IF piArticulo <> 0 THEN items_factura.id_articulo = piArticulo ELSE TRUE)
                                                AND (IF piCalidad  <> 0 THEN items_factura.id_calidad = piCalidad   ELSE TRUE).
      RUN addDespachoIndustria(ROWID(items_factura)).      
    END.

    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteEstadosTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteEstadosTambores Procedure 
PROCEDURE callReporteEstadosTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTam FOR tambores_industria.

  RUN deleteStockTambores.
  
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 2 /*mp/reproceso*/
        AND buTam.id_locacion_ubicacion = 4
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 3 /*producto en proceso*/
        AND buTam.id_tipotambor        <> 2 
        AND buTam.id_tipotambor        <> 7
        AND buTam.id_tipotambor        <> 8 
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 4 /*lotes terminados*/
        AND buTam.id_locacion_ubicacion = 4
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 5 /*producciones terminadas*/
        AND buTam.id_locacion_ubicacion = 4
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 6 /*producciones terminadas*/
        AND buTam.id_locacion_ubicacion = 4
        AND buTam.id_tipotambor         = 6
        AND buTam.nromov_destino        = 0
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 7 /*tambores vacios etiquetados*/
        AND buTam.id_locacion_ubicacion = 4
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.

  i = 0.
  k = 0.
  FOR EACH buTam
      WHERE buTam.id_sucursal_ubicacion = piSucUbi
        AND buTam.id_estado             = 11 /*apto para despacho*/
        AND buTam.id_locacion_ubicacion = 4
      BREAK BY buTam.nromov.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    IF LAST-OF(buTam.nromov) THEN DO:
      RUN addStockTambores(ROWID(buTam), i, k).
      i = 0.
      k = 0.
    END.    
  END.


    
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteMapaCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteMapaCamara Procedure 
PROCEDURE callReporteMapaCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cCam AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  FOR EACH ttMapaCamara.
    DELETE ttMapaCamara.    
  END.

  FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion  = piSuc
                                AND tambores_industria.id_sucursal_camara     = piSuc 
                                AND tambores_industria.id_camara              = piCam
                                AND tambores_industria.fecha                 >= DATE("01/01/2005")
                              BREAK BY tambores_industria.nro_fila_camara BY tambores_industria.nro_columna_camara.
    IF LAST-OF(tambores_industria.nro_columna_camara) THEN DO:
      cCam = getTamboresCuadrante(piSuc, 
                                  piCam, 
                                  piArt, 
                                  tambores_industria.nro_fila_camara, 
                                  tambores_industria.nro_columna_camara).
      /*
      DO i = 1 TO NUM-ENTRIES(cCam, CHR(10)):
        cRow = ENTRY(i, cCam, CHR(10)).
        CREATE ttMapaCamara.
        ASSIGN ttMapaCamara.camara    = "Camara # " + STRING(piCam)
               ttMapaCamara.fila      = tambores_industria.nro_fila_camara
               ttMapaCamara.columna   = tambores_industria.nro_columna_camara
               ttMapaCamara.id_lote   = INTEGER(ENTRY(2, cRow, CHR(1)))
               ttMapaCamara.anio      = INTEGER(ENTRY(3, cRow, CHR(1)))
               ttMapaCamara.articulo  = ENTRY(4, cRow, CHR(1))
               ttMapaCamara.tbs       = INTEGER(ENTRY(5, cRow, CHR(1))).
      END.
      */

    END.
    
  END.

  RUN generateExcel.p (INPUT TABLE ttMapaCamara,
                       INPUT " Mapa de Camara ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteMPJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteMPJugo Procedure 
PROCEDURE callReporteMPJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi  AS INTEGER    NO-UNDO.

  RUN deleteStockTambores.
  RUN fillttOrdenReporte.
  
  RUN fillStockTamboresNoOf (piSucUbi, 1).  /*produccion jugo*/
  RUN fillStockTamboresReproc(piSucUbi, 3, 2). /*reprocesos de jugo mp p/reproceso*/  

  RUN fillStockTamboresReproc(piSucUbi, 4, 2). /*reprocesos de sobrante de jugo mp p/reproceso*/  
  RUN fillStockTamboresReproc(piSucUbi, 5, 2). /*reprocesos de arrastre de jugo mp p/reproceso*/  




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteOEIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteOEIndustria Procedure 
PROCEDURE callReporteOEIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.


  DEFINE VAR v_flete AS DECIMAL.
  DEFINE VAR v_seguro AS DECIMAL.
  DEFINE VAR v_vs_ddp AS DECIMAL.
  DEFINE VAR v_entri AS DECIMAL.
  DEFINE VAR i AS INTEGER.
  DEFINE BUFFER bbItems FOR items_orden_entrega.
  DEFINE VAR vMarcas AS CHAR.

  DEFINE VARIABLE cGas AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fPor AS DECIMAL    NO-UNDO.

  crlf = CHR(10).
  
  FOR EACH rep_oe_fax.
      FOR EACH item_rep_oe_fax.
          DELETE ITEM_rep_oe_fax.
      END.
      DELETE rep_oe_fax.
  END.
  i = 0.
  vMarcas = "".
  
  FIND FIRST orden_entrega 
       WHERE orden_entrega.id_orden_entrega = piOE
       NO-LOCK NO-ERROR.

  IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST agencias     WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
    FIND FIRST tipos_plazo  OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST destinos     OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = orden_entrega.id_lugdes NO-LOCK NO-ERROR.
    
    CREATE rep_oe_fax.
    ASSIGN rep_oe_fax.id_orden_entrega  = orden_entrega.id_orden_entrega
           rep_oe_fax.semana            = orden_entrega.semana_embarque
           rep_oe_fax.id_agencia        = orden_entrega.id_agencia
           rep_oe_fax.agencia           = IF AVAILABLE agencias THEN agencias.descripcion ELSE "NONE"
           rep_oe_fax.plazo             = orden_entrega.plazo
           rep_oe_fax.id_tipo_plazo     = orden_entrega.id_tipo_plazo
           rep_oe_fax.tipo_plazo        = IF AVAILABLE tipos_plazo THEN tipos_plazo.descripcion ELSE "NONE"
           rep_oe_fax.fecha_embarque    = orden_entrega.fecha_embarque
           rep_oe_fax.id_vapor          = orden_entrega.id_vapor
           rep_oe_fax.vapor             = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NONE"
           rep_oe_fax.id_destino        = orden_entrega.id_destino
           rep_oe_fax.destino           = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
           rep_oe_fax.id_despachante    = orden_entrega.id_despachante
           rep_oe_fax.despachante       = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
           rep_oe_fax.id_lugar_descarga = orden_entrega.id_lugdes
           rep_oe_fax.lugar_descarga    = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "NONE"
           rep_oe_fax.observaciones     = orden_entrega.observaciones
           .
   
    FOR EACH gastos_orden_entrega  
          OF orden_entrega,
        FIRST gastos_venta OF gastos_orden_entrega                  
        WHERE gastos_orden_entrega.importe <> 0
        NO-LOCK.
      dTot = dTot + gastos_orden_entrega.importe.
      cGas = cGas + 
             STRING(gastos_orden_entrega.id_gasto, "99") + ".- " + 
             gastos_venta.descripcion + ": $" + 
             STRING(gastos_orden_entrega.importe, ">>>,>>9.99") + crlf.
      
    END.

    rep_oe_fax.gastos_oe = SUBSTRING(cGas, 1, LENGTH(cGas) - LENGTH(crlf)).
    rep_oe_fax.TOTAL_gastos_oe = dTot.


    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
        
      
      FIND FIRST clausulas WHERE clausulas.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF items_orden_entrega NO-LOCK NO-ERROR.

      IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
          FIND FIRST variedades WHERE variedades.id_variedad = items_orden_entrega.id_calidad NO-LOCK NO-ERROR.
      END.
      ELSE DO:
          FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
      END.

      IF items_orden_entrega.id_articulo = 54 THEN DO:
        FIND FIRST envases_prod
             WHERE envases_prod.id_envase = 14
             NO-LOCK NO-ERROR.


      END.
      ELSE DO:
        FOR FIRST tambores_industria
              OF items_orden_entrega
            NO-LOCK.
          FIND FIRST envases_prod
                  OF tambores_industria 
                NO-LOCK NO-ERROR.
        END.

      END.

      FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST marcas_prod OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST tipo_moneda OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST instruc_pago 
           WHERE instruc_pago.id_instruc_pago = contratos.id_payment_instruction
           NO-LOCK NO-ERROR.
      FIND FIRST bancos 
           WHERE bancos.id_banco = contratos.id_payment_instruction
           NO-LOCK NO-ERROR.
      
       
      
      /*bancos */
      ASSIGN rep_oe_fax.id_banco    = contratos.id_payment_instruction
             rep_oe_fax.banco       = getBancoOE(items_orden_entrega.id_cliente)
             rep_oe_fax.datos_banco = ""
             .

                
      IF i = 0 THEN DO:
          FIND FIRST contactos_industria WHERE contactos_industria.id_contacto = contratos.id_notify
                                          NO-LOCK NO-ERROR.
          IF AVAILABLE contactos_industria THEN DO:
              /* OBTENGO LOS DATOS DEL NOTIFY */
              ASSIGN rep_oe_fax.id_contacto_notify     = contactos_industria.id_contacto
                     rep_oe_fax.nombre_notify          = contactos_industria.nombre
                     rep_oe_fax.direccion_notify       = contactos_industria.direccion
                     rep_oe_fax.localidad_notify       = contactos_industria.localidad
                     rep_oe_fax.provincia_notify       = contactos_industria.provincia
                     rep_oe_fax.codigo_postal_notify   = contactos_industria.codigo_postal
                     rep_oe_fax.pais_notify            = contactos_industria.pais
                     rep_oe_fax.contacto_notify        = contactos_industria.contacto
                     rep_oe_fax.tel_notify             = contactos_industria.tel
                     rep_oe_fax.fax_notify             = contactos_industria.fax.                    
          END.
          FIND FIRST contactos_industria WHERE contactos_industria.id_contacto = contratos.id_consignee
                                          NO-LOCK NO-ERROR.
          IF AVAILABLE contactos_industria THEN DO:
              /* OBTENGO LOS DATOS DEL CONSIGNEE */
              ASSIGN rep_oe_fax.id_contacto_consignee     = contactos_industria.id_contacto
                     rep_oe_fax.nombre_consignee          = contactos_industria.nombre
                     rep_oe_fax.direccion_consignee       = contactos_industria.direccion
                     rep_oe_fax.localidad_consignee       = contactos_industria.localidad
                     rep_oe_fax.provincia_consignee       = contactos_industria.provincia
                     rep_oe_fax.codigo_postal_consignee   = contactos_industria.codigo_postal
                     rep_oe_fax.pais_consignee            = contactos_industria.pais
                     rep_oe_fax.contacto_consignee        = contactos_industria.contacto
                     rep_oe_fax.tel_consignee             = contactos_industria.tel
                     rep_oe_fax.fax_consignee             = contactos_industria.fax.
          END.
          FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
          IF AVAILABLE items_contratos THEN DO:
              FIND FIRST destinos WHERE destinos.id_destino = items_contratos.destino_final NO-LOCK NO-ERROR.

              ASSIGN rep_oe_fax.id_po_cliente         = items_contratos.id_po_cliente[1]
                     rep_oe_fax.id_product_cliente    = items_contratos.id_articulo_cliente[1]
                     rep_oe_fax.id_release_cliente    = items_contratos.numero_release[1]
                     rep_oe_fax.id_destino_final      = items_contratos.destino_final
                     rep_oe_fax.destino_final         = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
                     .
          END.
          i = 1.
      END.
      FIND FIRST tipos_plazo OF contratos NO-LOCK NO-ERROR.
      FIND FIRST instrumentos_pagos 
           WHERE instrumentos_pagos.id_instrumento_pago = contratos.id_instrumento_pago 
           NO-LOCK NO-ERROR.
      FIND FIRST tipo_unidad_venta OF items_contratos NO-LOCK NO-ERROR.
      
      FIND FIRST porcentaje_reint_articulo 
           WHERE porcentaje_reint_articulo.id_articulo = items_orden_entrega.id_articulo
           NO-LOCK NO-ERROR.
      /*condicion venta contratos*/
      FIND FIRST tipo_venta WHERE tipo_venta.id_tipo_venta = items_contratos.id_tipo_venta NO-LOCK NO-ERROR.


      CREATE ITEM_rep_oe_fax.
      ASSIGN ITEM_rep_oe_fax.id_orden_entrega     = items_orden_entrega.id_orden_entrega
             ITEM_rep_oe_fax.ITEM_oe              = items_orden_entrega.ITEM_oe
             ITEM_rep_oe_fax.id_cliente           = items_orden_entrega.id_cliente
             ITEM_rep_oe_fax.cliente              = IF AVAILABLE clientes THEN clientes.nombre ELSE "NONE"
             ITEM_rep_oe_fax.id_contrato          = items_orden_entrega.id_contrato
             ITEM_rep_oe_fax.id_tipo_contrato     = items_orden_entrega.id_tipo_contrato
             ITEM_rep_oe_fax.anio                 = items_orden_entrega.anio
             ITEM_rep_oe_fax.ITEM                 = items_orden_entrega.ITEM
             ITEM_rep_oe_fax.id_tipo_unidad_venta = items_contratos.id_tipo_unidad_venta
             ITEM_rep_oe_fax.tipo_unidad_venta    = tipo_unidad_venta.descripcion

             .

      IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
        ASSIGN ITEM_rep_oe_fax.calidad             = IF AVAILABLE variedades THEN variedades.abreviatura ELSE "NONE"
               ITEM_rep_oe_fax.id_calidad          = items_orden_entrega.id_calidad.
      END.
      ELSE DO:
        ASSIGN ITEM_rep_oe_fax.calidad             = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
               ITEM_rep_oe_fax.id_calidad          = items_orden_entrega.id_calidad.
      END.
      
      ASSIGN ITEM_rep_oe_fax.id_articulo         = items_orden_entrega.id_articulo
             ITEM_rep_oe_fax.articulo            = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
             ITEM_rep_oe_fax.id_envase           = items_orden_entrega.id_envase
             ITEM_rep_oe_fax.envase              = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
             ITEM_rep_oe_fax.tambores            = items_orden_entrega.cantidad_tambores
             ITEM_rep_oe_fax.kilos_netos         = items_orden_entrega.kgs_netos_tambores
             ITEM_rep_oe_fax.kilos_brutos        = items_orden_entrega.kgs_brutos_tambores
             ITEM_rep_oe_fax.id_clausula         = tipo_venta.id_tipo_venta
             ITEM_rep_oe_fax.clausula            = tipo_venta.descripcion 
             rep_oe_fax.id_incotrem              = items_orden_entrega.id_condicion_venta
             rep_oe_fax.incoterm                 = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "NONE".
      
      IF orden_entrega.id_tipo_orden_entrega = 1 THEN DO:
          ASSIGN ITEM_rep_oe_fax.id_marca            = items_orden_entrega.id_marca
                 ITEM_rep_oe_fax.marca               = IF AVAILABLE marcas_prod THEN marcas_prod.abreviatura ELSE "NONE".
      END.
      ELSE DO:
          FOR EACH bbItems OF orden_entrega NO-LOCK
              BREAK BY bbItems.id_marca.
              IF LAST-OF(bbItems.id_marca) THEN DO:
                  FIND FIRST marcas_prod OF bbItems NO-LOCK NO-ERROR.
                  IF AVAILABLE marcas_prod THEN DO:
                      vMarcas = vMarcas + "/" + TRIM(marcas_prod.descripcion).
                  END.
              END.

          END.
          ASSIGN ITEM_rep_oe_fax.id_marca            = items_orden_entrega.id_marca
                 ITEM_rep_oe_fax.marca               = vMarcas.
      END.
          
      IF items_orden_entrega.id_tipo_venta = 3 THEN DO:
        ASSIGN ITEM_rep_oe_fax.clausula            = "Consignacion".
      END.

      IF orden_entrega.id_tipo_orden_entrega = 1 THEN DO:       
        fCnt = IF items_orden_entrega.contenedores < 1 THEN items_orden_entrega.contenedores ELSE 1.
        ASSIGN ITEM_rep_oe_fax.id_tipo_unidad_venta  = items_contratos.id_tipo_unidad_venta
               ITEM_rep_oe_fax.precio_unitario_ton   = items_orden_entrega.fob_unitario.
        /*        
        CASE items_orden_entrega.id_tipo_unidad_venta:
          WHEN 1 THEN /* TONELADAS */ DO:
            ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / (items_orden_entrega.kgs_netos_tambores / 1000).                
          END.
          WHEN 2 THEN /* KILOS */ DO:
            ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / items_orden_entrega.kgs_netos_tambores.
          END.
          WHEN 3 THEN /* GALONES PERO ES IGUAL QUE TONELADAS */ DO:
            ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / (items_orden_entrega.kgs_netos_tambores / 1000).
          END.
          WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
            ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / (items_orden_entrega.kgs_netos_tambores / 1000).
          END.
          
        END CASE.
        */
          
      END.

          ELSE DO:
              /* ACA ENTRA CUANDO ES UNA OE DE FRUTA FRESCA */
              ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.precio_x_caja.
          END.

     fGas = 0.
     FOR EACH gastos_items_orden_entrega
           OF items_orden_entrega
         NO-LOCK.
       fGas = fGas + gastos_items_orden_entrega.importe.    
     END.

     
     ASSIGN ITEM_rep_oe_fax.TOTAL_fob               = items_orden_entrega.fob_ton
            ITEM_rep_oe_fax.seguro                  = v_seguro
            ITEM_rep_oe_fax.vs_ddp                  = v_vs_ddp
            ITEM_rep_oe_fax.importe_factura         = items_orden_entrega.total_factura
            ITEM_rep_oe_fax.precio_x_galon          = items_orden_entrega.total_factura / items_orden_entrega.TOTAL_galones
            ITEM_rep_oe_fax.TOTAL_galones           = items_orden_entrega.TOTAL_galones
            ITEM_rep_oe_fax.fob_ton                 = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.fob_unitario ELSE ROUND(items_orden_entrega.fob_unitario,0)
            ITEM_rep_oe_fax.coeficiente             = items_orden_entrega.coeficiente
            ITEM_rep_oe_fax.valor_aduana_derecho    = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.valor_aduana_derecho ELSE 0
            ITEM_rep_oe_fax.valor_aduana_reintegro  = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.valor_aduana_reintegro ELSE 0
            ITEM_rep_oe_fax.derecho                 = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.importe_derecho ELSE 0
            ITEM_rep_oe_fax.reintegro               = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.importe_reintegro ELSE 0
            ITEM_rep_oe_fax.comision                = items_orden_entrega.importe_comision
            ITEM_rep_oe_fax.cert_fito               = items_orden_entrega.cert_fito
            ITEM_rep_oe_fax.plazo                   = IF AVAILABLE contratos THEN contratos.plazo ELSE items_orden_entrega.plazo
            ITEM_rep_oe_fax.id_tipo_plazo           = IF AVAILABLE contratos THEN contratos.id_tipo_plazo ELSE items_orden_entrega.id_tipo_plazo
            ITEM_rep_oe_fax.tipo_plazo              = IF AVAILABLE tipos_plazo THEN tipos_plazo.descripcion ELSE "NONE"
            ITEM_rep_oe_fax.id_instrumento_pago     = IF AVAILABLE contratos THEN contratos.id_instrumento_pago ELSE items_orden_entrega.id_instrumento_pago
            ITEM_rep_oe_fax.instrumento_pago        = IF AVAILABLE instrumentos_pagos THEN instrumentos_pagos.descripcion ELSE "NONE"            
            ITEM_rep_oe_fax.pallets                 = items_orden_entrega.cantidad_pallets
            ITEM_rep_oe_fax.cajas_x_pallets         = items_orden_entrega.cajas_x_pallets
            ITEM_rep_oe_fax.x_kilos                 = items_orden_entrega.x_kilos
            item_rep_oe_fax.moneda_cambio           = IF AVAILABLE tipo_moneda THEN tipo_moneda.descripcion ELSE "NONE"
            ITEM_rep_oe_fax.entri                   = items_orden_entrega.importe_origen
            ITEM_rep_oe_fax.flete                   = fGas /*feo pero acumulo aqui los gastos del item_oe*/
            ITEM_rep_oe_fax.porc_reintegro          = IF AVAILABLE porcentaje_reint_articulo THEN porcentaje_reint_articulo.porcentaje ELSE 5
            .
  

    END.

  END. /*if available orden_entrega*/

  RELEASE rep_oe_fax.
  RELEASE ITEM_rep_oe_fax.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteOEsEmbarcadas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteOEsEmbarcadas Procedure 
PROCEDURE callReporteOEsEmbarcadas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT  PARAMETER piSemDes  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnioDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSemHas  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnioHas AS INTEGER    NO-UNDO.

  DEFINE BUFFER buIoe FOR items_orden_entrega.

  FOR EACH ttOes.
    DELETE ttOes.
  END.


  FOR EACH orden_entrega WHERE orden_entrega.semana_embarque >= piSemDes
                           AND orden_entrega.anio            >= piAnioDes
                           AND orden_entrega.semana_embarque <= piSemHas
                           AND orden_entrega.anio            <= piAnioHas
                         NO-LOCK, 
      EACH buIoe OF orden_entrega NO-LOCK .

    FIND FIRST contratos OF buIoe NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
    FIND FIRST vapores OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST items_contratos OF buIoe NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF buIoe NO-LOCK NO-ERROR.
    FIND FIRST calidades OF buIoe NO-LOCK NO-ERROR.
    FIND FIRST packing_list OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST estados_oe OF buIoe NO-LOCK NO-ERROR.

    CREATE ttOes.
    ASSIGN ttOes.id_orden_entrega = buIoe.id_orden_entrega
           ttOes.ITEM_oe          = buIoe.ITEM
           ttOes.semana           = buIoe.semana_entrega
           ttOes.vapor            = IF AVAILABLE vapores THEN vapores.descripcion ELSE "SIN VAPOR"
           ttOes.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-DESC"
           ttOes.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-CALIDAD"
           ttOes.cliente          = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NO-CLIENTE"
           ttOes.tambores         = buIoe.tambores_pedidos
           ttOes.nro_of           = contratos.orden_fabricacion
           ttOes.estado           = IF AVAILABLE estados_oe THEN estados_oe.descripcion ELSE "NO-INFO"
           ttOes.fecha_embarque   = IF AVAILABLE packing_list THEN packing_list.fecha_salida_vapor ELSE ?.

  END.

  RUN generateExcel.p (INPUT TABLE ttOes,
                       INPUT " Exportacion de Oes Embarcadas",
                       INPUT " "  ,
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteProceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteProceso Procedure 
PROCEDURE callReporteProceso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLot  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTam  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNat  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCon  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cH2O  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDes AS CHARACTER  NO-UNDO.

  crlf = CHR(13) + CHR(10).

  i = 0.
  k = 0.
  /*obtengo tambores proceso*/
  FOR EACH r_proceso_tambor WHERE r_proceso_tambor.id_empresa_proceso    = piEmp
                              AND r_proceso_tambor.id_sucursal_proceso   = piSuc
                              AND r_proceso_tambor.id_tipotambor_proceso = piTip
                              AND r_proceso_tambor.nromov_proceso        = piNro
                           BREAK BY r_proceso_tambor.nromov.
    IF FIRST-OF(r_proceso_tambor.nromov) THEN DO:
      FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = r_proceso_tambor.id_empresa
                                     AND tambores_industria.id_sucursal   = r_proceso_tambor.id_sucursal
                                     AND tambores_industria.id_tipotambor = r_proceso_tambor.id_tipotambor
                                     AND tambores_industria.nromov        = r_proceso_tambor.nromov
                                     AND tambores_industria.id_tambor     = r_proceso_tambor.id_tambor
                                   NO-LOCK.
        k = tambores_industria.kilos_tambor.
      END.
    END.

    i = i + 1.
    IF LAST-OF(r_proceso_tambor.nromov) THEN DO:
      FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = r_proceso_tambor.id_empresa
                                     AND tambores_industria.id_sucursal   = r_proceso_tambor.id_sucursal
                                     AND tambores_industria.id_tipotambor = r_proceso_tambor.id_tipotambor
                                     AND tambores_industria.nromov        = r_proceso_tambor.nromov
                                     AND tambores_industria.id_tambor     = r_proceso_tambor.id_tambor
                                   NO-LOCK.
        
        k = k * (i - 1) + tambores_industria.kilos_tambor.
        cLot = cLot + STRING(tambores_industria.id_lote) + "/" + 
                      STRING(tambores_industria.anio) + " " + 
                      getDescArticulo(tambores_industria.id_articulo) + " " + 
                      getDescCalidad(tambores_industria.id_calidad) + " por " + 
                      STRING(i) + " tambores Kgs: " + 
                      STRING(k) + crlf.                      
      END.
      i = 0 .
      k = 0.
    END.
  END.
  cLot = SUBSTRING(cLot, 1, LENGTH(cLot) - LENGTH(crlf)).

  FOR EACH tambores_proceso.
    DELETE tambores_proceso.
  END.

  CREATE tambores_proceso.
  ASSIGN tambores_proceso.id_empresa_proceso    = piEmp
         tambores_proceso.id_sucursal_proceso   = piSuc
         tambores_proceso.id_tipotambor_proceso = piTip
         tambores_proceso.nromov_proceso        = piNro
         tambores_proceso.tambores_proc         = cLot.

  
  /*obtengo datos de las cargas*/
  FOR EACH datos_carga.
    DELETE datos_carga.
  END.

  FOR EACH cargas WHERE cargas.id_empresa_proceso     = piEmp
                    AND cargas.id_sucursal_proceso    = piSuc
                    AND cargas.id_tipotambor_proceso  = piTip
                    AND cargas.nromov_proceso         = piNro
                  NO-LOCK.
    i = 0.
    k = 0.
    cTam = "".
    FOR EACH tambores_industria WHERE nromov_destino = cargas.nromov
                                BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.      
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        /*cTam = cTam + STRING(tambores_industria.id_lote) + "/" + 
                      STRING(tambores_industria.anio) + " " + 
                      getDescArticulo(tambores_industria.id_articulo) + " " + 
                      getDescCalidad(tambores_industria.id_calidad) + " por " + 
                      STRING(i) + " tambores " + 
                      STRING(k) + " kgs" + crlf.*/
        cTam = cTam + STRING(tambores_industria.id_lote) + "/" + 
                      STRING(tambores_industria.anio) + " " + 
                      STRING(i) + " tbs " + 
                      STRING(k) + " kgs " + 
                      getDescCalidad(tambores_industria.id_calidad) + crlf.
        i = 0.
        k = 0.
      END.      
    END.
    /*cTam = SUBSTRING(cTam, 1, LENGTH(cTam) - 2).*/


    /*calculo litos de jugo natural*/
    cNat = "".
    cDes = "".
    cH2O = "".
    cCon = "".
    FOR EACH composicion_carga WHERE composicion_carga.nromov      = cargas.nromov
                               NO-LOCK.
      CASE composicion_carga.id_articulo.
        WHEN 600 THEN
          cNat = cNat + getDescArticulo(composicion_carga.id_articulo)  + " " + 
                        STRING(composicion_carga.cantidad) + " lts." + crlf. 
        WHEN 401 THEN
          cDes = cDes + getDescArticulo(composicion_carga.id_articulo)  + " " + 
                        STRING(composicion_carga.cantidad) + " lts." + crlf. 
        WHEN 500 THEN
          cH2O = cH2O + getDescArticulo(composicion_carga.id_articulo)  + " " + 
                        STRING(composicion_carga.cantidad) + " lts." + crlf.
        WHEN 20 THEN
          cCon = cCon + getDescArticulo(composicion_carga.id_articulo)  + " " + 
                        STRING(composicion_carga.cantidad) + " lts." + crlf.
      END CASE.
      
    END.
    cNat = SUBSTRING(cNat, 1, LENGTH(cNat) - 2).
    cDes = SUBSTRING(cDes, 1, LENGTH(cDes) - 2).
    cH2O = SUBSTRING(cH2O, 1, LENGTH(cH2O) - 2).
    cCon = SUBSTRING(cCon, 1, LENGTH(cCon) - 2).






    /*creo registro en la tabla reporteadora*/
    CREATE datos_carga.
    ASSIGN datos_carga.id_empresa               = cargas.id_empresa
           datos_carga.id_sucursal              = cargas.id_sucursal
           datos_carga.id_tipotambor            = cargas.id_tipotambor
           datos_carga.nromov                   = cargas.nromov
           datos_carga.detalle_tambores_repro   = cTam
           datos_carga.detalle_jugo_natural     = cNat
           datos_carga.detalle_agua             = cH2O
           datos_carga.detalle_jugo_concentrado = cCon.
    
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteReprocesos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteReprocesos Procedure 
PROCEDURE callReporteReprocesos :
/*------------------------------------------------------------------------------
  Purpose:     obtiene tambores reprocesados entre dos fechas
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plAjus  AS LOGICAL    NO-UNDO. /*determina reprocesos de ajustes*/


  RUN deleteTablasPerdida.

  /*obtengo lotes o cargas en donde se reprocesaron tambores en este periodo*/ 
  IF NOT plAjus THEN
    RUN fillCabeceraReprocesos(pdDesde, pdHasta, piSuc).
  ELSE
    RUN fillCabeceraAjustes(pdDesde, pdHasta, piSuc).
                            

  /*obtengo tambores volcados en los lotes o cargas obtenidas arriba*/   
  FOR EACH perdida_cargas NO-LOCK.         
    /*tambores resprocesados en la carga on en el lote*/
    RUN fillTamboresVolcadosEn (perdida_cargas.id_empresa,
                                perdida_cargas.id_sucursal,
                                perdida_cargas.id_tipotambor,
                                perdida_cargas.nromov, 
                                perdida_cargas.fecha_ajuste).      
    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteReprocesosOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteReprocesosOld Procedure 
PROCEDURE callReporteReprocesosOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plCga   AS LOGICAL    NO-UNDO. /*incluye o no cargas*/
 

  DEFINE BUFFER buLot FOR tambores_industria.
  DEFINE BUFFER buRep FOR tambores_industria.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iKil AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iKi4 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCon AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec AS DATE       NO-UNDO.
  DEFINE VARIABLE lCre AS LOGICAL    NO-UNDO.

  FOR EACH compo_lote_jugo.
    DELETE compo_lote_jugo.    
  END.


  FOR EACH buLot NO-LOCK WHERE buLot.fecha >= pdDesde
                           AND buLot.fecha <= pdHasta
                           AND (buLot.id_tipotambor <> 6 AND buLot.id_tipotambor <> 7 AND buLot.id_tipotambor <> 8 AND buLot.id_tipotambor <> 2) /*excluye tambores de aceites*/
                           AND (IF piSuc <> 0 THEN buLot.id_sucursal   = piSuc ELSE TRUE)
                           AND (IF piTip <> 0 THEN buLot.id_tipotambor = piTip ELSE TRUE)
                         BREAK BY buLot.nromov.
    i = i + 1.
    k = k + buLot.kilos_tambor.
    
    IF LAST-OF(buLot.nromov) THEN DO:  
      /*creo aqui para incluir lotes que no tengan reprocesos*/      
      ASSIGN iLot = buLot.id_lote
             iAno = buLot.anio
             iTam = i
             iKil = k
             iKi4 = getKilos400(buLot.id_tipotambor, buLot.id_articulo, buLot.id_calidad, k)
             cArt = getDescArticulo(buLot.id_articulo)
             cCal = getDescCalidad(buLot.id_calidad)
             cEnv = getDescEnvase(buLot.id_envase)
             dFec = buLot.fecha
             iEmp = buLot.id_empresa
             iSuc = buLot.id_sucursal
             iTip = buLot.id_tipotambor
             iNro = buLot.nromov
             iArt = buLot.id_articulo                 
             iCal = buLot.id_calidad                 
             iEnv = buLot.id_envase
             cTip = getDescTipoTambor(buLot.id_tipotambor)
             cCon = getDescCondicionLote(buLot.id_empresa, buLot.id_sucursal, buLot.id_tipotambor, buLot.nromov).

      CREATE compo_lote_jugo.
      ASSIGN compo_lote_jugo.id_lote               = iLot
             compo_lote_jugo.anio_lote             = iAno
             compo_lote_jugo.tambores              = i
             compo_lote_jugo.kilos                 = k
             compo_lote_jugo.kilos_400             = iKi4
             compo_lote_jugo.articulo              = cArt
             compo_lote_jugo.calidad               = cCal
             compo_lote_jugo.envase                = cEnv
             compo_lote_jugo.fecha                 = dFec
             compo_lote_jugo.nromov                = iNro
             compo_lote_jugo.tipotambor            = cTip
             compo_lote_jugo.id_sucursal           = iSuc
             compo_lote_jugo.condicion             = cCon.
             /*compo_lote_jugo.id_empresa            = iEmp             
             compo_lote_jugo.id_tipotambor         = iTip
             compo_lote_jugo.nromov                = iNro
             compo_lote_jugo.id_articulo           = iArt
             compo_lote_jugo.id_calidad            = iCal
             compo_lote_jugo.id_envase             = iEnv
             */
            /*compo_lote_jugo.lotmov                = STRING(buLot.id_lote) + "/" + STRING(buLot.anio) + " " + compo_lote_jugo.articulo + " Kgs por " + STRING(k) + " - " + STRING(buLot.nromov)*/
      lCre = TRUE.
      
      /*todos los tambores que tienen como destino el tambor en el que estoy parado*/
      FOR EACH buRep NO-LOCK WHERE buRep.id_empresa    = buLot.id_empresa_destino
                               AND buRep.id_sucursal   = buLot.id_sucursal_destino
                               AND buRep.id_tipotambor = buLot.id_tipotambor_destino
                               AND buRep.nromov        = buLot.nromov_destino
                             BREAK BY buRep.nromov.
        p = p + 1.
        q = q + buRep.kilos_tambor.
        IF LAST-OF(buRep.nromov) THEN DO:   
          IF NOT lCre THEN
            CREATE compo_lote_jugo.
          
          ASSIGN compo_lote_jugo.id_lote               = iLot
                 compo_lote_jugo.anio_lote             = iAno
                 compo_lote_jugo.tambores              = i
                 compo_lote_jugo.kilos                 = k
                 compo_lote_jugo.kilos_400             = iKi4
                 compo_lote_jugo.articulo              = cArt
                 compo_lote_jugo.calidad               = cCal
                 compo_lote_jugo.envase                = cEnv
                 compo_lote_jugo.fecha                 = dFec
                 compo_lote_jugo.tipotambor            = cTip
                 compo_lote_jugo.nromov                = iNro
                 compo_lote_jugo.id_sucursal           = iSuc
                 compo_lote_jugo.condicion             = cCon.
                 /*compo_lote_jugo.id_empresa            = iEmp                 
                 compo_lote_jugo.id_tipotambor         = iTip
                 compo_lote_jugo.nromov                = iNro
                 compo_lote_jugo.id_articulo           = iArt
                 compo_lote_jugo.id_calidad            = iCal
                 compo_lote_jugo.id_envase             = iEnv*/                 
                /*compo_lote_jugo.lotmov                = STRING(buLot.id_lote) + "/" + STRING(buLot.anio) + " " + compo_lote_jugo.articulo + " Kgs por " + STRING(k) + " - " + STRING(buLot.nromov)*/
                

          ASSIGN compo_lote_jugo.id_lote_destino        = buRep.id_lote
                 compo_lote_jugo.anio_lote_destino      = buRep.anio
                 compo_lote_jugo.tambores_destino       = p
                 compo_lote_jugo.kilos_destino          = q
                 compo_lote_jugo.kilos_400_destino      = getKilos400(buRep.id_tipotambor, buRep.id_articulo, buRep.id_calidad, q)
                 compo_lote_jugo.articulo_destino       = getDescArticulo(buRep.id_articulo)
                 compo_lote_jugo.calidad_destino        = getDescCalidad(buRep.id_calidad)
                 compo_lote_jugo.envase_destino         = getDescEnvase(buRep.id_envase)
                 compo_lote_jugo.fecha_destino          = buRep.fecha
                 compo_lote_jugo.nromov_destino         = buRep.nromov
                 compo_lote_jugo.tipotambor_destino     = getDescTipoTambor(buRep.id_tipotambor)
                 compo_lote_jugo.id_sucursal_destino    = buRep.id_sucursal
                 /*compo_lote_jugo.id_empresa     = buRep.id_empresa
                 compo_lote_jugo.id_sucursal    = buRep.id_sucursal
                 compo_lote_jugo.id_tipotambor  = buRep.id_tipotambor
                 compo_lote_jugo.nromov         = buRep.nromov
                 compo_lote_jugo.id_articulo    = buRep.id_articulo                 
                 compo_lote_jugo.id_calidad     = buRep.id_calidad                
                 compo_lote_jugo.id_envase      = buRep.id_envase                 
                 compo_lote_jugo.lotmov         = STRING(buRep.id_lote) + "/" + STRING(buRep.anio) + " " + compo_lote_jugo.articulo + " por " + STRING(q) + " Kgs - " + STRING(buRep.nromov)*/
          
          lCre = FALSE.
          p = 0.                                   
          q = 0.
        END.      
      END.
      
      ASSIGN iLot = 0
             iAno = 0
             iTam = 0
             iKil = 0
             iKi4 = 0
             cArt = ""
             cCal = ""
             cEnv = ""
             dFec = ?
             iEmp = 0
             iSuc = 0
             iTip = 0
             iNro = 0
             iArt = 0
             iCal = 0
             iEnv = 0
             cTip = ""
             cCon = "".


      i = 0.
      k = 0.
    END.
  END.

  /*CARGAS*/
  FOR EACH cargas WHERE cargas.fecha >= pdDesde
                    AND cargas.fecha <= pdHasta
                    AND (IF plCga THEN TRUE ELSE FALSE) 
                    AND (IF piSuc <> 0 THEN cargas.id_sucursal = piSuc ELSE TRUE) /*CUIDADO AQUI!!!*/
                  NO-LOCK.

    FOR EACH buRep NO-LOCK WHERE buRep.id_empresa_destino    = cargas.id_empresa
                             AND buRep.id_sucursal_destino   = cargas.id_sucursal
                             AND buRep.id_tipotambor_destino = cargas.id_tipotambor
                             AND buRep.nromov_destino        = cargas.nromov
                           BREAK BY buRep.nromov.
      p = p + 1.
      q = q + buRep.kilos_tambor.
      IF LAST-OF(buRep.nromov) THEN DO: 
        CREATE compo_lote_jugo.          
        ASSIGN compo_lote_jugo.id_lote_destino               = cargas.id_carga
               compo_lote_jugo.anio_lote_destino             = cargas.anio
               compo_lote_jugo.tambores_destino              = p 
               compo_lote_jugo.kilos_destino                 = q
               compo_lote_jugo.kilos_400_destino             = 0
               compo_lote_jugo.articulo_destino              = getDescArticulo(cargas.id_articulo)
               compo_lote_jugo.calidad_destino               = getDescCalidad(cargas.id_calidad)
               compo_lote_jugo.envase_destino                = ""
               compo_lote_jugo.fecha_destino                 = cargas.fecha
               compo_lote_jugo.nromov_destino                = cargas.nromov
               compo_lote_jugo.tipotambor_destino            = getDescTipoTambor(cargas.id_tipotambor)
               compo_lote_jugo.id_sucursal_destino           = cargas.id_sucursal.
               /*compo_lote_jugo.id_empresa            = iEmp               
               compo_lote_jugo.id_tipotambor         = iTip
               compo_lote_jugo.nromov                = iNro
               compo_lote_jugo.id_articulo           = iArt
               compo_lote_jugo.id_calidad            = iCal
               compo_lote_jugo.id_envase             = iEnv               
               compo_lote_jugo.condicion             = cCon               
               compo_lote_jugo.lotmov               = STRING(buLot.id_lote) + "/" + STRING(buLot.anio) + " " + compo_lote_jugo.articulo + " Kgs por " + STRING(k) + " - " + STRING(buLot.nromov)*/              

        ASSIGN compo_lote_jugo.id_lote        = buRep.id_lote
               compo_lote_jugo.anio_lote      = buRep.anio
               compo_lote_jugo.tambores       = p
               compo_lote_jugo.kilos          = q
               compo_lote_jugo.kilos_400      = getKilos400(buRep.id_tipotambor, buRep.id_articulo, buRep.id_calidad, q)
               compo_lote_jugo.articulo       = getDescArticulo(buRep.id_articulo)
               compo_lote_jugo.calidad        = getDescCalidad(buRep.id_calidad)
               compo_lote_jugo.envase         = getDescEnvase(buRep.id_envase)
               compo_lote_jugo.fecha          = buRep.fecha
               compo_lote_jugo.nromov         = buRep.nromov
               compo_lote_jugo.tipotambor     = getDescTipoTambor(buRep.id_tipotambor).
               /*compo_lote_jugo.id_sucursal    = buRep.id_sucursal.
               compo_lote_jugo.id_empresa     = buRep.id_empresa               
               compo_lote_jugo.id_tipotambor  = buRep.id_tipotambor
               compo_lote_jugo.nromov         = buRep.nromov
               compo_lote_jugo.id_articulo    = buRep.id_articulo                 
               compo_lote_jugo.id_calidad     = buRep.id_calidad                
               compo_lote_jugo.id_envase      = buRep.id_envase               
               compo_lote_jugo.lotmov         = STRING(buRep.id_lote) + "/" + STRING(buRep.anio) + " " + compo_lote_jugo.articulo + " por " + STRING(q) + " Kgs - " + STRING(buRep.nromov)*/
          
        p = 0.
        q = 0.

      END.
    END.
    
    
  END.

  /*
  FOR EACH compo_lote_jugo.
    compo_lote_jugo.merma = getMerma(compo_lote_jugo.id_empresa, 
                                 compo_lote_jugo.id_sucursal, 
                                 compo_lote_jugo.id_tipotambor, 
                                 compo_lote_jugo.nromov, 
                                 compo_lote_jugo.kilos).
    
  END.
  */

  RUN exportExcelReproc.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteStock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteStock Procedure 
PROCEDURE callReporteStock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plLotPro AS LOGICAL    NO-UNDO.

  RUN deleteStockTambores.
  RUN fillttOrdenReporte.


  RUN fillStockTamboresOf (piSucUbi, 3).    /*lotes jugo of*/  
  RUN fillStockTamboresNoOf (piSucUbi, 3).  /*lotes jugo no of*/
  
  RUN fillStockTamboresNoOf (piSucUbi, 1).  /*produccion jugo*/
  
  RUN fillStockTamboresOf (piSucUbi, 6).    /*lotes aceite of*/
  RUN fillStockTamboresNoOf (piSucUbi, 6).  /*lotes aceite no of*/
  
  RUN fillStockTamboresNoOf (piSucUbi, 7).  /*foldeado*/

  RUN fillStockTamboresNoOf (piSucUbi, 4).  /*sobrante jugo*/
  RUN fillStockTamboresNoOf (piSucUbi, 5).  /*arrastre jugo*/
  
  RUN callMatPrimaAceite (piSucUbi).        /*materia prima aceite*/
  
  RUN fillStockTamboresNoOf (piSucUbi, 9).  /*prod. terceros*/
  
  RUN fillStockTamboresReproc(piSucUbi, 3, 2). /*reprocesos de jugo mp p/reproceso*/  
  
  RUN fillStockTamboresReproc(piSucUbi, 4, 2). /*reprocesos de sobrante de jugo mp p/reproceso*/  
  RUN fillStockTamboresReproc(piSucUbi, 5, 2). /*reprocesos de arrastre de jugo mp p/reproceso*/  
  
  IF plLotPro THEN
    RUN fillStockTamboresReproc(piSucUbi, 3, 7).  /*tambores en estado vacio-etiquetado, en proceso de envase*/

  RUN fillStockTamboresReproc(piSucUbi, 1, 3).  /*tambores de produccion de jugo rebatcheados en lotes que todavia no estan cerrados*/
  RUN fillStockTamboresReproc(piSucUbi, 3, 3).  /*tambores de lotes de jugo rebatcheados en lotes que todavia no estan cerrados*/
  RUN fillStockTamboresReproc(piSucUbi, 4, 3).  /*tambores de sobrante de jugo rebatcheados en lotes que todavia no estan cerrados*/
  RUN fillStockTamboresReproc(piSucUbi, 5, 3).  /*tambores de arrastre de jugo rebatcheados en lotes que todavia no estan cerrados*/

  RUN fillStockTbsReproCargas(piSucUbi, 1).  /*tambores de produccion reprocesados en cargas*/
  RUN fillStockTbsReproCargas(piSucUbi, 3).  /*tambores de lotes reprocesados en cargas*/
  RUN fillStockTbsReproCargas(piSucUbi, 4).  /*tambores de sobrante reprocesados en cargas*/
  RUN fillStockTbsReproCargas(piSucUbi, 5).  /*tambores de arrastre reprocesados en cargas*/
  RUN fillStockTbsReproCargas(piSucUbi, 9).  /*tambores de terceros reprocesados en cargas*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callReporteStockDepExt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callReporteStockDepExt Procedure 
PROCEDURE callReporteStockDepExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  RUN deleteStockTambores.
  RUN fillttOrdenReporte.


  FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion = piSuc
                                AND tambores_industria.id_locacion_ubicacion = 4
                              BREAK BY tambores_industria.id_lote_deposito.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.id_lote_deposito) THEN DO:
      RUN addStockTambores(ROWID(tambores_industria), i, k).
      i = 0.
      k = 0.
    END.
    
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callStockFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callStockFecha Procedure 
PROCEDURE callStockFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt    AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  FOR EACH stock_fecha.
    DELETE stock_fecha.
  END.
                                                      
  RUN addTamboresFecha (piSucUbi, pdDesde, pdHasta, piLote, piAnio, piArt). /*tambores involucrados en el periodo*/
  RUN delTamboresRemitosSalida (piSucUbi, pdDesde, pdHasta, piLote, piAnio, piArt). /*elimino tambores sacados por remitos*/
  RUN addTamboresRemitosEntrada (piSucUbi, pdDesde, pdHasta, piLote, piAnio, piArt). /*agrego tambores enviados por remitos de otras sucursales*/
  RUN delTamboresProcesados(1). /*producciones de jugo*/
  RUN delTamboresProcesados(2). /*producciones de aceite*/
  RUN delTamboresProcesados(3). /*lotes de jugo*/
  RUN delTamboresProcesados(4). /*sobrantes de jugo*/
  RUN delTamboresProcesados(3). /*arrastre de jugo*/
  RUN delTamboresProcesados(8). /*sobrantes de aceite*/
  RUN delTamboresProcesados(9). /*producciones de terceros*/
  RUN delTamboresProcesadosCargas(1, pdDesde, pdHasta). /*producciones de jugos reprocesadas en cargas*/
  RUN delTamboresProcesadosCargas(3, pdDesde, pdHasta). /*lotes de jugos reprocesadas en cargas*/
  RUN setEstadoTamboresAbiertoFecha(piSucUbi, pdDesde, pdHasta, piLote, piAnio, piArt). /*lotes abiertos en fecha pdHasta*/
  
  RUN deleteStockTambores.

  FOR EACH stock_fecha BREAK BY stock_fecha.nromov.
    i = i + 1.
    k = k + stock_fecha.kilos.    
    IF LAST-OF(stock_fecha.nromov) THEN DO:    
      /*DISP stock_fecha.id_lote stock_fecha.anio stock_fecha.nromov stock_fecha.nro_remito i WITH WIDTH 150.*/
      FIND FIRST buTam WHERE buTam.id_empresa    = stock_fecha.id_empresa
                         AND buTam.id_sucursal   = stock_fecha.id_sucursal
                         AND buTam.id_tipotambor = stock_fecha.id_tipotambor
                         AND buTam.nromov        = stock_fecha.nromov
                       NO-LOCK NO-ERROR.
      IF AVAILABLE buTam AND i <> 0 THEN DO:     
        RUN addStockTambores(ROWID(buTam), i, k).        
        i = 0.
        k = 0.
      END.      
    END.

    /*revisar aqui, esto esta horrible*/
    FOR EACH stock_tambores.
      ASSIGN stock_tambores.estado = getDescEstado(stock_tambores.id_estado).      
    END.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-callSubdDespachos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callSubdDespachos Procedure 
PROCEDURE callSubdDespachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  FOR EACH rptDespachos.
    DELETE rptDespachos.
  END.

  FOR EACH subd_despachos
      WHERE subd_despachos.fecha_despacho >= pdDesde
        AND subd_despachos.fecha_despacho <= pdHasta.
    RUN addSubdDespacho (ROWID(subd_despachos_industria)).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteClienteDespachos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteClienteDespachos Procedure 
PROCEDURE deleteClienteDespachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH cliente_despachos.
    DELETE cliente_despachos.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteCompoLoteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteCompoLoteJugo Procedure 
PROCEDURE deleteCompoLoteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH compo_lote_jugo.
    DELETE compo_lote_jugo.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteProduccionIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteProduccionIndustria Procedure 
PROCEDURE deleteProduccionIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH produccion_industria.
    DELETE produccion_industria.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteStockTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteStockTambores Procedure 
PROCEDURE deleteStockTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH stock_tambores.
    DELETE stock_tambores.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteTablasPerdida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTablasPerdida Procedure 
PROCEDURE deleteTablasPerdida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*borrar tablas reporteadoras*/
  FOR EACH perdida_cargas.
    DELETE perdida_cargas.    
  END.
  FOR EACH perdida_tbs_repro.
    DELETE perdida_tbs_repro.    
  END.
  FOR EACH perdida_composicion.
    DELETE perdida_composicion.    
  END.
  FOR EACH perdida_pto_envase.
    DELETE perdida_pto_envase.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteTamboresCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTamboresCamara Procedure 
PROCEDURE deleteTamboresCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tambores_camara.
    DELETE tambores_camara.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delTamboresProcesados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delTamboresProcesados Procedure 
PROCEDURE delTamboresProcesados :
/*------------------------------------------------------------------------------
  Purpose:     recorre el conjunto de tambores involucrados y borra aquellos tamboers
               que hayan sido procesados en otro lote
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  
  DEFINE BUFFER buLD FOR stock_fecha. /*lo uso como lotes destinos de producciones*/

  /*reprocesos en lotes o prods*/
  FOR EACH stock_fecha WHERE stock_fecha.id_tipotambor   = piTip
                         AND stock_fecha.nromov_destino <> 0.
    FIND FIRST buLD WHERE stock_fecha.nromov_destino = buLD.nromov NO-ERROR.
    IF AVAILABLE buLD THEN DO: /*la produccion tiene destino y el lote destino esta en los tambores involucrados*/
      DELETE stock_fecha.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delTamboresProcesadosCargas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delTamboresProcesadosCargas Procedure 
PROCEDURE delTamboresProcesadosCargas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  /*reprocesos en caragas*/
  FOR EACH stock_fecha WHERE stock_fecha.id_tipotambor   = piTip
                         AND stock_fecha.nromov_destino <> 0.
    FIND FIRST cargas WHERE cargas.nromov = stock_fecha.nromov_destino
                        AND cargas.fecha >= pdDesde
                        AND cargas.fecha <= pdHasta
                      NO-LOCK NO-ERROR.
    IF AVAILABLE cargas THEN DO: /*la produccion fue reprocesada en una carga*/
      DELETE stock_fecha.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delTamboresRemitosSalida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delTamboresRemitosSalida Procedure 
PROCEDURE delTamboresRemitosSalida :
/*------------------------------------------------------------------------------
  Purpose:     elimina tambores sacados via remitos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt   AS INTEGER    NO-UNDO.


  FOR EACH remitos WHERE remitos.id_sucursal = piSuc
                     AND remitos.fecha      >= pdDesde
                     AND remitos.fecha      <= pdHasta
                     AND remitos.estado      = TRUE
                   NO-LOCK, 
      EACH items_factura OF remitos WHERE (IF piArt <> 0 THEN items_factura.id_articulo = piArt ELSE TRUE)
                                    NO-LOCK, 
      EACH r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                             AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                             AND r_tambor_remito.nro_remito         = items_factura.nro
                             AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                             AND r_tambor_remito.id_sucursal        = piSuc
                             AND r_tambor_remito.fecha             >= pdDesde
                             AND r_tambor_remito.fecha             <= pdHasta
                          NO-LOCK, 
      FIRST tambores_industria WHERE tambores_industria.id_empresa    = r_tambor_remito.id_empresa
                                 AND tambores_industria.id_sucursal   = r_tambor_remito.id_sucursal
                                 AND tambores_industria.id_tipotambor = r_tambor_remito.id_tipotambor
                                 AND tambores_industria.nromov        = r_tambor_remito.nromov
                                 AND tambores_industria.id_tambor     = r_tambor_remito.id_tambor
                                 AND tambores_industria.fecha        >= pdDesde
                                 AND tambores_industria.fecha        <= pdHasta
                                 AND (IF piLote <> 0 THEN tambores_industria.id_lote     = piLote ELSE TRUE)
                                 AND (IF piAnio <> 0 THEN tambores_industria.anio        = piAnio ELSE TRUE)
                                 AND (IF piArt  <> 0 THEN tambores_industria.id_articulo = piArt  ELSE TRUE)
                               NO-LOCK .
    FIND FIRST stock_fecha WHERE stock_fecha.id_empresa    = r_tambor_remito.id_empresa
                             AND stock_fecha.id_sucursal   = r_tambor_remito.id_sucursal
                             AND stock_fecha.id_tipotambor = r_tambor_remito.id_tipotambor
                             AND stock_fecha.nromov        = r_tambor_remito.nromov
                             AND stock_fecha.id_tambor     = r_tambor_remito.id_tambor
                           NO-ERROR.
    IF AVAILABLE stock_fecha THEN DO:
      DELETE stock_fecha.
    END.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-despachoRemitos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE despachoRemitos Procedure 
PROCEDURE despachoRemitos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  FOR EACH remitos
      WHERE remitos.id_operacion = 311
        AND remitos.fecha       >= pdDesde
        AND remitos.fecha       >= pdHasta
      NO-LOCK.

    FIND FIRST proveedores OF remitos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF remitos NO-LOCK NO-ERROR.
    FIND FIRST sucursales WHERE sucursales.id_sucursal = remitos.id_sucursal NO-LOCK NO-ERROR.
    FIND FIRST destinos OF remitos NO-LOCK NO-ERROR.
    FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
    
    CREATE  rptRemitoTransporte.
    ASSIGN  rptRemitoTransporte.id_sucursal_remito    = remitos.id_sucursal
            rptRemitoTransporte.id_tipo_movsto        = remitos.id_tipo_movsto
            rptRemitoTransporte.nro_remito            = remitos.nro
            rptRemitoTransporte.nro_comp              = remitos.nro_comp
            rptRemitoTransporte.fecha_remito          = remitos.fecha 
            rptRemitoTransporte.id_cliente            = remitos.id_cliente
            rptRemitoTransporte.kilos_total           = remitos.peso_neto 
            rptRemitoTransporte.lugar_descarga        = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "NONE" 
            rptRemitoTransporte.destino               = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
            rptRemitoTransporte.domicilio_cliente     = IF AVAILABLE clientes THEN clientes.domicilio ELSE "NONE"
            rptRemitoTransporte.domicilio_transporte  = IF AVAILABLE proveedores THEN proveedores.domicilio ELSE "NONE"
            rptRemitoTransporte.cuit_cliente          = IF AVAILABLE clientes THEN clientes.cuit ELSE "NONE"
            rptRemitoTransporte.cliente               = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE"
            rptRemitoTransporte.nombre_chofer         = remitos.chofer
            rptRemitoTransporte.patente_chasis        = remitos.pat_chasis
            rptRemitoTransporte.patente_acoplado      = remitos.pat_acopla
            rptRemitoTransporte.peso_bruto            = remitos.peso_bruto
            rptRemitoTransporte.peso_neto             = remitos.peso_neto
            rptRemitoTransporte.transporte            = IF AVAILABLE proveedores THEN proveedores.razon_social ELSE "NONE"
            .

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-destinatariosMuestras) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destinatariosMuestras Procedure 
PROCEDURE destinatariosMuestras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER dHasta AS DATE       NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH muestras 
      WHERE muestras.fecha >= dDesde
        AND muestras.fecha <= dHasta
      BREAK BY muestras.id_destinatario.
    i = i + 1.
  
    IF LAST-OF(muestras.id_destinatario) THEN DO:
      FIND FIRST contactos_muestras 
           WHERE contactos_muestra.id_contacto = muestras.id_destinatario 
           NO-LOCK NO-ERROR.
      IF AVAILABLE contactos_muestras THEN DO:
        CREATE ttDestinosMuestras.
        BUFFER-COPY contactos_muestra TO ttDestinosMuestras.
        ttDestinosMuestras.cantidad = i.
      END.
  
      i = 0.
    END.
  
  
  END.

   RUN generateExcel.p (INPUT TABLE ttDestinosMuestras,
                        INPUT " Destinos Muestras",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportConsumoEnvases) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportConsumoEnvases Procedure 
PROCEDURE exportConsumoEnvases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piArtic AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iCant AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.fecha >= pdDesde
        AND tambores_industria.fecha <= pdHasta
        AND (IF piArtic <> 0 THEN tambores_industria.id_articulo = piArtic ELSE TRUE)
      BREAK BY STRING(tambores_industria.id_articulo) + 
               STRING(tambores_industria.id_envase)
      BY tambores_industria.id_articulo.
  
    iCant = iCant + 1.

    IF LAST-OF(STRING(tambores_industria.id_articulo) + STRING(tambores_industria.id_envase)) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      CREATE ttConsumos.
      ASSIGN  ttConsumos.id_articulo = tambores_industria.id_articulo
              ttConsumos.id_envase   = tambores_industria.id_envase
              ttConsumos.envase      = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE ""
              ttConsumos.articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE ""
              ttConsumos.cantidad    = iCant
              iCant                  = 0
              .
    END.
  END.

  RUN generateExcel.p (TABLE ttConsumos,
                       " Consumo de Envases Industria",
                       ""  ,
                       7,
                       8,
                       "Arial",
                       8).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportConsumosClarificado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportConsumosClarificado Procedure 
PROCEDURE exportConsumosClarificado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTbs AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.

  FOR EACH ttAuxCompo.
    DELETE ttAuxCompo.
  END.
  FOR EACH ttAuxRepro.
    DELETE ttAuxRepro.
  END.
  FOR EACH ttConsumosProcesos.
    DELETE ttConsumosProcesos.
  END.

  FOR EACH ttResultadoProcesos.
    DELETE ttResultadoProcesos.
  END.

  /* recupero composiones de cargas */
  FOR EACH proceso
      WHERE proceso.fecha     >= pdDesde
        AND proceso.fecha     <= pdHasta
        AND proceso.fecha_fin <> ?
      NO-LOCK, 
      EACH cargas
      WHERE cargas.nromov_proceso = proceso.nromov
      NO-LOCK, 
      EACH composicion_carga
      WHERE composicion_carga.nromov = cargas.nromov
      NO-LOCK.

    CREATE ttAuxCompo.
    ASSIGN  ttAuxCompo.nromov_proceso = proceso.nromov
            ttAuxCompo.nromov_carga   = cargas.nromov 
            ttAuxCompo.id_articulo    = composicion_carga.id_articulo
            ttAuxCompo.cantidad       = composicion_carga.cantidad
            ttAuxCompo.bx             = composicion_carga.bx_20_20
            ttAuxCompo.ac             = composicion_carga.acidez_w_w
            ttAuxCompo.bx_corr        = composicion_carga.bx_correg
            ttAuxCompo.ac_gpl         = composicion_carga.acidez_w_v
            ttAuxCompo.id_unidad      = composicion_carga.id_unidad_quimica
            ttAuxCompo.kilos          = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, composicion_carga.acidez_w_w, composicion_carga.bx_20_20, composicion_carga.cantidad)
            ttAuxCompo.k400           = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, composicion_carga.bx_20_20, composicion_carga.acidez_w_w, composicion_carga.cantidad, TRUE)
            .
 
  END.

  /* recupero reprocesos volcados en cargas */
  FOR EACH proceso
      WHERE proceso.fecha     >= pdDesde
        AND proceso.fecha     <= pdHasta
        AND proceso.fecha_fin <> ?
      NO-LOCK, 
      EACH cargas
      WHERE cargas.nromov_proceso = proceso.nromov
      NO-LOCK,
      EACH tambores_industria
      WHERE tambores_industria.nromov_destino = cargas.nromov
      BREAK BY tambores_industria.nromov.

    iTbs = iTbs + 1.
    fKil = fKil + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
        
      cAnl = getValoresAnalisis(tambores_industria.id_empresa, 
                                tambores_industria.id_sucursal, 
                                tambores_industria.id_tipotambor, 
                                tambores_industria.nromov).

      IF AVAILABLE tambores_industria THEN
        fKi4 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, fKil). 
      ELSE
        fKi4 = 0.
 
      CREATE  ttAuxRepro.
      ASSIGN  ttAuxRepro.nromov_proceso = proceso.nromov
              ttAuxRepro.nromov_lote    = IF AVAILABLE tambores_industria THEN tambores_industria.nromov ELSE 0
              ttAuxRepro.id_articulo    = IF AVAILABLE tambores_industria THEN tambores_industria.id_articulo ELSE 0
              ttAuxRepro.tambores       = iTbs
              ttAuxRepro.kilos          = fKil 
              ttAuxRepro.k400           = fKi4
              ttAuxRepro.bx             = DECIMAL(ENTRY(3, cAnl, CHR(1)))
              ttAuxRepro.ac             = DECIMAL(ENTRY(2, cAnl, CHR(1)))
              ttAuxRepro.bx_corr        = DECIMAL(ENTRY(4, cAnl, CHR(1)))
              ttAuxRepro.ac_gpl         = DECIMAL(ENTRY(1, cAnl, CHR(1)))
              iTbs                      = 0
              fKil                      = 0
              fKi4                      = 0
              . 

    END.
  END.

  /* tabla resultado */
  FOR EACH ttAuxCompo
      BREAK BY STRING(ttAuxCompo.nromov_proceso) + STRING(ttAuxCompo.id_articulo).
    
    fLit = fLit + ttAuxCompo.cantidad.
    fKil = fKil + ttAuxCompo.kilos.
    fKi4 = fKi4 + ttAuxCompo.k400.
    
    IF LAST-OF(STRING(ttAuxCompo.nromov_proceso) + STRING(ttAuxCompo.id_articulo)) THEN DO:

      FIND FIRST proceso WHERE proceso.nromov = ttAuxCompo.nromov_proceso NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = ttAuxCompo.id_articulo NO-LOCK NO-ERROR.
      FIND FIRST unidades_quimicas WHERE unidades_quimicas.id_unidad = ttAuxCompo.id_unidad NO-LOCK NO-ERROR.
     
      CREATE  ttConsumosProcesos.
      ASSIGN  ttConsumosProcesos.id_proceso     = proceso.id_proceso
              ttConsumosProcesos.anio           = proceso.anio
              ttConsumosProcesos.descripcion    = proceso.descripcion
              ttConsumosProcesos.id_arti_comp   = ttAuxCompo.id_articulo
              ttConsumosProcesos.producto_comp  = productos_terminados.descripcion
              ttConsumosProcesos.cantidad       = fLit
              ttConsumosProcesos.bx             = ttAuxCompo.bx
              ttConsumosProcesos.ac             = ttAuxCompo.ac
              ttConsumosProcesos.bx_corr        = ttAuxCompo.bx_corr
              ttConsumosProcesos.gpl            = ttAuxCompo.ac_gpl
              ttConsumosProcesos.kilos          = fKil
              ttConsumosProcesos.kilos400       = fKi4
              ttConsumosProcesos.unidad         = unidades_quimicas.descripcion
              fLit                              = 0
              fKil                              = 0
              fKi4                              = 0
              .
    END.
  END.

  FOR EACH ttAuxRepro
      BREAK BY STRING(ttAuxRepro.nromov_proceso) + STRING(ttAuxRepro.id_articulo).

    iTbs = iTbs + ttAuxRepro.tambores.
    fKil = fKil + ttAuxRepro.kilos.
    fKi4 = fKi4 + ttAuxRepro.k400.

    IF LAST-OF(STRING(ttAuxRepro.nromov_proceso) + STRING(ttAuxRepro.id_articulo)) THEN DO:

      FIND FIRST proceso WHERE proceso.nromov = ttAuxRepro.nromov_proceso NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = ttAuxRepro.id_articulo NO-LOCK NO-ERROR.

      CREATE  ttConsumosProcesos.
      ASSIGN  ttConsumosProcesos.id_proceso     = proceso.id_proceso
              ttConsumosProcesos.anio           = proceso.anio
              ttConsumosProcesos.descripcion    = proceso.descripcion
              ttConsumosProcesos.id_arti_comp   = ttAuxRepro.id_articulo
              ttConsumosProcesos.producto_comp  = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttConsumosProcesos.cantidad       = fKil
              ttConsumosProcesos.bx             = ttAuxRepro.bx
              ttConsumosProcesos.ac             = ttAuxRepro.ac
              ttConsumosProcesos.bx_corr        = ttAuxRepro.bx_corr
              ttConsumosProcesos.gpl            = ttAuxRepro.ac_gpl
              ttConsumosProcesos.kilos          = fKil
              ttConsumosProcesos.kilos400       = fKi4
              ttConsumosProcesos.unidad         = "KILOS"
              ttConsumosProcesos.tambores       = iTbs
              fKil                              = 0
              iTbs                              = 0
              fKi4                              = 0
              .
              
    END.
  END.

  /* recupero tambores resultado de los procesos */
  FOR EACH proceso
      WHERE proceso.fecha     >= pdDesde
        AND proceso.fecha     <= pdHasta
        AND proceso.fecha_fin <> ?
      NO-LOCK,
      EACH r_proceso_tambor
      WHERE r_proceso_tambor.nromov_proceso = proceso.nromov
      BREAK BY string(r_proceso_tambor.nromov_proceso) + string(r_proceso_tambor.nromov).
    
    iTbs = iTbs + 1.
    IF LAST-OF(string(r_proceso_tambor.nromov_proceso) + string(r_proceso_tambor.nromov)) THEN DO:
      
      FIND FIRST tambores_industria WHERE tambores_industria.nromov = r_proceso_tambor.nromov NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.

     fKil = DYNAMIC-FUNCTION('getKilosLote' IN hLib, r_proceso_tambor.id_empresa, r_proceso_tambor.id_sucursal, r_proceso_tambor.id_tipotambor, r_proceso_tambor.nromov).
     fKi4 = DYNAMIC-FUNCTION('getKilos400' IN hLib, r_proceso_tambor.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, fKil).
      
      CREATE ttResultadoProcesos.
      ASSIGN  ttResultadoProcesos.id_proceso     = proceso.id_proceso
              ttResultadoProcesos.anio           = proceso.anio
              ttResultadoProcesos.descripcion    = proceso.descripcion
              ttResultadoProcesos.id_arti_comp   = tambores_industria.id_articulo
              ttResultadoProcesos.producto_comp  = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttResultadoProcesos.cantidad       = fKil
              ttResultadoProcesos.bx             = 0
              ttResultadoProcesos.ac             = 0
              ttResultadoProcesos.bx_corr        = ttAuxRepro.bx_corr
              ttResultadoProcesos.gpl            = ttAuxRepro.ac_gpl
              ttResultadoProcesos.kilos          = fKil
              ttResultadoProcesos.kilos400       = fKi4
              ttResultadoProcesos.unidad         = "KILOS"
              ttResultadoProcesos.tambores       = iTbs
              fKil                               = 0
              iTbs                               = 0
              fKi4                               = 0
              .
    END.



  END.


  RUN generateExcel.p (TABLE ttConsumosProcesos,
                       " Consumos Clarificados",
                       ""  ,
                       7,
                       8,
                       "Arial",
                       8).

  RUN generateExcel.p (TABLE ttResultadoProcesos,
                       " Resultado Procesos Clarificado",
                       ""  ,
                       7,
                       8,
                       "Arial",
                       8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelAnalisisJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelAnalisisJugo Procedure 
PROCEDURE exportExcelAnalisisJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  FOR EACH lotes_jugo
      WHERE lotes_jugo.fecha >= pdDesde
        AND lotes_jugo.fecha <= pdHasta
      NO-LOCK.

    FIND FIRST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_jugo NO-LOCK NO-ERROR.

    IF AVAILABLE inspecciones_lote THEN DO:
      CREATE ttAnalisis.
      ASSIGN  ttAnalisis.id_lote      = lotes_jugo.id_lote
              ttAnalisis.anio         = lotes_jugo.anio
              ttAnalisis.id_articulo  = lotes_jugo.id_articulo
              ttAnalisis.producto     = productos_terminados.descripcion
              ttAnalisis.calidad      = calidades.descripcion
              ttAnalisis.envase       = envases_prod.descripcion
              ttAnalisis.acidez_w_w   = Inspecciones_lote.Acidez_w_v 
              ttAnalisis.acidez_w_v   = Inspecciones_lote.Acidez_w_w 
              ttAnalisis.bx_20        = Inspecciones_lote.Bx_20_20 
              ttAnalisis.bx_corr      = Inspecciones_lote.Bx_correg 
              ttAnalisis.ftu          = Inspecciones_lote.ftu 
              ttAnalisis.litros       = Inspecciones_lote.Litros 
              ttAnalisis.pulpa        = Inspecciones_lote.Porcentaje_pulpa 
              ttAnalisis.ratio        = Inspecciones_lote.Ratio
              .
    END.

  END.

  RUN generateExcel.p (INPUT TABLE ttAnalisis,
                        INPUT " Analisis de Lotes Jugo",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelCamara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelCamara Procedure 
PROCEDURE exportExcelCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR vcRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\dinamicaReproc.xls').
  /*chWorkbook  = chExcelAplication:Workbooks:ADD().*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  chWorkSheet:Range("A1:AM1"):Font:Bold = TRUE.
  
  chWorkSheet:Range("A1:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.
   
  chWorkSheet:Range("A1"):Value = "Sucursal".
  chWorkSheet:Range("A1:A1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("A"):ColumnWidth = 10.

  chWorkSheet:Range("B1"):Value = "Lote".
  chWorkSheet:Range("B1:B1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("B"):ColumnWidth = 10.

  chWorkSheet:Range("C1"):Value = "Tambor".
  chWorkSheet:Range("C1:C1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("C"):ColumnWidth = 10.

  chWorkSheet:Range("D1"):Value = "Camara".
  chWorkSheet:Range("D1:D1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("D"):ColumnWidth = 10.
  
  chWorkSheet:Range("E1"):Value = "Fila".
  chWorkSheet:Range("E1:E1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("E"):ColumnWidth = 10.
  
  chWorkSheet:Range("F1"):Value = "Columna".
  chWorkSheet:Range("F1:F1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("F"):ColumnWidth = 10.
  
  chWorkSheet:Range("G1"):Value = "Articulo".
  chWorkSheet:Range("G1:G1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("G"):ColumnWidth = 10.
  
  chWorkSheet:Range("H1"):Value = "Calidad".
  chWorkSheet:Range("H1:H1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("H"):ColumnWidth = 10.

  chWorkSheet:Range("I1"):Value = "Envase".
  chWorkSheet:Range("I1:I1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("I"):ColumnWidth = 10.
/*
  chWorkSheet:Range("J1"):Value = "Tambores".
  chWorkSheet:Range("J1:J1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("J"):ColumnWidth = 10.

  chWorkSheet:Range("K1"):Value = "Kgs".
  chWorkSheet:Range("K1:K1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("K"):ColumnWidth = 10.
  
  chWorkSheet:Range("L1"):Value = "Articulo".
  chWorkSheet:Range("L1:L1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("L"):ColumnWidth = 10.
  
  chWorkSheet:Range("M1"):Value = "Fecha".
  chWorkSheet:Range("M1:M1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("M"):ColumnWidth = 10.

  chWorkSheet:Range("N1"):Value = "TipoTambor Destino".
  chWorkSheet:Range("N1:N1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("N"):ColumnWidth = 10.

  chWorkSheet:Range("O1"):Value = "NroMov Destino".
  chWorkSheet:Range("O1:O1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("O"):ColumnWidth = 10.

  chWorkSheet:Range("P1"):Value = "NroMov".
  chWorkSheet:Range("P1:P1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("P"):ColumnWidth = 10.

  chWorkSheet:Range("Q1"):Value = "Tipo Tambor".
  chWorkSheet:Range("Q1:Q1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("Q"):ColumnWidth = 10.

  chWorkSheet:Range("R1"):Value = "Condicion Destino".
  chWorkSheet:Range("R1:R1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("R"):ColumnWidth = 10.
  */

  viFila = 2.
  FOR EACH tambores_camara NO-LOCK.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(tambores_camara.id_sucursal_ubicacion) .
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(tambores_camara.id_lote) + " / " + STRING(tambores_camara.anio_lote).
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = STRING(tambores_camara.id_tambor).
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.camara.
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.nro_fila.
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.nro_columna.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.articulo.
    vcRange = "H" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.calidad.
    vcRange = "I" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = tambores_camara.envase.
    /*
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tambores.
    vcRange = "K" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.kilos, ">>,>>>,>>9.99").
    vcRange = "L" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.articulo.
    vcRange = "M" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.fecha, "99/99/9999").
    vcRange = "N" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tipotambor_destino.
    vcRange = "O" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.nromov_destino.
    vcRange = "P" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.nromov.
    vcRange = "Q" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tipotambor.
    vcRange = "R" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.condicion.
    */

    viFila = viFila + 1.
  END.
  RELEASE compo_lote_jugo.

  /*
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("reproc"):RefreshTable().
  */
  /*chExcelAplication:VISIBLE = TRUE.*/
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelGalones) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelGalones Procedure 
PROCEDURE exportExcelGalones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcFile AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piRows AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEnv AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fGal AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.


  

  
  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR iFila  AS INTEGER.
  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN(pcFile). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

  DO iFila = 2 TO piRows:
    /* obtengo clave de lote */
    ASSIGN  iArt = INTEGER(chWorkSheet:Range("Q" + STRING(iFila)):VALUE)
            iEnv = INTEGER(chWorkSheet:Range("S" + STRING(iFila)):VALUE)
            iLot = INTEGER(chWorkSheet:Range("Z" + STRING(iFila)):VALUE)
            iAno = INTEGER(chWorkSheet:Range("AA" + STRING(iFila)):VALUE).

    FOR FIRST tambores_industria
        WHERE tambores_industria.id_articulo = iArt
          AND tambores_industria.id_envase   = iEnv
          AND tambores_industria.id_lote     = iLot
          AND tambores_industria.anio        = iAno
        NO-LOCK.
      ASSIGN iEmp = tambores_industria.id_empresa
             iSuc = tambores_industria.id_sucursal
             iTip = tambores_industria.id_tipotambor
             iNro = tambores_industria.nromov.
    
    END.

    fKil = DYNAMIC-FUNCTION('getKilosLote' IN hLib, iEmp, iSuc, iTip, iNro).
    fGal = DYNAMIC-FUNCTION('getGalonesLote' IN hLib, iEmp, iSuc, iTip, iNro).
    fGal = fGal / fKil.


    chWorkSheet:Range("IV" + STRING(iFila)):Value  = fGal.
  END.

  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelMolienda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelMolienda Procedure 
PROCEDURE exportExcelMolienda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.

  DEFINE VARIABLE chApp      AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chSheet    AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chCell     AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom    AS HANDLE.
  DEFINE VARIABLE cRange     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iFila      AS INTEGER    NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  chApp = DYNAMIC-FUNCTION('openExcelApplication' IN hLibCom, '..\industria\tplMolienda.xls', TRUE).
  IF NOT VALID-HANDLE(chApp) THEN RETURN.
  chSheet = chApp:Sheets:ITEM(1).
  iFila = 2.

  
  FOR EACH molienda_silo
      WHERE (IF pdFec <> ? THEN molienda_silo.fecha = pdFec ELSE TRUE)
      NO-LOCK,
    EACH items_molienda_silo 
      OF molienda_silo
     BY items_molienda_silo.id_linea 
     BY molienda_silo.id_turno 
     BY items_molienda_silo.id_silo.

    FIND FIRST lineas_produccion WHERE lineas_produccion.id_linea = molienda_silo.id_linea NO-LOCK NO-ERROR.
    FIND FIRST silo OF items_molienda_silo NO-LOCK NO-ERROR.
 
    cRange = "A" + STRING(iFila).
    chCell = chSheet:Range(cRange).
    chCell:VALUE  = molienda_silo.fecha.
    chCell        = chCell:NEXT.
    chCell:VALUE  = lineas_produccion.abreviatura.
    chCell        = chCell:NEXT.
    chCell:VALUE  = molienda_silo.id_turno.
    chCell        = chCell:NEXT.
    chCell:VALUE  = molienda_silo.responsable_turno.
    chCell        = chCell:NEXT.
    chCell:VALUE  = silo.descripcion.
    chCell        = chCell:NEXT.
    chCell:VALUE  = items_molienda_silo.hora_comienzo.
    chCell        = chCell:NEXT.
    chCell:VALUE  = items_molienda_silo.hora_termino.
    chCell        = chCell:NEXT.
    chCell:VALUE  = items_molienda_silo.kilos.
    chCell        = chCell:NEXT.
    chCell:VALUE  = items_molienda_silo.kilos_descarte.
    chCell        = chCell:NEXT.
    chCell:VALUE  = items_molienda_silo.kilos + items_molienda_silo.kilos_descarte.

    iFila = iFila + 1. 
   

  END.

  /*Actualizar Dinamicas*/
  chSheet = chApp:Sheets:ITEM(2).
  chSheet:PivotTables("Dinamica"):RefreshTable().
  chSheet = chApp:Sheets:ITEM(4).
  chSheet:PivotTables("DinamicaGrafico"):RefreshTable().

  RUN closeExcelApplication IN hLibCom (chApp).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelOEs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelOEs Procedure 
PROCEDURE exportExcelOEs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
 
  /* obtengo solo las fechas */
  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  FOR EACH ttOEs.
    DELETE ttOEs.
  END.

  FOR EACH orden_entrega NO-LOCK,
      EACH items_orden_entrega OF orden_entrega
      WHERE orden_entrega.fecha_embarque        >= dDes
        AND orden_entrega.fecha_embarque        <= dHas
        AND orden_entrega.id_tipo_orden_entrega  = 1
      NO-LOCK.
    
    FIND FIRST agencias OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST vapores OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST clausulas WHERE items_orden_entrega.id_condicion_venta = clausulas.id_clausula NO-LOCK NO-ERROR.
    FIND FIRST estados_oe OF items_orden_entrega NO-LOCK NO-ERROR.

    CREATE ttOes.
    ASSIGN  ttOes.id_orden_entrega    = orden_entrega.id_orden_entrega
            ttOes.ITEM_oe             = items_orden_entrega.ITEM_oe
            ttOes.semana              = orden_entrega.semana_embarque
            ttOes.vapor               = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NO-INFO"
            ttOes.cliente             = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NO-INFO"
            ttOes.articulo            = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-INFO"
            ttOes.calidad             = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-INFO"
            ttOes.tambores            = items_orden_entrega.tambores_pedidos
            ttOes.nro_of              = 0
            ttOes.estado              = IF AVAILABLE estados_oe THEN estados_oe.descripcion ELSE "NO-INFO"
            ttOes.fecha_embarque      = orden_entrega.fecha_embarque
            ttOes.agencia             = IF AVAILABLE agencias THEN agencias.descripcion ELSE "NO-INFO"
            ttOes.incoterm            = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "NO-INFO"
            ttOes.contenedores        = items_orden_entrega.contenedores
            .

  END.

  RUN generateExcel.p (INPUT TABLE ttOEs,
                      INPUT " OE's",
                      INPUT ""  ,
                      INPUT 7,
                      INPUT 8,
                      INPUT "Arial",
                      INPUT 8).

END PROCEDURE.


/* 
 
 DEFINE TEMP-TABLE ttOes
  RCODE-INFORMATION
  FIELD id_orden_entrega  AS INTEGER    COLUMN-LABEL "OE"
  FIELD ITEM_oe           AS INTEGER    COLUMN-LABEL "Parte"
  FIELD semana            AS INTEGER    COLUMN-LABEL "Semana"
  FIELD vapor             AS CHARACTER  COLUMN-LABEL "Vapor"
  FIELD cliente           AS CHARACTER  COLUMN-LABEL "Cliente"
  FIELD articulo          AS CHARACTER  COLUMN-LABEL "Articulo"
  FIELD calidad           AS CHARACTER  COLUMN-LABEL "Calidad"
  FIELD tambores          AS INTEGER    COLUMN-LABEL "Tambores"
  FIELD nro_of            AS INTEGER    COLUMN-LABEL "Nro OF"
  FIELD id_estado         AS INTEGER    COLUMN-LABEL "Estado"
  FIELD fecha_embarque    AS DATE       COLUMN-LABEL "Fecha Embarque"
  FIELD agencia           AS CHARACTER  COLUMN-LABEL "Agencia"
  FIELD incoterm          AS CHARACTER  COLUMN-LABEL "Incoterm"
  .*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelOrdenCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelOrdenCarga Procedure 
PROCEDURE exportExcelOrdenCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER ro FOR tambores_industria.


  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR iFila  AS INTEGER.
  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.

  

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplOrdenCarga.xls'). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

  FIND FIRST cargas WHERE cargas.id_empresa    = piEmp
                      AND cargas.id_sucursal   = piSuc
                      AND cargas.id_tipotambor = piTip
                      AND cargas.nromov        = piNro
                    NO-LOCK.
  IF NOT AVAILABLE cargas THEN
    RETURN.



  chWorkSheet:Range("D4"):Value  = "'" + STRING(cargas.id_carga) + "/" + STRING(cargas.anio).
  chWorkSheet:Range("D5"):Value  = "'" + STRING(cargas.fecha).
  chWorkSheet:Range("D6"):Value  = getDescArticulo(cargas.id_articulo).
  chWorkSheet:Range("D7"):Value  = getDescTanque(cargas.id_tanque).
  chWorkSheet:Range("D8"):Value  = "'" + STRING(cargas.litros).

  chWorkSheet:Range("D10"):Value = "'" + STRING(cargas.temperatura_carga).
  chWorkSheet:Range("D11"):Value = "'" + STRING(cargas.tipo_enzima).
  chWorkSheet:Range("D12"):Value = "'" + STRING(cargas.cantidad_enzima).
  chWorkSheet:Range("D13"):Value = "'" + STRING(cargas.tiempo_actividad_enzima).


  iFila = 19.
  j     = 1.
  FOR EACH composicion_carga WHERE composicion_carga.id_empresa     = cargas.id_empresa
                               AND composicion_carga.id_sucursal    = cargas.id_sucursal
                               AND composicion_carga.id_tipotambor  = cargas.id_tipotambor
                               AND composicion_carga.nromov         = cargas.nromov
                             NO-LOCK.
    
    cRange = "B" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(j).
    cRange = "C" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = getDescArticulo(composicion_carga.id_articulo).
    cRange = "D" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(composicion_carga.cantidad).
    chWorkSheet:Range(cRange):VALUE = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.
    cRange = "E" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = getDescUnidad(composicion_carga.id_unidad).
    cRange = "F" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = composicion_carga.observaciones.

    iFila = iFila + 1.
    j = j + 1.
    
  END.
 
  iFila = 34. 
  j     = 1.
  FOR EACH ro WHERE ro.nromov_destino = cargas.nromov
              BREAK BY ro.nromov.
    ASSIGN i = i + 1
           k = k + ro.kilos_tambor.
    IF LAST-OF(ro.nromov) THEN DO:
      cRange = "B" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = STRING(j).
      cRange = "C" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = getDescArticulo(ro.id_articulo).
      cRange = "D" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = STRING(ro.id_lote).
      cRange = "E" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = STRING(ro.anio).
      cRange = "F" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = STRING(i).
      chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):Value) * 1.
      cRange = "G" + STRING(iFila).
      chWorkSheet:Range(cRange):Value = STRING(k).
      chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.      
      
      iFila = iFila + 1.
      j = j + 1.
      
      ASSIGN i = 0
             k = 0.

    END.
  END.
    


  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelPerdida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelPerdida Procedure 
PROCEDURE exportExcelPerdida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VARIABLE dConv   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iCal    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArtOri AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArtDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dFecha  AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecRep AS DATE       NO-UNDO.
  
  DEFINE VAR viFila       AS INTEGER.
  DEFINE VAR vcFila       AS CHARACTER.
  DEFINE VAR vcRange      AS CHARACTER.

  DEFINE BUFFER buOri FOR tambores_industria.
  DEFINE BUFFER buDes FOR tambores_industria.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
    
  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplRepros.xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  

  viFila = 3.
  FOR EACH perdida_tbs_repro NO-LOCK.
    
    dFecRep = ?.
    dFecha  = ?.
    iArtDes = 0.
    IF perdida_tbs_repro.id_tipotambor_destino = 10 THEN
      FOR FIRST cargas WHERE cargas.nromov = perdida_tbs_repro.nromov_destino NO-LOCK.
        iArtDes = cargas.id_articulo.
        FIND FIRST proceso WHERE proceso.nromov = cargas.nromov_proceso NO-LOCK NO-ERROR.
        dFecha = IF AVAILABLE proceso THEN proceso.fecha_fin ELSE ?.
      END.
    ELSE DO:    
      FOR FIRST buDes WHERE buDes.nromov = perdida_tbs_repro.nromov_destino NO-LOCK.
        iArtDes = buDes.id_articulo.
        FIND FIRST lotes_jugo OF buDes NO-LOCK NO-ERROR.
        dFecha  = IF AVAILABLE lotes_jugo THEN lotes_jugo.fecha_finalizacion ELSE ?.
      END.
    END.

    iArtOri = 0.
    iCal    = 0.
    dConv   = 1.
    FOR FIRST buOri WHERE buOri.nromov = perdida_tbs_repro.nromov NO-LOCK.
      iArtOri = buOri.id_articulo.
      iCal    = buOri.id_calidad.
      dFecRep = buOri.fecha_reproceso.
      dConv   = DYNAMIC-FUNCTION('getCoefConversion400' IN hLib, buOri.id_tipotambor,
                                                                 buOri.id_articulo,
                                                                 buOri.id_calidad).
    END.

    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_sucursal_destino).
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = STRING(iArtOri).
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = getDescArticulo(iArtOri).
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_lote) + " / " + STRING(perdida_tbs_repro.anio) .
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.kilos.  /*STRING(perdida_tbs_repro.kilos_destino, ">>>>>>>9.99").*/
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.kilos * dConv.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = dConv.
    vcRange = "H" + STRING(viFila).    
    chWorkSheet:Range(vcRange):VALUE = STRING(iArtDes).
    vcRange = "I" + STRING(viFila).    
    chWorkSheet:Range(vcRange):VALUE = getDescArticulo(iArtDes).
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.tambores.
    vcRange = "K" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.repro_mismo_mes.
    vcRange = "L" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.id_tipotambor_destino.    
    vcRange = "M" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.nromov).
    vcRange = "N" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.nromov_destino).
    vcRange = "O" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_sucursal_destino).
    vcRange = "P" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecRep).
    vcRange = "Q" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecha).

    viFila = viFila + 1.
  END.
  
  RELEASE perdida_tbs_repro.
  
  
  
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("Dinamica"):RefreshTable().
  
  /*chExcelAplication:VISIBLE = TRUE.*/
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelProceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelProceso Procedure 
PROCEDURE exportExcelProceso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLiR AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLiC AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGpl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGp2 AS CHARACTER  NO-UNDO.


  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR iFila  AS INTEGER.
  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplRptProceso.xls'). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

  FIND FIRST proceso WHERE proceso.id_empresa    = piEmp
                       AND proceso.id_sucursal   = piSuc
                       AND proceso.id_tipotambor = piTip
                       AND proceso.nromov        = piNro.
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE proceso THEN
     RETURN.

  cPro                          = "CLARIFICADO PLANTA " + CAPS(getDescSucursal(piSuc)) + " Proceso N° " + STRING(proceso.id_proceso) + "/" + STRING(proceso.anio).                                                        
  chWorkSheet:Range("B1"):Value =  cPro.
  chWorkSheet:Range("H1"):Value =  STRING(proceso.fecha_inicio).
  chWorkSheet:Range("H2"):Value =  STRING(proceso.fecha_fin).

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  iFila = 7.
  dKgs  = 0.00.
  FOR EACH cargas WHERE cargas.id_empresa_proceso     = piEmp
                    AND cargas.id_sucursal_proceso    = piSuc
                    AND cargas.id_tipotambor_proceso  = piTip 
                    AND cargas.nromov_proceso         = piNro
                  BY cargas.id_carga.

    dCoe = DYNAMIC-FUNCTION('getCoefSolidosSolubles' IN hLib, cargas.bx_correg).
    /*cGpl = DYNAMIC-FUNCTION('getAporteGPLRepro' IN hLib, cargas.id_empresa, cargas.id_sucursal, cargas.id_tipotambor, cargas.nromov).
    cGp2 = DYNAMIC-FUNCTION('getAporteGPLNoRepro' IN hLib, cargas.id_empresa, cargas.id_sucursal, cargas.id_tipotambor, cargas.nromov).
    dLit = DECIMAL(ENTRY(1, cGpl, CHR(1))) + DECIMAL(ENTRY(1, cGp2, CHR(1))).*/
    /*dSol = DECIMAL(ENTRY(2, cGpl, CHR(1))) + DECIMAL(ENTRY(2, cGp2, CHR(1))).*/
    /*dKgs = DECIMAL(ENTRY(3, cGpl, CHR(1))) + DECIMAL(ENTRY(3, cGp2, CHR(1))).*/
    dLit = cargas.litros.
    dSol = dCoe * cargas.litros.
    dKgs = DYNAMIC-FUNCTION('getKilos400GplCarga' IN hLib, cargas.id_empresa,
                                                           cargas.id_sucursal,
                                                           cargas.id_tipotambor,
                                                           cargas.nromov).
    
    dLit = ROUND(dLit, 2).
    dSol = ROUND(dSol, 2).
    dKgs = ROUND(dKgs, 2).

    IF dLit <> 0 AND dCoe <> 0 AND dSol = 0 THEN DO: /*esto se cumple cuando hacen una carga con agua y jugo dulce*/
      dSol = dLit * dCoe.
    END.
    /*
    dKgs = DYNAMIC-FUNCTION('getKilos400GplCarga' IN hLib, cargas.id_empresa,
                                                           cargas.id_sucursal,
                                                           cargas.id_tipotambor,
                                                           cargas.nromov).
    */
    cRange = "B" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(cargas.id_carga).
    cRange = "C" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(cargas.bx_correg, ">>9.99").
    chWorkSheet:Range(cRange):VALUE = DECIMAL(chWorkSheet:Range(cRange):Value) * 1.
    cRange = "D" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dCoe, ">>9.99").
    chWorkSheet:Range(cRange):VALUE = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.
    cRange = "E" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dLit, ">>>,>>>,>>9.99").
    cRange = "F" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dSol, ">>>,>>>,>>9.99").
    chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.
    cRange = "G" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(cargas.id_tanque).
    cRange = "H" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dKgs, ">>>,>>>,>>9.99").
    chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.

    iFila = iFila + 1.
    dLit  = 0.
    
  END.


  iFila = 24.
  dKgs  = 0.00.
  FOR EACH punto_envase WHERE punto_envase.id_empresa_proceso     = piEmp
                          AND punto_envase.id_sucursal_proceso    = piSuc
                          AND punto_envase.id_tipotambor_proceso  = piTip
                          AND punto_envase.nromov_proceso         = piNro
                        BY punto_envase.id_punto_envase.
    dCoe = DYNAMIC-FUNCTION('getCoefSolidosSolubles' IN hLib, punto_envase.bx_correg).
    dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, punto_envase.bx_correg, punto_envase.litros).

    FOR FIRST tambores_industria WHERE punto_envase.nromov = tambores_industria.nromov
                                 NO-LOCK.
      cLot = STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio).
      IF tambores_industria.id_tipotambor = 3 THEN DO:      
        cTip = "L".
        cAnl = getValoresAnalisis(tambores_industria.id_empresa,
                                  tambores_industria.id_sucursal,
                                  tambores_industria.id_tipotambor,
                                  tambores_industria.nromov).
      END.
      IF tambores_industria.id_tipotambor = 1 THEN DO:      
        cTip = "P".
     
        cAnl = getValoresAnalisisProduccion(tambores_industria.id_empresa,
                                            tambores_industria.id_sucursal,
                                            tambores_industria.id_tipotambor,
                                            tambores_industria.nromov).
      END.
    END.

    dKgs = 0.00.
    IF DECIMAL(ENTRY(2, cAnl, CHR(1))) <> 0 THEN
      dKgs = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                                            DECIMAL(ENTRY(3, cAnl, CHR(1))), 
                                                            DECIMAL(ENTRY(6, cAnl, CHR(1)))).      
    ELSE 
      dKgs = DYNAMIC-FUNCTION('getKilosFromBxCorreg' IN hLib, DECIMAL(ENTRY(4, cAnl, CHR(1))), 
                                                              DECIMAL(ENTRY(6, cAnl, CHR(1)))).
    
    

    
    /*kilos 400*/    
    dKgs = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, punto_envase.bx_20_20, 
                                                      punto_envase.acidez_w_w, 
                                                      punto_envase.litros, 
                                                      FALSE).      
    


    /*
    dKgs = getKilos400(punto_envase.id_tipotambor,
                       punto_envase.id_articulo,
                       punto_envase.id_calidad,
                       dKgs).*/
          
    cRange = "B" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(punto_envase.id_punto_envase).
    cRange = "C" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(punto_envase.bx_correg, ">>9.99").
    chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):Value) * 1.
    cRange = "D" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dCoe, ">>>,>>>,>>9.99").
    chWorkSheet:Range(cRange):VALUE = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.
    cRange = "E" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(punto_envase.litros).
    cRange = "F" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dSol, ">>>,>>>,>>9.99").
    chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):Value) * 1.
    cRange = "G" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = "' " + cTip + "(" + cLot + ")".
    cRange = "H" + STRING(iFila).
    chWorkSheet:Range(cRange):Value = STRING(dKgs, ">>>,>>>,>>9.99").
    chWorkSheet:Range(cRange):Value = DECIMAL(chWorkSheet:Range(cRange):VALUE) * 1.
    
    iFila = iFila + 1.
    
  END.

  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelProduccion Procedure 
PROCEDURE exportExcelProduccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSemana        AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcStreamJugo    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcStreamAceite  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcStreamCascara AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcStreamFruta   AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR iFila  AS INTEGER.
  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplProduccionSemana.xls'). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).


  chWorkSheet:Range("B2"):Value  = "Informe Produccion Semana " + STRING(piSemana).

  /*jugo*/
  cRow    = ENTRY(1, pcStreamJugo, CHR(10)).
  chWorkSheet:Range("C6"):Value  = DECIMAL(ENTRY(1, cRow, CHR(1))).
  chWorkSheet:Range("D6"):Value  = DECIMAL(ENTRY(2, cRow, CHR(1))).
  cRow    = ENTRY(2, pcStreamJugo, CHR(10)).
  chWorkSheet:Range("C7"):Value  = DECIMAL(ENTRY(1, cRow, CHR(1))).
  chWorkSheet:Range("D7"):Value  = DECIMAL(ENTRY(2, cRow, CHR(1))).
  cRow    = ENTRY(3, pcStreamJugo, CHR(10)).
  chWorkSheet:Range("C8"):Value  = DECIMAL(ENTRY(1, cRow, CHR(1))).
  chWorkSheet:Range("D8"):Value  = DECIMAL(ENTRY(2, cRow, CHR(1))).
  cRow    = ENTRY(4, pcStreamJugo, CHR(10)).
  chWorkSheet:Range("C9"):Value  = DECIMAL(ENTRY(1, cRow, CHR(1))).
  chWorkSheet:Range("D9"):Value  = DECIMAL(ENTRY(2, cRow, CHR(1))).


  
  /*aceite*/  
  chWorkSheet:Range("C15"):Value  = DECIMAL(ENTRY(1, pcStreamAceite, CHR(10))).
  chWorkSheet:Range("C16"):Value  = DECIMAL(ENTRY(2, pcStreamAceite, CHR(10))).
  chWorkSheet:Range("C17"):Value  = DECIMAL(ENTRY(3, pcStreamAceite, CHR(10))).
  chWorkSheet:Range("C18"):Value  = DECIMAL(ENTRY(4, pcStreamAceite, CHR(10))).


  
  /*cascara*/  
  chWorkSheet:Range("C24"):Value  = DECIMAL(ENTRY(1, pcStreamCascara, CHR(1))).
  chWorkSheet:Range("C25"):Value  = DECIMAL(ENTRY(2, pcStreamCascara, CHR(1))).

  
  /*fruta*/
  chWorkSheet:Range("C30"):Value  = DECIMAL(ENTRY(1, pcStreamFruta, CHR(1))).
  chWorkSheet:Range("C31"):Value  = DECIMAL(ENTRY(2, pcStreamFruta, CHR(1))).


  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelReproc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelReproc Procedure 
PROCEDURE exportExcelReproc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR vcRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\dinamicaReproc(RO).xls').
  /*chWorkbook  = chExcelAplication:Workbooks:ADD().*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  /*
  chWorkSheet:Range("A1:AM1"):Font:Bold = TRUE.
  
  chWorkSheet:Range("A1:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.
   
  chWorkSheet:Range("A1"):Value = "Lote Destino".
  chWorkSheet:Range("A1:A1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("A"):ColumnWidth = 10.

  chWorkSheet:Range("B1"):Value = "Anio Destino".
  chWorkSheet:Range("B1:B1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("B"):ColumnWidth = 10.

  chWorkSheet:Range("C1"):Value = "Tambores Destino".
  chWorkSheet:Range("C1:C1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("C"):ColumnWidth = 10.

  chWorkSheet:Range("D1"):Value = "Kgs Destino".
  chWorkSheet:Range("D1:D1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("D"):ColumnWidth = 10.
  
  chWorkSheet:Range("E1"):Value = "Articulo Destino".
  chWorkSheet:Range("E1:E1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("E"):ColumnWidth = 10.
  
  chWorkSheet:Range("F1"):Value = "Fecha Destino".
  chWorkSheet:Range("F1:F1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("F"):ColumnWidth = 10.
  
  chWorkSheet:Range("G1"):Value = "Sucursal Destino".
  chWorkSheet:Range("G1:G1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("G"):ColumnWidth = 10.
  
  chWorkSheet:Range("H1"):Value = "Lote".
  chWorkSheet:Range("H1:H1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("H"):ColumnWidth = 10.

  chWorkSheet:Range("I1"):Value = "Anio".
  chWorkSheet:Range("I1:I1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("I"):ColumnWidth = 10.

  chWorkSheet:Range("J1"):Value = "Tambores".
  chWorkSheet:Range("J1:J1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("J"):ColumnWidth = 10.

  chWorkSheet:Range("K1"):Value = "Kgs".
  chWorkSheet:Range("K1:K1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("K"):ColumnWidth = 10.
  
  chWorkSheet:Range("L1"):Value = "Articulo".
  chWorkSheet:Range("L1:L1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("L"):ColumnWidth = 10.
  
  chWorkSheet:Range("M1"):Value = "Fecha".
  chWorkSheet:Range("M1:M1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("M"):ColumnWidth = 10.

  chWorkSheet:Range("N1"):Value = "TipoTambor Destino".
  chWorkSheet:Range("N1:N1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("N"):ColumnWidth = 10.

  chWorkSheet:Range("O1"):Value = "NroMov Destino".
  chWorkSheet:Range("O1:O1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("O"):ColumnWidth = 10.

  chWorkSheet:Range("P1"):Value = "NroMov".
  chWorkSheet:Range("P1:P1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("P"):ColumnWidth = 10.

  chWorkSheet:Range("Q1"):Value = "Tipo Tambor".
  chWorkSheet:Range("Q1:Q1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("Q"):ColumnWidth = 10.

  chWorkSheet:Range("R1"):Value = "Condicion Destino".
  chWorkSheet:Range("R1:R1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("R"):ColumnWidth = 10.
  */

  viFila = 2.
  FOR EACH compo_lote_jugo NO-LOCK.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.id_lote_destino) + " / " + STRING(compo_lote_jugo.anio_lote_destino) .
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.anio_lote_destino.
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tambores_destino.
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.kilos_destino.  /*STRING(compo_lote_jugo.kilos_destino, ">>>>>>>9.99").*/
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.articulo_destino.
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.calidad_destino, "99/99/9999").
    vcRange = "G" + STRING(viFila).    
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.fecha_destino, "99/99/9999").
    vcRange = "H" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.id_sucursal_destino.
    vcRange = "I" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.id_lote) + " / " + STRING(compo_lote_jugo.anio_lote).
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.anio_lote.
    vcRange = "K" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tambores.
    vcRange = "L" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.kilos.  /*STRING(compo_lote_jugo.kilos, ">>>>>>>9.99").*/
    vcRange = "M" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.articulo.
    vcRange = "N" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.calidad.
    vcRange = "O" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.fecha, "99/99/9999").
    vcRange = "P" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tipotambor_destino.
    vcRange = "Q" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.nromov_destino).
    vcRange = "R" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(compo_lote_jugo.nromov).
    vcRange = "S" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.tipotambor.
    vcRange = "T" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = compo_lote_jugo.condicion.


    viFila = viFila + 1.
  END.
  RELEASE compo_lote_jugo.

  
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("dinamicakgs"):RefreshTable().
  chWorkSheet = chExcelAplication:Sheets:ITEM(3).
  chWorkSheet:PivotTables("dinamicatbs"):RefreshTable().
  
  /*chExcelAplication:VISIBLE = TRUE.*/
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportExcelReprocesos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportExcelReprocesos Procedure 
PROCEDURE exportExcelReprocesos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VARIABLE dConv   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iCal    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArtOri AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArtDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dFecha  AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecRep AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecAju AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecCre AS DATE       NO-UNDO.
  DEFINE VARIABLE dFecRes AS DATE       NO-UNDO.
  DEFINE VARIABLE cLotPro AS CHARACTER  NO-UNDO.
  
  DEFINE VAR viFila       AS INTEGER.
  DEFINE VAR vcFila       AS CHARACTER.
  DEFINE VAR vcRange      AS CHARACTER.

  DEFINE BUFFER buOri FOR tambores_industria.
  DEFINE BUFFER buDes FOR tambores_industria.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
    
  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplRepros.xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  

  viFila = 3.
  FOR EACH perdida_tbs_repro NO-LOCK.
    
    dFecRep = ?.
    dFecha  = ?.
    dFecAju = ?.
    iArtDes = 0.
    cLotPro = "".
    IF perdida_tbs_repro.id_tipotambor_destino = 10 THEN
      FOR FIRST cargas WHERE cargas.nromov = perdida_tbs_repro.nromov_destino NO-LOCK.
        iArtDes = cargas.id_articulo.
        FIND FIRST proceso WHERE proceso.nromov = cargas.nromov_proceso NO-LOCK NO-ERROR.
        dFecha  = IF AVAILABLE proceso THEN proceso.fecha_fin ELSE ?.
        IF AVAILABLE proceso THEN DO: 
          cLotPro ="Proceso:" + STRING(proceso.id_proceso) + "/" + STRING(proceso.anio) + " - Producciones Resultantes: " .
          dFecCre = proceso.fecha_inicio.
          FOR EACH r_proceso_tambor
              WHERE r_proceso_tambor.nromov_proceso = proceso.nromov
              BREAK BY r_proceso_tambor.nromov.
            IF LAST-OF(r_proceso_tambor.nromov) THEN DO:     
              FIND FIRST tambores_industria WHERE tambores_industria.nromov = r_proceso_tambor.nromov NO-LOCK NO-ERROR.
              IF AVAILABLE tambores_industria THEN 
                cLotPro = cLotPro +
                        STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio) + ", ".
                dFecRes = tambores_industria.fecha.
            END.
          END.
          cLotPro = SUBSTRING(cLotPro, 1, LENGTH(cLotPro) - 2).
        END.
                  
      END.
    ELSE DO:    
      FOR FIRST buDes WHERE buDes.nromov = perdida_tbs_repro.nromov_destino NO-LOCK.
        iArtDes = buDes.id_articulo.
        FIND FIRST lotes_jugo WHERE lotes_jugo.nromov = buDes.nromov NO-LOCK NO-ERROR.
        dFecha  = IF AVAILABLE lotes_jugo THEN lotes_jugo.fecha_finalizacion ELSE ?.
        cLotPro = IF AVAILABLE lotes_jugo THEN "Lote: " + STRING(lotes_jugo.id_lote) + "/" + STRING(lotes_jugo.anio) ELSE "".
        dFecCre = IF AVAILABLE lotes_jugo THEN lotes_jugo.fecha_comienzo_envase ELSE ?.
        dFecRes = dFecCre.
      END.
    END.

    iArtOri = 0.
    iCal    = 0.
    dConv   = 1.
    FOR FIRST buOri WHERE buOri.nromov = perdida_tbs_repro.nromov NO-LOCK.
      iArtOri = buOri.id_articulo.
      iCal    = buOri.id_calidad.
      dConv   = DYNAMIC-FUNCTION('getCoefConversion400' IN hLib, buOri.id_tipotambor,
                                                                 buOri.id_articulo,
                                                                 buOri.id_calidad).
    END.

    /* obtengo fecha de volcado (reproceso) */
    FIND FIRST tambores_industria
         WHERE tambores_industria.nromov         = perdida_tbs_repro.nromov
           AND tambores_industria.nromov_destino = perdida_tbs_repro.nromov_destino
         NO-LOCK NO-ERROR.
    dFecRep = IF AVAILABLE tambores_industria THEN tambores_industri.fecha_reproceso ELSE ?.

    /* obtengo fecha de ajuste */
    FOR FIRST perdida_cargas
         WHERE perdida_cargas.nromov = perdida_tbs_repro.nromov_destino
         NO-LOCK.
      dFecAju = perdida_cargas.fecha_ajuste.
    END.


    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_sucursal_destino).
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = STRING(iArtOri).
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = getDescArticulo(iArtOri).
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_lote) + " / " + STRING(perdida_tbs_repro.anio) .
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.kilos.  /*STRING(perdida_tbs_repro.kilos_destino, ">>>>>>>9.99").*/
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.kilos * dConv.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = dConv.
    vcRange = "H" + STRING(viFila).    
    chWorkSheet:Range(vcRange):VALUE = STRING(iArtDes).
    vcRange = "I" + STRING(viFila).    
    chWorkSheet:Range(vcRange):VALUE = getDescArticulo(iArtDes).
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.tambores.
    vcRange = "K" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.repro_mismo_mes.
    vcRange = "L" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = perdida_tbs_repro.id_tipotambor_destino.    
    vcRange = "M" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.nromov).
    vcRange = "N" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.nromov_destino).
    vcRange = "O" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(perdida_tbs_repro.id_sucursal_destino).
    vcRange = "P" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecRep).
    vcRange = "Q" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecha).
    vcRange = "R" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecAju).
    vcRange = "S" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = cLotPro.
    vcRange = "T" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecCre).
    vcRange = "U" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = "'" + STRING(dFecRes).

    viFila = viFila + 1.
  END.
  
  RELEASE perdida_tbs_repro.
  
  
  
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("Dinamica"):RefreshTable().
  
  /*chExcelAplication:VISIBLE = TRUE.*/
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportProdLotesCascara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportProdLotesCascara Procedure 
PROCEDURE exportProdLotesCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDes AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHas AS DATE       NO-UNDO.

  FOR EACH produccion_cascara
      WHERE produccion_cascara.fecha >= pdDes
        AND produccion_cascara.fecha <= pdHas
      NO-LOCK.
    FOR EACH r_produccion_cascara_lote
        WHERE r_produccion_cascara_lote.id_sucursal_prod    = produccion_cascara.id_sucursal
          AND r_produccion_cascara_lote.id_produccion_prod  = produccion_cascara.id_produccion
        NO-LOCK.

      FIND FIRST lotes_cascara WHERE lotes_cascara.nromov = r_produccion_cascara_lote.nromov_lote NO-LOCK NO-ERROR.
      
      CREATE ttProdLoteCascara.
      ASSIGN  ttProdLoteCascara.id_produccion = produccion_cascara.id_produccion
              ttProdLoteCascara.anio_prod     = produccion_cascara.anio
              ttProdLoteCascara.cantidad_pro  = produccion_cascara.cantidad
              ttProdLoteCascara.kilos_prod    = produccion_cascara.cantidad * 50
              ttProdLoteCascara.fecha_asoc    = r_produccion_cascara_lote.c_fecha
              ttProdLoteCascara.cantidad_asoc = r_produccion_cascara.cantidad
              ttProdLoteCascara.kilos_asoc    = r_produccion_cascara.cantidad * 50
              ttProdLoteCascara.id_lote       = lotes_cascara.id_lote
              ttProdLoteCascara.anio_lote     = lotes_cascara.anio
              .
    END.

  END.

  RUN generateExcel.p (INPUT TABLE ttProdLoteCascara,
                        INPUT " Formacion Lotes Cascara",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).

END PROCEDURE.

/* FIELD id_produccion       AS INTEGER      COLUMN-LABEL "Produccion"
  FIELD anio_prod           AS INTEGER      COLUMN-LABEL "Anio Prod"
  FIELD cantidad_prod       AS INTEGER      COLUMN-LABEL "Cantidad Prod"
  FIELD kilos_prod          AS DECIMAL      COLUMN-LABEL "Kgs Prod"
  FIELD fecha_asoc          AS DATE         COLUMN-LABEL "Fecha Asoc"
  FIELD cantidad_asoc       AS INTEGER      COLUMN-LABEL "Cantidad Asoc"
  FIELD kilos_asoc          AS DECIMAL      COLUMN-LABEL "Kgs Asoc"
  FIELD id_lote             AS INTEGER      COLUMN-LABEL "Lote"
  FIELD anio_lote           AS INTEGER      COLUMN-LABEL "Anio Lote"
  FIELD cantidad_lote       AS INTEGER      COLUMN-LABEL "Cantidad Lote"
  FIELD kilos_lote          AS DECIMAL      COLUMN-LABEL "Kgs Lote"
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportRemitoFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportRemitoFactura Procedure 
PROCEDURE exportRemitoFactura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
 
  FOR EACH ttFacturaRemito.
    DELETE ttFacturaRemito.
  END.
  

  FOR EACH remitos
      WHERE remitos.id_operacion = 311
        AND remitos.fecha       >= pdDesde
        AND remitos.fecha       <= pdHasta
        AND (IF piSuc <> 0 THEN remitos.id_sucursal = piSuc ELSE TRUE)
      NO-LOCK, 
      EACH items_factura
        OF remitos 
      NO-LOCK.

    FIND FIRST productos_terminados OF items_factura NO-LOCK NO-ERROR.
    FIND FIRST calidades OF items_factura NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF items_factura NO-LOCK NO-ERROR.
    FIND FIRST clientes OF remitos NO-LOCK NO-ERROR.
    FIND FIRST destinos OF remitos NO-LOCK NO-ERROR.
    FIND FIRST proveedores OF remitos NO-LOCK NO-ERROR.
    FIND FIRST vapores OF remitos NO-LOCK NO-ERROR.
    FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
    
    fKil = IF items_factura.id_tipotambor = 11 THEN items_factura.peso * items_factura.cantidad ELSE items_factura.peso.
   
    CREATE ttFacturaRemito.
    ASSIGN  ttFacturaRemito.fecha_remito  = remitos.fecha
            ttFacturaRemito.nro_remito    = string(remitos.nro_comp, "9999-99999999")
            ttFacturaRemito.lote          = integer(entry(1, string(items_factura.nro_lote), "/"))
            ttFacturaRemito.anio          = integer(entry(2, string(items_factura.nro_lote), "/"))
            ttFacturaRemito.articulo      = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
            ttFacturaRemito.calidad       = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
            ttFacturaRemito.envase        = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
            ttFacturaRemito.destino       = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
            ttFacturaRemito.transporte    = IF AVAILABLE proveedores THEN proveedores.razon_social ELSE "NONE"
            ttFacturaRemito.vapor         = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NONE"
            ttFacturaRemito.cliente       = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE"
            ttFacturaRemito.orden_fab     = remitos.orden_fabricacion
            ttFacturaRemito.kilos_400     = DYNAMIC-FUNCTION('getKilos400' IN hLib, items_factura.id_tipotambor, items_factura.id_articulo, items_factura.id_calidad, fKil)
            ttFacturaRemito.kilos_neto    = fKil
            ttFacturaRemito.cantidad      = items_factura.cantidad
            ttFacturaRemito.parte         = items_factura.ITEM
            ttFacturaRemito.lugdes        = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "NONE"
            ttFacturaRemito.id_suc_des    = IF AVAILABLE lugar_descarga THEN lugar_descarga.id_sucursal ELSE 0
            ttFacturaRemito.permiso       = remitos.nro_per_embarque
            ttFacturaRemito.estado        = IF remitos.estado THEN "VIGENTE" ELSE "ANULADO"
            .
            


  END.


  RUN generateExcel.p (INPUT TABLE ttFacturaRemito,
                        INPUT " Remitos",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportTablaBrix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportTablaBrix Procedure 
PROCEDURE exportTablaBrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH ttBrix.
    DELETE ttBrix.
  END.

  FOR EACH brix
      NO-LOCK.
    CREATE ttBrix.
    ASSIGN  ttBrix.brix = brix.brix
            ttBrix.pesp = brix.pe
            ttBrix.sols = brix.solido_soluble.
  END.

  RUN generateExcel.p (INPUT TABLE ttBrix,
                        INPUT " Tabla Relacion Brix - Peso Especifico - Solidos Solubles",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillCabeceraAjustes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillCabeceraAjustes Procedure 
PROCEDURE fillCabeceraAjustes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
        
            
  FOR EACH registro_post_cierre
      WHERE registro_post_cierre.fecha >= pdDesde
        AND registro_post_cierre.fecha <= pdHasta
      NO-LOCK.
    
    IF registro_post_cierre.id_tipotambor <> 10 THEN DO:
      FOR FIRST lotes_jugo
          WHERE lotes_jugo.nromov              = registro_post_cierre.nromov
          NO-LOCK.

            RUN addPerdidaCarga(ROWID(lotes_jugo)).
            /* pongo fecha ajuste en cabecera de repro */
            FIND LAST perdida_cargas
                 WHERE perdida_cargas.nromov = registro_post_cierre.nromov
                 NO-ERROR.
             IF AVAILABLE perdida_cargas THEN
               perdida_cargas.fecha_ajuste = registro_post_cierre.fecha.

      END.
    END.
  
    IF registro_post_cierre.id_tipotambor = 13 THEN DO:
      FOR FIRST proceso
          WHERE proceso.nromov     = registro_post_cierre.nromov.

          FOR EACH cargas
              WHERE cargas.nromov_proceso = proceso.nromov.
            
            FOR FIRST tambores_industria
                WHERE tambores_industria.nromov_destino   = cargas.nromov
                  AND tambores_industria.fecha_reproceso >= registro_post_cierre.fecha
                NO-LOCK. 
              RUN addPerdidaCarga(ROWID(cargas)).

              /* pongo fecha ajuste en cabecera de repro */
              FIND LAST perdida_cargas
                   WHERE perdida_cargas.nromov = cargas.nromov
                   NO-ERROR.
              IF AVAILABLE perdida_cargas THEN
                 perdida_cargas.fecha_ajuste = registro_post_cierre.fecha.

         
            END.
          END.
  
      END.
    END.

   
  
      
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillCabeceraReprocesos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillCabeceraReprocesos Procedure 
PROCEDURE fillCabeceraReprocesos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.

  FOR EACH proceso
      WHERE proceso.fecha_fin >= pdDesde
        AND proceso.fecha_fin <= pdHasta
        AND (IF piSuc <> 0 THEN proceso.id_sucursal = piSuc ELSE TRUE)
      NO-LOCK.

    FOR EACH cargas
      WHERE cargas.nromov_proceso = proceso.nromov
      NO-LOCK.

      RUN addPerdidaCarga(ROWID(cargas)).
    END.
    
 END.

  FOR EACH lotes_jugo 
      WHERE lotes_jugo.fecha_finalizacion >= pdDesde
        AND lotes_jugo.fecha_finalizacion <= pdHasta /*considera solo lotes cerrados al momento del listado*/                        
        AND (IF piSuc <> 0 THEN lotes_jugo.id_sucursal = piSuc ELSE TRUE)
      NO-LOCK.

    RUN addPerdidaCarga(ROWID(lotes_jugo)).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillPerdidaComposicionCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillPerdidaComposicionCarga Procedure 
PROCEDURE fillPerdidaComposicionCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  /*recupero los productos que componen las cargas que no son tambores reprocesados*/
  FOR EACH composicion_carga WHERE composicion_carga.id_empresa     = piEmp
                               AND composicion_carga.id_sucursal    = piSuc
                               AND composicion_carga.id_tipotambor  = piTip
                               AND composicion_carga.nromov         = piNro
                             NO-LOCK.
    RUN addPerdidaComposicionCarga(ROWID(composicion_carga)).      
  END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillPerdidaPtoEnv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillPerdidaPtoEnv Procedure 
PROCEDURE fillPerdidaPtoEnv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  
  
  /*obtengo productos terminados en el periodo*/
  /*recupero las producciones que se crearon como resultado de los procesos entre las fechas parametro*/
  FOR EACH punto_envase WHERE punto_envase.fecha >= pdDesde
                          AND punto_envase.fecha <= pdHasta
                          AND (IF piSuc <> 0 THEN punto_envase.id_sucursal = piSuc ELSE TRUE)
                        NO-LOCK.
    RUN addPerdidaPuntoEnvase(ROWID(punto_envase)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillPerdidaReprocesosRebatcheo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillPerdidaReprocesosRebatcheo Procedure 
PROCEDURE fillPerdidaReprocesosRebatcheo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buLotes FOR tambores_industria.

  FOR EACH buLotes WHERE buLotes.fecha_reproceso        >= pdDesde
                     AND buLotes.fecha_reproceso        <= pdHasta
                     AND buLotes.nromov_destino         <> 0
                     AND buLotes.id_tipotambor_destino  <> 10      /*no considera tbs reprocesados en cargas*/
                     AND buLotes.id_tipotambor_destino  <> 6
                     AND (buLotes.id_tipotambor          = 1
                      OR buLotes.id_tipotambor           = 3
                      OR buLotes.id_tipotambor           = 4
                      OR buLotes.id_tipotambor           = 5
                      OR buLotes.id_tipotambor           = 9)
                     AND (IF piSuc <> 0 THEN buLotes.id_sucursal = piSuc ELSE TRUE)
                   BREAK BY buLotes.nromov.
    i = i + 1.
    k = k + buLotes.kilos_tambor.
    IF LAST-OF(buLotes.nromov) THEN DO:
      RUN addPerdidaTamboresRepro(ROWID(buLotes), i, 0, k, FALSE).        
      i = 0.
      k = 0.
    END.
    
  END.

END PROCEDURE.

/*

                     AND buLotes.id_tipotambor          <> 2       /*no considera tbs de produccion de aceite*/
                     AND buLotes.id_tipotambor          <> 6       /*no considera lotes de aceite*/
                     AND buLotes.id_tipotambor          <> 7       /*no considera tambores foldeado*/
                     AND buLotes.id_tipotambor          <> 8       /*no considera tbs de sobrante aceite*/

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillProduccionesIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillProduccionesIndustria Procedure 
PROCEDURE fillProduccionesIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta      AS DATE       NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTamboresOF FOR tambores_industria.

  FOR EACH buTamboresOF WHERE buTamboresOF.id_sucursal   = piSucUbi
                          AND buTamboresOF.id_tipotambor = piTipoTambor
                          AND (IF piArticulo <> 0     THEN buTamboresOF.id_articulo = piArticulo ELSE TRUE)
                          AND (IF pdDesde <> DATE("") THEN buTamboresOF.fecha      >= pdDesde    ELSE TRUE)
                          AND (IF pdHasta <> DATE("") THEN buTamboresOF.fecha      <= pdHasta    ELSE TRUE)
                        BREAK BY YEAR(buTamboresOf.fecha) 
                              BY buTamboresOf.id_lote
                              BY buTamboresOf.id_articulo
                              BY buTamboresOf.id_envase.
    i = i + 1.
    k = k + buTamboresOF.kilos_tambor.    
    IF LAST-OF(buTamboresOF.id_envase) THEN DO:     
      IF getExisteCabecera(ROWID(buTamboresOF)) THEN
        RUN addProduccionIndustria(ROWID(buTamboresOF), i, k).
      i = 0.
      k = 0.
    END.
  END.

  IF piTipoTambor = 12 THEN DO: /*cascara*/
    FOR EACH produccion_cascara WHERE produccion_cascara.id_sucursal = piSucUbi
                                  AND (IF piArticulo <> 0     THEN produccion_cascara.id_articulo = piArticulo ELSE TRUE)
                                  AND (IF pdDesde <> DATE("") THEN produccion_cascara.fecha >= pdDesde           ELSE TRUE)
                                  AND (IF pdHasta <> DATE("") THEN produccion_cascara.fecha <= pdHasta           ELSE TRUE)
                                NO-LOCK.
      RUN addProduccionIndustria(ROWID(produccion_cascara), 0, 0).
    END.
  END.

  IF piTipoTambor = 11 THEN DO: /*cascara*/
    FOR EACH lotes_cascara WHERE lotes_cascara.id_sucursal = piSucUbi
                             AND (IF piArticulo <> 0     THEN lotes_cascara.id_articulo = piArticulo ELSE TRUE)
                             AND (IF pdDesde <> DATE("") THEN lotes_cascara.fecha >= pdDesde         ELSE TRUE)
                             AND (IF pdHasta <> DATE("") THEN lotes_cascara.fecha <= pdHasta         ELSE TRUE)
                           NO-LOCK.
      RUN addProduccionIndustria(ROWID(lotes_cascara), 0, 0).
      
    END.
    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillProduccionIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillProduccionIndustria Procedure 
PROCEDURE fillProduccionIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta      AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER plRepro      AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER plLotPro     AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTamboresOF FOR tambores_industria.

  
  FOR EACH buTamboresOF WHERE buTamboresOF.id_tipotambor          = piTipoTambor
                          AND buTamboresOF.id_estado             <> 7
                          AND buTamboresOF.id_tipotambor         <> 3 /*no considera lotes de jugo*/
                          AND (IF NOT plRepro         THEN buTamboresOF.id_locacion_ubicacion <> 10 ELSE TRUE)
                          AND (IF piSucUbi <> 0       THEN buTamboresOF.id_sucursal = piSucUbi      ELSE TRUE)  
                          AND (IF piArticulo <> 0     THEN buTamboresOF.id_articulo = piArticulo    ELSE TRUE)
                          AND (IF pdDesde NE DATE("") THEN buTamboresOF.fecha      >= pdDesde       ELSE TRUE)
                          AND (IF pdHasta NE DATE("") THEN buTamboresOF.fecha      <= pdHasta       ELSE TRUE)
                        BREAK BY buTamboresOF.nromov BY buTamboresOF.id_estado.
    i = i + 1.
    k = k + buTamboresOF.kilos_tambor.    
    IF LAST-OF(buTamboresOF.nromov) AND LAST-OF(buTamboresOF.id_estado) THEN DO:     
      IF getExisteCabecera(ROWID(buTamboresOF)) THEN
        RUN addProduccionIndustria(ROWID(buTamboresOF), i, k).
      i = 0.
      k = 0.
    END.
  END.



  /*****/
  FOR EACH lotes_jugo
      WHERE (IF pdDesde NE DATE("") THEN lotes_jugo.fecha_finalizacion >= pdDesde ELSE TRUE)
        AND (IF pdHasta NE DATE("") THEN lotes_jugo.fecha_finalizacion <= pdHasta ELSE TRUE), 
      EACH buTamboresOF 
         OF lotes_jugo
      WHERE buTamboresOF.id_tipotambor          = piTipoTambor
        AND buTamboresOF.id_estado             <> 7
        AND buTamboresOF.id_tipotambor          = 3 /*considera solo lotes de jugo*/
        AND (IF NOT plRepro         THEN buTamboresOF.id_locacion_ubicacion <> 10 ELSE TRUE)
        AND (IF piSucUbi <> 0       THEN buTamboresOF.id_sucursal = piSucUbi      ELSE TRUE)  
        AND (IF piArticulo <> 0     THEN buTamboresOF.id_articulo = piArticulo    ELSE TRUE) 
      BREAK BY buTamboresOF.nromov BY buTamboresOF.id_estado.

    i = i + 1.
    k = k + buTamboresOF.kilos_tambor.    
    IF LAST-OF(buTamboresOF.nromov) AND LAST-OF(buTamboresOF.id_estado) THEN DO:     
        RUN addProduccionIndustria(ROWID(buTamboresOF), i, k).
      i = 0.
      k = 0.
    END.
  END.


  /****/
  


  
  IF piTipoTambor = 12 THEN DO: /*cascara*/
    FOR EACH produccion_cascara WHERE (IF piSucUbi <> 0       THEN produccion_cascara.id_sucursal = piSucUbi   ELSE TRUE)
                                  AND (IF piArticulo <> 0     THEN produccion_cascara.id_articulo = piArticulo ELSE TRUE)
                                  AND (IF pdDesde <> DATE("") THEN produccion_cascara.fecha >= pdDesde         ELSE TRUE)
                                  AND (IF pdHasta <> DATE("") THEN produccion_cascara.fecha <= pdHasta         ELSE TRUE)
                                NO-LOCK.
      RUN addProduccionIndustria(ROWID(produccion_cascara), 0, 0).
    END.
  END.

  IF piTipoTambor = 11 THEN DO: /*cascara*/
    FOR EACH lotes_cascara WHERE (IF piSucUbi <> 0       THEN lotes_cascara.id_sucursal = piSucUbi   ELSE TRUE)
                             AND (IF piArticulo <> 0     THEN lotes_cascara.id_articulo = piArticulo ELSE TRUE)
                             AND (IF pdDesde <> DATE("") THEN lotes_cascara.fecha >= pdDesde         ELSE TRUE)
                             AND (IF pdHasta <> DATE("") THEN lotes_cascara.fecha <= pdHasta         ELSE TRUE)
                           NO-LOCK.
      dDesde = pdDesde.
      dHasta = pdHasta.
      RUN addProduccionIndustria(ROWID(lotes_cascara), 0, 0).
      
    END.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTamboresMatPrimaOil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTamboresMatPrimaOil Procedure 
PROCEDURE fillStockTamboresMatPrimaOil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo   AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTamboresMP FOR tambores_industria.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE r AS ROWID      NO-UNDO.
  
  /*trato las producciones de aceite diferente porque cada tambor de produccion de aceite es un lote (nromov distinto) si no lo hago tengo muchas filas en el reporte de un solo tambor*/
  IF piTipoTambor = 2 THEN DO:    
    FOR EACH buTamboresMP WHERE buTamboresMP.id_sucursal_ubicacion = piSucUbi                          
                            AND buTamboresMP.id_articulo           = piArticulo
                            AND buTamboresMP.id_locacion_ubicacion = 4
                            AND (IF piTipoTambor <> 0 THEN buTamboresMP.id_tipotambor = piTipoTambor ELSE TRUE)
                          NO-LOCK.
      i = i + 1.
      k = k + buTamboresMP.kilos_tambor.
      r = ROWID(buTamboresMP).
    END.
    RUN addStockTambores(r, i, k, FALSE).
    

  END.
  ELSE DO:    
    FOR EACH buTamboresMP WHERE buTamboresMP.id_sucursal_ubicacion = piSucUbi                          
                            AND buTamboresMP.id_articulo           = piArticulo
                            AND buTamboresMP.id_locacion_ubicacion = 4
                            AND (IF piTipoTambor <> 0 THEN buTamboresMP.id_tipotambor = piTipoTambor ELSE TRUE)
                          BREAK BY buTamboresMP.nromov.
      i = i + 1.
      k = k + buTamboresMP.kilos_tambor.
      IF LAST-OF(buTamboresMP.nromov) THEN DO:          
        r = ROWID(buTamboresMP).
        RUN addStockTambores(r, i, k, FALSE).
        i = 0.
        k = 0.
      END.
    END.
    
  END.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTamboresNoOf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTamboresNoOf Procedure 
PROCEDURE fillStockTamboresNoOf :
/*------------------------------------------------------------------------------
  Purpose:     calcula stock para lotes de jugo, aceite y foldeado (id_tipotambor = 3, 6, 7)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTambores FOR tambores_industria.
  
  FOR EACH buTambores WHERE buTambores.id_contrato_of        = ""
                        AND buTambores.id_tipocontrato_of    = 0
                        AND buTambores.anio_of               = 0
                        AND buTambores.id_locacion_ubicacion = 4 
                        AND buTambores.id_tipotambor         = piTipoTambor
                        AND buTambores.id_sucursal_ubicacion = piSucUbi
                        AND buTambores.nromov_destino        = 0
                        AND buTambores.id_estado            <> 2  /*para que no incluya los tambores de lote marcados para materia prima*/
                        AND buTambores.id_estado            <> 3  /*no incluye tambores que estan siendo reprocesados en lotes*/
                        AND buTambores.id_estado            <> 7  /*para que no considere los tambores en proceso de envase*/
                        AND buTambores.id_estado            <> 9  /*para que no considere los tambores procesados en carga*/
                        AND buTambores.id_estado            <> 10 /*para que no incluya los procesados en cargas pero en proceso abierto al momento del listado.*/
                      BREAK BY buTambores.nromov.
    i = i + 1.
    k = k + buTambores.kilos_tambor.
    IF LAST-OF(buTambores.nromov) THEN DO:  
      IF getCabeceraTieneFechaCierre(ROWID(buTambores)) THEN
        RUN addStockTambores(ROWID(buTambores), i, k, TRUE).
      i = 0.
      k = 0.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTamboresOf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTamboresOf Procedure 
PROCEDURE fillStockTamboresOf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTamboresOF FOR tambores_industria.
  
  FOR EACH buTamboresOF WHERE buTamboresOF.id_contrato_of        <> ""
                          AND buTamboresOF.id_tipocontrato_of    <> 0
                          AND buTamboresOF.anio_of               <> 0
                          AND buTamboresOF.id_locacion_ubicacion  = 4 
                          AND buTamboresOF.id_tipotambor          = piTipoTambor
                          AND buTamboresOF.id_sucursal_ubicacion  = piSucUbi
                          AND buTamboresOF.nromov_destino         = 0
                          AND buTamboresOF.id_estado             <> 2  /*para que no incluya los tambores de lote marcados para materia prima*/
                          AND buTamboresOF.id_estado             <> 3  /*no incluye tambores que estan siendo reprocesados en lotes*/
                          AND buTamboresOF.id_estado             <> 7  /*para que no considere los tambores en proceso de envase*/
                          AND buTamboresOF.id_estado             <> 9  /*para que no considere los tambores procesados en carga*/
                          AND buTamboresOF.id_estado             <> 10 /*para que no incluya los procesados en cargas pero en proceso abierto al momento del listado.*/
                        BREAK BY buTamboresOF.nromov.
    i = i + 1.
    k = k + buTamboresOF.kilos_tambor.    
    IF LAST-OF(buTamboresOF.nromov) THEN DO:    
      IF getCabeceraTieneFechaCierre(ROWID(buTamboresOF)) THEN
        RUN addStockTambores(ROWID(buTamboresOF), i, k, TRUE).
      i = 0.
      k = 0.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTamboresOilProdxLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTamboresOilProdxLote Procedure 
PROCEDURE fillStockTamboresOilProdxLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo   AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTamboresPA FOR tambores_industria.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE r AS ROWID      NO-UNDO.
  
  FOR EACH buTamboresPA WHERE buTamboresPA.id_sucursal_ubicacion = piSucUbi
                          AND buTamboresPA.id_articulo           = piArticulo
                          AND buTamboresPA.id_locacion_ubicacion = 4
                          /*AND buTamboresPA.nromov_destino        = 0*/
                        NO-LOCK
                        BREAK BY buTamboresPA.id_lote
                               BY YEAR(buTamboresPA.fecha)
                               BY buTamboresPA.id_articulo
                               BY buTamboresPA.id_calidad
                               BY buTamboresPA.id_envase
                               BY buTamboresPA.kilos_tambor.
    
    i = i + 1.
    k = k + buTamboresPA.kilos_tambor.
    IF LAST-OF(buTamboresPA.kilos_tambor) THEN DO:
      RUN addStockTambores(ROWID(buTamboresPA), i, k, FALSE).
      i = 0.
      k = 0.
    END.
  END.

  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTamboresReproc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTamboresReproc Procedure 
PROCEDURE fillStockTamboresReproc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTambores FOR tambores_industria.
  
  FOR EACH buTambores WHERE buTambores.id_sucursal_ubicacion = piSucUbi
                        AND buTambores.id_tipotambor         = piTipoTambor
                        AND buTambores.id_estado             = piEstado
                        AND (IF piEstado <> 9 THEN buTambores.id_locacion_ubicacion = 4 ELSE TRUE) /*esto esta feo pero es para que recupere los tambores procesados en cargas*/
                      BREAK BY buTambores.nromov.
    i = i + 1.
    k = k + buTambores.kilos_tambor.
    IF LAST-OF(buTambores.nromov) THEN DO:    
      RUN addStockTambores(ROWID(buTambores), i, k, FALSE).
      i = 0.
      k = 0.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTbsReproBatcheo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTbsReproBatcheo Procedure 
PROCEDURE fillStockTbsReproBatcheo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEstado     AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTambores FOR tambores_industria.
  
  FOR EACH buTambores WHERE buTambores.id_sucursal_ubicacion = piSucUbi
                        AND buTambores.id_tipotambor         = piTipoTambor
                        AND buTambores.id_estado             = piEstado
                        AND buTambores.id_locacion_ubicacion = 10
                      BREAK BY buTambores.nromov.
    i = i + 1.
    k = k + buTambores.kilos_tambor.
    IF LAST-OF(buTambores.nromov) THEN DO:    
      RUN addStockTambores(ROWID(buTambores), i, k).
      i = 0.
      k = 0.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillStockTbsReproCargas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillStockTbsReproCargas Procedure 
PROCEDURE fillStockTbsReproCargas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTambores FOR tambores_industria.


  FOR EACH proceso WHERE proceso.id_sucursal = piSucUbi
                     AND proceso.fecha_fin   = ?
                   NO-LOCK.
    FOR EACH cargas WHERE cargas.nromov_proceso = proceso.nromov
                    NO-LOCK.
      FOR EACH buTambores WHERE buTambores.id_empresa_destino     = cargas.id_empresa
                            AND buTambores.id_sucursal_destino    = cargas.id_sucursal
                            AND buTambores.id_tipotambor_destino  = cargas.id_tipotambor
                            AND buTambores.nromov_destino         = cargas.nromov
                            AND buTambores.id_tipotambor          = piTipoTambor
                          BREAK BY buTambores.nromov.
        i = i + 1.
        k = k + buTambores.kilos_tambor.
        IF LAST-OF(buTambores.nromov) THEN DO:    
          RUN addStockTambores(ROWID(buTambores), i, k, FALSE).
          i = 0.
          k = 0.
        END.
      END.      
    END.    
  END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTamboresVolcadosEn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTamboresVolcadosEn Procedure 
PROCEDURE fillTamboresVolcadosEn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdAju AS DATE       NO-UNDO.

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE l     AS DECIMAL    NO-UNDO.

 
  /*recupero los tambores reprocesados en la carga o lote*/
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino     = piEmp
                                AND tambores_industria.id_sucursal_destino    = piSuc
                                AND tambores_industria.id_tipotambor_destino  = piTip
                                AND tambores_industria.nromov_destino         = piNro
                                AND (IF pdAju <> ? THEN tambores_industria.fecha_reproceso >= pdAju ELSE TRUE)
                              BREAK BY tambores_industria.nromov.
    i = i + 1.

    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov) THEN DO:
      RUN addPerdidaTamboresRepro(ROWID(tambores_industria), i, l, k, FALSE).        
      i = 0.
      k = 0.
    END.      
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillttOrdenReporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillttOrdenReporte Procedure 
PROCEDURE fillttOrdenReporte PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttOrdenReporte.
    DELETE ttOrdenReporte.    
  END.


  
  
  RUN addttOrden(10, 3, 52, 0).
  RUN addttOrden(20, 3, 53, 0).
  RUN addttOrden(10, 3, 52, 4).
  RUN addttOrden(11, 3, 52, 11).
  RUN addttOrden(20, 3, 53, 4).
  RUN addttOrden(10, 3, 52, 6).
  RUN addttOrden(20, 3, 53, 6).
  RUN addttOrden(21, 3, 53, 11).

  RUN addttOrden(30, 1, 532, 0).
  RUN addttOrden(40, 1, 521, 0).
  RUN addttOrden(47, 11, 52, 0).
  RUN addttOrden(48, 11, 53, 0).
  RUN addttOrden(50, 4, 524, 0).
  RUN addttOrden(60, 4, 534, 0).
  RUN addttOrden(70, 5, 523, 0).
  RUN addttOrden(80, 5, 535, 0).
  RUN addttOrden(90, 1, 43, 0).
  RUN addttOrden(100, 1, 42, 0).
  RUN addttOrden(110, 3, 952, 0).
  RUN addttOrden(120, 3, 953, 0).
  RUN addttOrden(130, 3, 52, 2).
  RUN addttOrden(140, 3, 53, 2).
  RUN addttOrden(150, 3, 52, 3).
  RUN addttOrden(160, 3, 53, 3).  
  RUN addttOrden(170, 9, 42, 0).  
  RUN addttOrden(171, 9, 52, 0).  
  RUN addttOrden(172, 9, 53, 0).  
  RUN addttOrden(173, 9, 521, 0).  
  RUN addttOrden(174, 9, 532, 0).  
  RUN addttOrden(175, 9, 952, 0).  
  RUN addttOrden(176, 9, 953, 0).  
  RUN addttOrden(177, 9, 57, 0). 
  RUN addttOrden(178, 9, 54, 0).  
  RUN addttOrden(179, 9, 64, 0).  
  RUN addttOrden(180, 3, 71, 4).
  RUN addttOrden(181, 3, 71, 11).
  RUN addttOrden(182, 3, 30, 4).
  RUN addttOrden(183, 3, 30, 11).
  RUN addttOrden(1001, 3, 52, 7).
  RUN addttOrden(1002, 3, 53, 7).
  RUN addttOrden(1003, 3, 71, 2).  
  RUN addttOrden(1002, 3, 71, 7).
  RUN addttOrden(1002, 3, 30, 2).
  RUN addttOrden(1003, 3, 30, 7).
  RUN addttOrden(1003, 1, 532, 10).
  RUN addttOrden(1004, 1, 533, 10).
  RUN addttOrden(1005, 1, 53, 10).
  RUN addttOrden(1006, 1, 52, 10).
  RUN addttOrden(1007, 1, 42, 10).
  RUN addttOrden(1008, 1, 49, 10).
  RUN addttOrden(1009, 1, 521, 10).
  RUN addttOrden(1010, 4, 534, 10).
  RUN addttOrden(1011, 5, 534, 10).
  RUN addttOrden(1011, 1, 43, 10).

  RUN addttOrden(1012, 1, 532, 3).
  RUN addttOrden(1012, 1, 521, 3).
  RUN addttOrden(1012, 1, 532, 10).
  RUN addttOrden(1012, 1, 521, 10).

  RUN addttOrden(1013, 3, 52, 3).
  RUN addttOrden(1013, 3, 53, 3).
  RUN addttOrden(1013, 9, 52, 3).
  RUN addttOrden(1013, 9, 53, 3).
  RUN addttOrden(1013, 3, 52, 10).
  RUN addttOrden(1013, 3, 53, 10).
  
  RUN addttOrden(1014, 9, 952, 3).
  RUN addttOrden(1014, 9, 953, 3).
  RUN addttOrden(1014, 9, 42, 10).
  RUN addttOrden(1014, 9, 43, 10).

  RUN addttOrden(1015, 4, 534, 3).
  RUN addttOrden(1015, 4, 524, 3).
  RUN addttOrden(1015, 5, 535, 3).
  RUN addttOrden(1015, 4, 524, 3).



  RUN addttOrden(1012, 1, 532, 3).



/*
  RUN generateExcel.p (INPUT TABLE ttOrdenReporte,
                         INPUT " Orden Reporte ",
                         INPUT " ",
                         INPUT 7,
                         INPUT 8,
                         INPUT "Century Gothic",
                         INPUT 7).
*/


/*
  RUN addttOrden(100, 9, 41, 0).
  RUN addttOrden(60, 1, 42, 0).
  RUN addttOrden(170, 9, 42, 0).
  RUN addttOrden(55, 1, 43, 0).
  RUN addttOrden(65, 3, 46, 0).

  RUN addttOrden(73, 6, 50, 0).
  RUN addttOrden(74, 6, 51, 0).
  RUN addttOrden(10, 3, 52, 0).
  RUN addttOrden(1, 3, 53, 0).
  RUN addttOrden(110, 6, 57, 0).
  RUN addttOrden(105, 6, 58, 0).

  RUN addttOrden(130, 6, 61, 0).
  RUN addttOrden(170, 9, 64, 0).
  RUN addttOrden(170, 9, 66, 0).

  RUN addttOrden(20, 3, 71, 0).
  RUN addttOrden(25, 3, 70, 0).
  RUN addttOrden(170, 6, 74, 0).
  RUN addttOrden(180, 6, 76, 0).

  RUN addttOrden(111, 6, 90, 0).

  RUN addttOrden(75, 6, 512, 0).
  /*RUN addttOrden(80, 8, 513, 0).*/
  /*RUN addttOrden(81, 8, 514, 0).*/
  RUN addttOrden(90, 6, 517, 0).
  /*RUN addttOrden(91, 8, 518, 0).*/
  /*RUN addttOrden(92, 8, 519, 0).*/
  /*RUN addttOrden(93, 8, 520, 0).*/
  RUN addttOrden(40, 1, 521, 0).
  RUN addttOrden(45, 5, 523, 0).
  /*RUN addttOrden(200, 4, 524, 0).*/

  RUN addttOrden(30, 1, 532, 0).
  /*RUN addttOrden(210, 4, 534, 0).*/
  /*RUN addttOrden(36, 5, 535, 0).*/
  
  RUN addttOrden(196, 7, 582, 0).

  RUN addttOrden(190, 0, 761, 0).
  RUN addttOrden(193, 6, 762, 0).
  RUN addttOrden(195, 8, 763, 0).

  RUN addttOrden(40, 1, 882, 0).
  RUN addttOrden(50, 0, 942, 0).
  RUN addttOrden(30, 3, 952, 0).
  RUN addttOrden(30, 3, 953, 0).
  RUN addttOrden(31, 3, 52, 2).
  RUN addttOrden(32, 3, 53, 2).
  RUN addttOrden(33, 3, 52, 3).
  RUN addttOrden(34, 3, 53, 3).
  
  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-impresionHojaCarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresionHojaCarga Procedure 
PROCEDURE impresionHojaCarga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cFiltro         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReport         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibRem         AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibRem   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p'). 
  DELETE OBJECT hLibCom.
                                               
  cFiltro = "remitos.id_sucursal = " + STRING(piSuc)+
            " AND remitos.id_tipo_movsto = " + STRING(piTip) + 
            " AND remitos.nro = " + STRING(piNro).
                                    
  cOps = DYNAMIC-FUNCTION('getOperariosCarga' IN hLibRem, piSuc, piTip, piNro).
                                                          
  cOps = REPLACE(cOps, CHR(10), "|").
  cOps = "operarios=" + cOps.

  FIND FIRST items_factura
       WHERE items_factura.id_sucursal    = piSuc
         AND items_factura.id_tipo_movsto = piTip
         AND items_factura.nro            = piNro
       NO-LOCK NO-ERROR.
 
  IF items_factura.id_tipotambor = 11 THEN
    cReport = "despacho_productos_cascara".
  ELSE
    cReport = "despacho_productos".

  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                        cReport,                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Hoja de Despacho",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        cOps /* RB-OTHER-PARAMETERS */,
                        "").   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-impresionInspeccionTransporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresionInspeccionTransporte Procedure 
PROCEDURE impresionInspeccionTransporte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cFiltro         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReport         AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibRem         AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibRem   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p'). 
  DELETE OBJECT hLibCom.
                                               
  cFiltro = "[tbl].id_sucursal_remito = " + STRING(piSuc)+
            " AND [tbl].id_tipo_movsto = " + STRING(piTip) + 
            " AND [tbl].nro_remito = " + STRING(piNro).
                                    
  cOps = DYNAMIC-FUNCTION('getOperariosCarga' IN hLibRem, piSuc, piTip, piNro).
                                                          
  cOps = REPLACE(cOps, CHR(10), "|").
  cOps = "operarios=" + cOps.

  FIND FIRST inspeccion_camion
       WHERE inspeccion_camion.id_sucursal_remito = piSuc
         AND inspeccion_camion.id_tipo_movsto     = piTip
         AND inspeccion_camion.nro_remito         = piNro
       NO-LOCK NO-ERROR.
 
  IF AVAILABLE inspeccion_camion THEN
    cReport = "inspeccion_camiones".
  
  FIND FIRST inspeccion_contenedor
       WHERE inspeccion_contenedor.id_sucursal_remito    = piSuc
         AND inspeccion_contenedor.id_tipo_movsto        = piTip
         AND inspeccion_contenedor.nro_remito            = piNro
       NO-LOCK NO-ERROR.
 
  IF AVAILABLE inspeccion_contenedor THEN
    cReport = "inspeccion_contenedores".

  cFiltro = REPLACE(cFiltro, "[tbl]", cReport).

  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                        cReport,                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Inspeccion Transporte",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        cOps,                  /* RB-OTHER-PARAMETERS */
                        "").   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-impresionRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresionRemito Procedure 
PROCEDURE impresionRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iNroCopia AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrt      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFiltro   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPdf      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib      AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
 
  FIND FIRST remitos WHERE remitos.id_sucursal    = piSuc
                       AND remitos.id_tipo_movsto = piTip
                       AND remitos.nro            = piNro
                     NO-ERROR.
  IF AVAILABLE remitos THEN DO:
      iNroCopia = IF remitos.id_sucursal <> 96 AND remitos.id_sucursal <> 95 THEN 6 ELSE 5.

      /*pedido por la prochi 22/12/2006 para que laura pueda reimprimir remitos sin la leyenda de reimpreso */
      IF USERID('userdb') = 'y_lmunoz' THEN DO:
        iPrt = remitos.impresion.
        remitos.impresion = 0.
        iNroCopia = 5.
      END.

      /* recalculo kilos remito */
      RUN recalcKilosRemito IN  hLib (piSuc, piTip, piNro).
       
      RUN fillReportTable IN hLib (piSuc, piTip, piNro).

      RUN p_reportes_remito.p ("rptRemitoIndustria",
                               "Remito",
                               "",
                               "",
                               iNroCopia).
     
      IF USERID('userdb') = 'y_lmunoz' THEN DO:
        remitos.impresion = iPrt.
      END.
      ASSIGN remitos.impresion = remitos.impresion + 1.
      
      IF remitos.mercado = 1 THEN DO:   
        /* hoja de carga */
        RUN impresionHojaCarga (piSuc, piTip, piNro).
  
        /* inspeccion de transporte */
        RUN impresionInspeccionTransporte (piSuc, piTip, piNro).
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parteDetalleMolienda) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parteDetalleMolienda Procedure 
PROCEDURE parteDetalleMolienda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLin AS INTEGER    NO-UNDO.

  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSto AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBza AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fAcc AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH registro_molienda.
    DELETE registro_molienda.
  END.

  fBza = DYNAMIC-FUNCTION('getIngresoBalanzaDia' IN hLib, pdFec, piLin, 0).
  fSil = DYNAMIC-FUNCTION('getStockSilos' IN hLib, pdFec - 1, piLin).
  fSto = fBza + fSil.

  /* recupero dia de hoy a hasta 6 am */
  FOR EACH molienda_silo
      WHERE molienda_silo.id_linea  = piLin
        AND molienda_silo.fecha     = pdFec 
      NO-LOCK,
      EACH items_molienda_silo 
        OF molienda_silo
     BY items_molienda_silo.fecha
     BY items_molienda_silo.id_turno
     BY INTEGER(ENTRY(1, items_molienda_silo.hora_comienzo, ":"))
     BY INTEGER(ENTRY(1, items_molienda_silo.hora_termino, ":")).

    FIND FIRST lineas_produccion WHERE lineas_produccion.id_linea = molienda_silo.id_linea NO-LOCK NO-ERROR.
    FIND FIRST silo OF items_molienda_silo NO-LOCK NO-ERROR.
 
    fKil = fKil + items_molienda_silo.kilos + items_molienda_silo.kilos_descarte.
    fAcc = fSto - (items_molienda_silo.kilos + items_molienda_silo.kilos_descarte).
    CREATE registro_molienda.
    ASSIGN registro_molienda.id_linea         = items_molienda_silo.id_linea
           registro_molienda.id_silo          = items_molienda_silo.id_silo
           registro_molienda.id_turno         = items_molienda_silo.id_turno
           registro_molienda.fecha            = items_molienda_silo.fecha
           registro_molienda.hora_inicio      = items_molienda_silo.hora_comienzo
           registro_molienda.hora_fin         = items_molienda_silo.hora_termino
           registro_molienda.kilos            = items_molienda_silo.kilos
           registro_molienda.descarte         = items_molienda_silo.kilos_descarte
           registro_molienda.acumulado        = fKil
           registro_molienda.linea            = lineas_produccion.abreviatura 
           registro_molienda.stock            = fSto 
           registro_molienda.observaciones    = ""
           registro_molienda.acumulado_stock  = fAcc
           registro_molienda.fecha_desde      = pdFec - 1
           registro_molienda.hora_desde       = "06:00"
           registro_molienda.fecha_hasta      = pdFec
           registro_molienda.hora_hasta       = "06:00"
           .
    fSto = fSto - (items_molienda_silo.kilos + items_molienda_silo.kilos_descarte).
    
   

  END.

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parteIngresoBalanza) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parteIngresoBalanza Procedure 
PROCEDURE parteIngresoBalanza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDes AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHas AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLin AS INTEGER    NO-UNDO.

  DEFINE BUFFER buProv FOR proveedores.
  DEFINE BUFFER buProvOri FOR proveedores.

  FOR EACH ttIngresoBalanza.
    DELETE ttIngresoBalanza.
  END.


  FOR EACH balanza_tickets
      WHERE balanza_tickets.id_balanza        = 3
        /*AND balanza_tickets.id_materia_prima  = 1*/
        AND balanza_tickets.fecha_operativa  >= pdDes
        AND balanza_tickets.fecha_operativa  <= pdHas
      NO-LOCK, 
      FIRST balanza_pesada 
        OF balanza_tickets
      WHERE(IF piLin <> 0 THEN balanza_pesada.id_pesada_ctf = piLin ELSE TRUE)
      NO-LOCK.

    FIND FIRST buProv WHERE balanza_tickets.id_proveedor = buProv.id_proveedor NO-LOCK NO-ERROR.
    FIND FIRST buProvOri WHERE balanza_tickets.id_proveedor_origen = buProvOri.id_proveedor NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
    FIND FIRST origenes
         WHERE origenes.id_proveedor = balanza_tickets.id_proveedor
           AND origenes.id_origen    = balanza_tickets.id_origen
         NO-LOCK.

    CREATE ttIngresoBalanza.
    ASSIGN  ttIngresoBalanza.fecha        = balanza_tickets.fecha_operativa
            ttIngresoBalanza.nro_pesada   = balanza_pesada.id_pesada
            ttIngresoBalanza.producto     = productos_terminados.descripcion
            ttIngresoBalanza.hora_entrada = balanza_tickets.hora_entrada
            ttIngresoBalanza.hora_salida  = balanza_tickets.hora_salida
            ttIngresoBalanza.peso         = balanza_tickets.peso_neto_ticket
            ttIngresoBalanza.linea        = IF balanza_pesada.id_pesada_ctf = 1 THEN "LINEA F" ELSE "LINEA L"
            ttIngresoBalanza.origen       = origenes.descripcion
            ttIngresoBalanza.proveedor    = IF AVAILABLE buProv THEN buProv.razon_social ELSE "NONE"
            .
         

  END.


  RUN generateExcel.p (INPUT TABLE ttIngresoBalanza,
                        INPUT " Ingresos de Fruta a Industria por Balanza",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-printPackingListsFromExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printPackingListsFromExcel Procedure 
PROCEDURE printPackingListsFromExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcExcelFile AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piLastRow   AS INTEGER    NO-UNDO.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.


  DEFINE VARIABLE cRange    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPackings AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFactura  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iAduana   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFila     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cPermiso  AS CHARACTER  NO-UNDO.


  IF SEARCH(pcExcelFile) = ? THEN DO:
    MESSAGE "No se encontro el archivo " + pcExcelFile
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN(pcExcelFile). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).


  DO i = 2 TO piLastRow:
    iAnio    = chWorkSheet:Range("A" + STRING(i)):VALUE.
    iAduana  = chWorkSheet:Range("B" + STRING(i)):VALUE.
    cPermiso = chWorkSheet:Range("C" + STRING(i)):VALUE.



    FOR EACH items_packing_list
        WHERE items_packing_list.id_aduana             = iAduana
          AND items_packing_list.anio_permiso          = iAnio
          AND items_packing_list.nro_permiso_embarque  = cPermiso 
        BREAK BY items_packing_list.id_packing_list.
      
      IF LAST-OF(items_packing_list.id_packing_list) THEN DO:
        FIND FIRST packing_list 
             OF items_packing_list
             WHERE packing_list.id_tipo_pack_list = 2
             NO-LOCK NO-ERROR.
        IF AVAILABLE packing_list THEN DO:
          cPackings = IF chWorkSheet:Range("D" + STRING(i)):VALUE = ? THEN "" ELSE chWorkSheet:Range("D" + STRING(i)):VALUE.
          cPackings = cPackings + ", " + STRING(packing_list.nro_pack_list).
          chWorkSheet:Range("D" + STRING(i)):VALUE = cPackings.
          cPackings = "".

        
          /* find a factura */
          FIND FIRST r_items_venta_pack_list OF packing_list NO-LOCK NO-ERROR.
          IF AVAILABLE r_items_venta_pack_list THEN
            cFactura = "FACTURADO".
          ELSE 
            cFactura = "".
          chWorkSheet:Range("E" + STRING(i)):VALUE = cFactura.

          RUN ..\ventas\p-imprime-packing-list-fc-02-facu.p (ROWID(packing_list), 1, 1, 1, 1, 1, 1). 
  
        END.
      END.

    END.

 
  END.
 
  chWorkBook:SAVE().
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet.
 

  MESSAGE "Terminado!!" SKIP "QUE SISTEMA PAPAAA!!!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-printPackingListSinFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printPackingListSinFactura Procedure 
PROCEDURE printPackingListSinFactura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcExcelFile AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piLastRow   AS INTEGER    NO-UNDO.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.


  DEFINE VARIABLE cPacking AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSignal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER    NO-UNDO.


  IF SEARCH(pcExcelFile) = ? THEN DO:
    MESSAGE "No se encontro el archivo " + pcExcelFile
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN(pcExcelFile). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).


  DO i = 5 TO piLastRow:
    cPacking = chWorkSheet:Range("B" + STRING(i)):VALUE.
    cSignal  = "".
    FIND FIRST packing_list
         WHERE packing_list.nro_pack_list = cPacking
           AND packing_list.id_tipo_pack_list = 2
         NO-LOCK NO-ERROR.
    IF AVAILABLE packing_list THEN DO:
      cSignal = "Print OK".
      RUN ..\ventas\p-imprime-packing-list-fc-02-facu.p (ROWID(packing_list), 1, 1, 1, 1, 1, 1). 
      chWorkSheet:Range("E" + STRING(i)):VALUE = cSignal.
    END.
  END.
 
  chWorkBook:SAVE().
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet.
 

  MESSAGE "Terminado!!" SKIP "QUE SISTEMA PAPAAA!!!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEstadoTamboresAbiertoFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstadoTamboresAbiertoFecha Procedure 
PROCEDURE setEstadoTamboresAbiertoFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt   AS INTEGER    NO-UNDO.

  
  FOR EACH lotes_jugo WHERE lotes_jugo.id_sucursal         = piSuc
                        AND lotes_jugo.fecha_comienzo     <= pdHasta
                        AND lotes_jugo.fecha_finalizacion >= pdHasta
                        AND (IF piLote <> 0 THEN lotes_jugo.id_lote     = piLote ELSE TRUE)
                        AND (IF piAnio <> 0 THEN lotes_jugo.anio        = piAnio ELSE TRUE)
                        AND (IF piArt  <> 0 THEN lotes_jugo.id_articulo = piArt  ELSE TRUE)
                      NO-LOCK.

    FOR EACH stock_fecha WHERE lotes_jugo.nromov = stock_fecha.nromov.
      ASSIGN stock_fecha.id_estado = 7.      
    END.

    FOR EACH stock_fecha WHERE stock_fecha.nromov_destino = lotes_jugo.nromov.
      ASSIGN stock_fecha.id_estado = 3.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getBancoOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBancoOE Procedure 
FUNCTION getBancoOE RETURNS CHARACTER
  (piCli AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "CITIBANK".

  CASE piCli:
      WHEN 100085 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 200005 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100124 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100998 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100239 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100999 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100201 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100109 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100068 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 200001 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 200023 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 100247 THEN cRet = "BNP PARIBAS (Nro. 266)".
      WHEN 3484 THEN cRet = "BNP PARIBAS (Nro. 266)".

  END CASE.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCabeceraTieneFechaCierre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCabeceraTieneFechaCierre Procedure 
FUNCTION getCabeceraTieneFechaCierre RETURNS LOGICAL
  (prTambor AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = TRUE.


  FOR FIRST tambores_industria
     WHERE ROWID(tambores_industria) = prTambor
     NO-LOCK.
  
   IF tambores_industria.id_tipotambor = 3 THEN DO: 

     FOR FIRST lotes_jugo 
         WHERE lotes_jugo.nromov = tambores_industria.nromov
         NO-LOCK.
       IF lotes_jugo.fecha_finalizacion = ? THEN
         lRet = FALSE.
       ELSE 
         lRet = TRUE.
     END.
  
   END.
  
  END.

  RETURN lRet.
    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadesLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadesLote Procedure 
FUNCTION getCantidadesLote RETURNS CHARACTER
  (piEmpresa    AS INTEGER, 
   piSucursal   AS INTEGER, 
   piTipoTambor AS INTEGER, 
   piNromov     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  cRet = "0,0".
  FOR EACH buTam WHERE buTam.id_empresa    = piEmpresa
                   AND buTam.id_sucursal   = piSucursal
                   AND buTam.id_tipotambor = piTipoTambor
                   AND buTam.nromov        = piNroMov
                 NO-LOCK.
    i = i + 1.
    k = k + buTam.kilos_tambor.
  END.

  cRet = STRING(i) + CHR(1) + STRING(k).
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantidadKilosSucUbi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadKilosSucUbi Procedure 
FUNCTION getCantidadKilosSucUbi RETURNS CHARACTER
  (piEmpresa    AS INTEGER, 
   piSucursal   AS INTEGER, 
   piTipoTambor AS INTEGER, 
   piNromov     AS INTEGER, 
   piSucUbi     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER NO-UNDO.

  cRet = "0" + CHR(1) + "0".
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa            = piEmpresa
                                AND tambores_industria.id_sucursal           = piSucursal
                                AND tambores_industria.id_tipotambor         = piTipoTambor
                                AND tambores_industria.nromov                = piNroMov
                                AND tambores_industria.id_sucursal_ubicacion = piSucUbi
                                AND tambores_industria.id_locacion_ubicacion = 4
                              NO-LOCK.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
  END.

  cRet = STRING(i) + CHR(1) + STRING(k).
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosContratoLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosContratoLote Procedure 
FUNCTION getDatosContratoLote RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    FIND FIRST contratos WHERE contratos.id_contrato      = tambores_industria.id_contrato_of
                           AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                           AND contratos.anio             = tambores_industria.anio_of
                         NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN DO:    
      cRet = contratos.id_contrato + CHR(1) + 
             STRING(contrato.id_tipo_contrato) + CHR(1) + 
             STRING(contratos.anio) + CHR(1) + 
             STRING(contratos.orden_fabricacion) + CHR(1) + 
             STRING(contratos.id_cliente).
    END.
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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST productos_terminados WHERE productos_terminados.id_articulo = piArticulo
                                 NO-LOCK.
    cRet = productos_terminados.descripcion.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescArticuloIngles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescArticuloIngles Procedure 
FUNCTION getDescArticuloIngles RETURNS CHARACTER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = piArticulo
                                  NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN
    RETURN productos_terminados.descripcion_ingles.
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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST calidades WHERE calidades.id_calidad = piCalidad
                      NO-LOCK.
    cRet = calidades.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescCliente Procedure 
FUNCTION getDescCliente RETURNS CHARACTER
  (piCliente AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND FIRST clientes WHERE clientes.id_cliente = piCliente
                      NO-LOCK NO-ERROR.
  IF AVAILABLE clientes THEN
    RETURN clientes.razon_social.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescCondicionLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescCondicionLote Procedure 
FUNCTION getDescCondicionLote RETURNS CHARACTER
  (iEmp AS INTEGER,
   iSuc AS INTEGER,
   iTip AS INTEGER,
   iNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la descripcion de con que fue hecho el lote
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa = iEmp
                          AND lotes_jugo.id_sucursal = iSuc
                          AND lotes_jugo.id_tipotambor = iTip
                          AND lotes_jugo.nromov = iNro
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    IF lotes_jugo.id_condicion_origen = 1 THEN
      RETURN "SOLO JUGO DE LINEA".

    IF lotes_jugo.id_condicion_origen = 2 THEN
      RETURN "JUGO DE LINEA + REPROCESO".
  END.



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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST envases_prod WHERE envases_prod.id_envase = piEnvase
                         NO-LOCK.
    cRet = envases_prod.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescEstado Procedure 
FUNCTION getDescEstado RETURNS CHARACTER
  (piEstado AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST estados_tambor WHERE estados_tambor.id_estado = piEstado
                            NO-LOCK NO-ERROR.
  IF AVAILABLE estados_tambor THEN
    RETURN estados_tambor.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescEstados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescEstados Procedure 
FUNCTION getDescEstados RETURNS CHARACTER
  (piEstado     AS INTEGER, 
   piTipoTambor AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST estados_lote WHERE estados_lote.id_estado = piEstado
                          NO-LOCK NO-ERROR.
  IF AVAILABLE estados_lote THEN
    RETURN estados_lote.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescLugarDescarga) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescLugarDescarga Procedure 
FUNCTION getDescLugarDescarga RETURNS CHARACTER
  (piLugDes AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = piLugDes 
                             NO-LOCK.
    cRet = lugar_descarga.descripcion.
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescOrdenFabricacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescOrdenFabricacion Procedure 
FUNCTION getDescOrdenFabricacion RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piTipo     AS INTEGER,
   piAnio     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST contratos WHERE contratos.id_contrato      = pcContrato
                         AND contratos.id_tipo_contrato = piTipo
                         AND contratos.anio             = piAnio
                       NO-LOCK NO-ERROR.
  IF AVAILABLE contratos THEN 
    RETURN STRING(contratos.orden_fabricacion).
  ELSE
    RETURN "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescProveedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescProveedor Procedure 
FUNCTION getDescProveedor RETURNS CHARACTER
  (piProveedor AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST proveedores WHERE proveedores.id_proveedor = piProveedor
                         NO-LOCK NO-ERROR.
  IF AVAILABLE proveedores THEN
    RETURN proveedores.razon_social.
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

&IF DEFINED(EXCLUDE-getDescTanque) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescTanque Procedure 
FUNCTION getDescTanque RETURNS CHARACTER
  (piTanque AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST tanque WHERE tanque.id_tanque = piTanque NO-LOCK.
    cRet = tanque.descripcion.
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

&IF DEFINED(EXCLUDE-getDescUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescUnidad Procedure 
FUNCTION getDescUnidad RETURNS CHARACTER
  (piUnidad AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST unidades_quimicas WHERE unidades_quimicas.id_unidad = piUnidad NO-LOCK.
    cRet = unidades_quimicas.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExisteCabecera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExisteCabecera Procedure 
FUNCTION getExisteCabecera RETURNS LOGICAL
  (prLote AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  compruebo que exista la cabecera, encontre lotes que no tienen cabecera en lotes_jugo
            ej 341/04 1080 tambores
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buTICab FOR tambores_industria.

  FIND FIRST buTICab WHERE ROWID(buTICab) = prLote
                                NO-LOCK NO-ERROR.
  IF AVAILABLE buTICab THEN DO:
    CASE buTICab.id_tipotambor:
      WHEN 1 THEN 
        RETURN TRUE.
      WHEN 2 THEN
        RETURN TRUE.
      WHEN 3 THEN DO:
        FIND FIRST lotes_jugo OF buTICab NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_jugo THEN 
          RETURN TRUE.
        ELSE
          RETURN FALSE.     
      END.        
      WHEN 4 THEN 
        RETURN TRUE.
      WHEN 5 THEN
        RETURN TRUE.
      WHEN 6 THEN DO:
      FIND FIRST lotes_aceite OF buTICab NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_aceite THEN 
          RETURN TRUE.
        ELSE
          RETURN FALSE.     
      END.        
      WHEN 7 THEN
        RETURN TRUE.
      WHEN 8 THEN DO:
        FIND FIRST sobrante_lotes_aceite WHERE sobrante_lotes_aceite.nromov_sobrante = buTICab.nromov NO-LOCK NO-ERROR.
        IF AVAILABLE sobrante_lotes_aceite THEN
          RETURN TRUE.
        ELSE 
          RETURN FALSE.
        END.      
      WHEN 9 THEN
        RETURN TRUE.
      WHEN 10 THEN
        RETURN TRUE.
      WHEN 11 THEN
        RETURN TRUE.
      WHEN 12 THEN
        RETURN TRUE.
    END CASE.
  END.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExisteDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExisteDestino Procedure 
FUNCTION getExisteDestino RETURNS LOGICAL
  (piNroMov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  DEFINE BUFFER buDestino FOR tambores_industria.

  FOR FIRST buDestino WHERE buDestino.nromov <> 0
                        AND buDestino.nromov = piNroMov
                      NO-LOCK .
    lRet = TRUE.
  END.

  RETURN lRet.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalonesTambor Procedure 
FUNCTION getGalonesTambor RETURNS DECIMAL
  (pdBrixCorreg AS DECIMAL,
   pdPeso       AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dGall AS DECIMAL    NO-UNDO.

  
  FIND LAST brix WHERE brix.brix <= ROUND(pdBrixCorreg,1) 
                 NO-LOCK NO-ERROR.
  IF AVAILABLE brix THEN
    dGall = ROUND((pdPeso / brix.pe) / 3.785, 2).
  ELSE
    dGall = 0.00.   

  RETURN dGall.

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
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE dKilos  AS DECIMAL    NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  dKilos = DYNAMIC-FUNCTION('getKilos400' IN hLibTam, piTipoTambor,
                                                      piArticulo,
                                                      piCalidad,
                                                      pdKilos).

  RETURN dKilos.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosFromAnalisis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosFromAnalisis Procedure 
FUNCTION getKilosFromAnalisis RETURNS DECIMAL
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
  DEFINE VARIABLE dLitros AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKgs    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.

  RUN libTamboresIndustria PERSISTENT SET hLib.


  IF piTip = 3 THEN DO: /*lote*/
    FIND FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa     = piEmp
                                   AND inspecciones_lote.id_sucursal    = piSuc
                                   AND inspecciones_lote.id_tipotambor  = piTip
                                   AND inspecciones_lote.nromov         = piNro
                                   AND inspecciones_lote.final          = TRUE.
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN DO:
      dBrix   = inspecciones_lote.bx_20_20.
      dAcidez = inspecciones_lote.acidez_w_w.
      dLitros = inspecciones_lote.litros.
    END.
  END.

  IF piTip = 1 THEN DO: /*produccion*/
    FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa     = piEmp
                                 AND produccion_jugo.id_sucursal    = piSuc
                                 AND produccion_jugo.id_tipotambor  = piTip
                                 AND produccion_jugo.nromov         = piNro
                               NO-LOCK NO-ERROR.
    IF AVAILABLE produccion_jugo THEN DO:
      dBrix   = produccion_jugo.bx_20_20.
      dAcidez = produccion_jugo.acidez_w_w.
      dLitros = produccion_jugo.litros.
    END.
  END.

  dKgs = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, dAcidez, dBrix, dLitros).

  IF VALID-HANDLE(hLib) THEN
    DELETE OBJECT hLib.

  RETURN dKgs.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteCerradoEnPeriodo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteCerradoEnPeriodo Procedure 
FUNCTION getLoteCerradoEnPeriodo RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   pdDesde AS DATE,
   pdHasta AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  solo para lotes de jugo el resto devuelve verdadero
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL INITIAL TRUE NO-UNDO.

  FOR FIRST lotes_jugo
      WHERE lotes_jugo.id_empresa    = piEmp
        AND lotes_jugo.id_sucursal   = piSuc
        AND lotes_jugo.id_tipotambor = piTip
        AND lotes_jugo.nromov        = piNro
      NO-LOCK.

    lRet = lotes_jugo.fecha_finalizacion >= pdDesde AND lotes_jugo.fecha_finalizacion <= pdHasta.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteCerradoEnPeriodod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteCerradoEnPeriodod Procedure 
FUNCTION getLoteCerradoEnPeriodod RETURNS LOGICAL
  (prLote  AS ROWID,
   pdDesde AS DATE,
   pdHasta AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.


  FOR FIRST lotes_jugo
      WHERE ROWID(lotes_jugo) = prLote
        AND lotes_jugo.fecha_finalizacion >= pdDesde
        AND lotes_jugo.fecha_finalizacion <= pdHasta
      NO-LOCK.
    lRet = TRUE.
  END.


  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMerma) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMerma Procedure 
FUNCTION getMerma RETURNS DECIMAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER, 
   pdKil AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTam FOR tambores_industria.


  FOR EACH buTam WHERE buTam.id_empresa_destino     = piEmp
                   AND buTam.id_sucursal_destino    = piSuc
                   AND buTam.id_tipotambor_destino  = piTip
                   AND buTam.nromov_destino         = piNro
                 NO-LOCK.
    dKil = dKil + buTam.kilos_tambor.    
  END.

  RETURN dKil - pdKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOF Procedure 
FUNCTION getOF RETURNS INTEGER
  (piContrato AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND FIRST contratos WHERE contratos.id_contrato = piContrato
                       NO-LOCK NO-ERROR.
  IF AVAILABLE contratos THEN
    RETURN contratos.orden_fabricacion.
  ELSE 
    RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrdenReporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdenReporte Procedure 
FUNCTION getOrdenReporte RETURNS INTEGER
  (piTipoTambor AS INTEGER, 
   piArticulo   AS INTEGER, 
   piEstado     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iOrden AS INTEGER    NO-UNDO.
  
  iOrden = 1000.
  FOR FIRST ttOrdenReporte WHERE ttOrdenReporte.id_tipotambor = piTipoTambor
                             AND (IF piArticulo <> 0 THEN ttOrdenReporte.id_articulo    = piArticulo ELSE TRUE)
                             AND (IF piEstado   <> 0 THEN ttOrdenReporte.id_estado      = piEstado   ELSE TRUE)
                          BY ttOrdenReporte.id_tipotambor BY ttOrdenReporte.id_articulo BY ttOrdenReporte.id_estado.    
    iOrden = ttOrdenReporte.id_orden.

  END.
  



/*
  FIND FIRST ttOrdenReporte NO-LOCK NO-ERROR.

  FIND FIRST ttOrdenReporte WHERE ttOrdenReporte.id_tipotambor = piTipoTambor
                              AND (IF piArticulo = 0 THEN TRUE ELSE ttOrdenReporte.id_articulo   = piArticulo)
                              AND (IF piEstado   = 0 THEN TRUE ELSE ttOrdenReporte.id_estado     = piEstado)
                            NO-LOCK NO-ERROR.
  IF AVAILABLE ttOrdenReporte THEN
    iOrden = ttOrdenReporte.id_orden.
  ELSE
    iOrden = 99.
*/

  RETURN iOrden.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRNPA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRNPA Procedure 
FUNCTION getRNPA RETURNS CHARACTER
  (piArt AS INTEGER, 
   piCal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_calidad  = piCal
                                   AND r_productos_calidad.id_articulo = piArt
                                 NO-LOCK NO-ERROR.
                
  IF AVAILABLE r_productos_calidad THEN
    RETURN  r_productos_calidad.rnpa.
  ELSE
    RETURN "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresCuadrante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTamboresCuadrante Procedure 
FUNCTION getTamboresCuadrante RETURNS CHARACTER
  (piSuc AS INTEGER,
   piCam AS INTEGER,
   piArt AS INTEGER,
   pcRow AS CHARACTER,
   pcCol AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE BUFFER buCam FOR tambores_industria.


  FOR EACH buCam WHERE buCam.id_sucursal_ubicacion  = piSuc
                   AND buCam.id_locacion_ubicacion  = 4
                   AND buCam.id_sucursal_camara     = piSuc
                   AND buCam.id_camara              = piCam
                   AND buCam.nro_fila_camara        = pcRow
                   AND buCam.nro_columna_camara     = pcCol
                   AND (IF piArt <> 0 THEN buCam.id_articulo = piArt ELSE TRUE) 
                 BREAK BY buCam.nromov.
    i = i + 1.
    IF LAST-OF(buCam.nromov) THEN DO:
      cArt = getDescArticulo(buCam.id_articulo).
      CREATE ttMapaCamara.
      ASSIGN ttMapaCamara.camara    = "Camara # " + STRING(piCam)
             ttMapaCamara.fila      = pcRow
             ttMapaCamara.columna   = pcCol
             ttMapaCamara.id_lote   = buCam.id_lote
             ttMapaCamara.anio      = buCam.anio
             ttMapaCamara.articulo  = cArt
             ttMapaCamara.tbs       = i.
      /*
      cRet = cRet + 
             STRING(buCam.nromov)  + CHR(1) + 
             STRING(buCam.id_lote) + CHR(1) + 
             STRING(buCam.anio)    + CHR(1) + 
             cArt                  + CHR(1) + 
             STRING(i)             + CHR(10).
      */       
      i = 0.
    END.
    
  END.

  /*cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).*/

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTaraEnvase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTaraEnvase Procedure 
FUNCTION getTaraEnvase RETURNS DECIMAL
  (piEnv AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST envases_prod WHERE envases_prod.id_envase = piEnv NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.tara.
  ELSE 
    RETURN 0.00.   

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
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  ASSIGN iEmp = piEmp
         iSuc = piSuc
         iTip = piTip
         iNro = piNro.

  cRet = "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" .

  IF piTip = 6 THEN DO: /*aceite*/
    cRet = "Aceite".
    RETURN cRet.
  END.

  IF piTip = 9 THEN DO: /*terceros*/
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = iEmp
                                  AND tambores_industria.id_sucursal    = iSuc
                                  AND tambores_industria.id_tipotambor  = iTip
                                  AND tambores_industria.nromov         = piNro
                                NO-LOCK.
      k = k + tambores_industria.kilos_tambor.      
    END.
    cRet = "0" + CHR(1) + 
           "0" + CHR(1) + 
           "0" + CHR(1) + 
           "0" + CHR(1) + 
           "0" + CHR(1) + 
           STRING(k) + CHR(1) + 
           "0" + CHR(1) + 
           "0" .
  END.
  
  IF piTip = 4 THEN DO: /*tambor de sobrante*/
    FOR FIRST sobrante WHERE sobrante.id_empresa             = piEmp
                         AND sobrante.id_sucursal            = piSuc
                         AND sobrante.id_tipotambor_sobrante = piTip
                         AND sobrante.nromov_sobrante        = piNro
                       NO-LOCK.
      ASSIGN iEmp = sobrante.id_empresa
             iSuc = sobrante.id_sucursal
             iTip = sobrante.id_tipotambor
             iNro = sobrante.nromov.

    END.
  END.

  IF piTip = 5 THEN DO: /*tambor de arrastre*/
    FOR FIRST arrastre WHERE arrastre.id_empresa             = piEmp
                         AND arrastre.id_sucursal            = piSuc
                         AND arrastre.id_tipotambor_arrastre = piTip
                         AND arrastre.nromov_arrastre        = piNro
                       NO-LOCK.
      ASSIGN iEmp = arrastre.id_empresa
             iSuc = arrastre.id_sucursal
             iTip = arrastre.id_tipotambor
             iNro = arrastre.nromov.

    END.
  END.

  IF piTip = 1 THEN DO:
    FOR FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = piEmp
                                AND produccion_jugo.id_sucursal   = piSuc
                                AND produccion_jugo.id_tipotambor = piTip
                                AND produccion_jugo.nromov        = piNro
                              NO-LOCK.
      cRet = STRING(produccion_jugo.acidez_w_v) + CHR(1) + 
             STRING(produccion_jugo.acidez_w_w) + CHR(1) + 
             STRING(produccion_jugo.bx_20_20)   + CHR(1) + 
             STRING(produccion_jugo.bx_correg)  + CHR(1) + 
             STRING(produccion_jugo.pulpa)      + CHR(1) + 
             STRING(produccion_jugo.litros)     + CHR(1) + 
             STRING(produccion_jugo.ratio)      + CHR(1) + 
             STRING(produccion_jugo.vitaminac).
      RETURN cRet.
    END.
  END.



  FIND FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa    = iEmp
                                 AND inspecciones_lote.id_sucursal   = iSuc
                                 AND inspecciones_lote.id_tipotambor = iTip
                                 AND inspecciones_lote.nromov        = iNro
                                 AND inspecciones_lote.final         = TRUE
                               NO-LOCK NO-ERROR.
  IF AVAILABLE inspecciones_lote THEN DO:
    cRet = STRING(inspecciones_lote.acidez_w_v) + CHR(1) + 
           STRING(inspecciones_lote.acidez_w_w) + CHR(1) + 
           STRING(inspecciones_lote.bx_20_20)   + CHR(1) + 
           STRING(inspecciones_lote.bx_correg)  + CHR(1) + 
           STRING(inspecciones_lote.hora)       + CHR(1) + 
           STRING(inspecciones_lote.litros)     + CHR(1) + 
           STRING(inspecciones_lote.ratio)      + CHR(1) + 
           STRING(inspecciones_lote.porcentaje_pulpa).
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValoresAnalisisProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValoresAnalisisProduccion Procedure 
FUNCTION getValoresAnalisisProduccion RETURNS CHARACTER
  (piEmp AS INTEGER,
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) +
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00" + CHR(1) + 
         "0.00".

  FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = piEmp
                               AND produccion_jugo.id_sucursal   = piSuc
                               AND produccion_jugo.id_tipotambor = piTip
                               AND produccion_jugo.nromov        = piNro
                              NO-LOCK NO-ERROR.
  IF AVAILABLE produccion_jugo THEN DO:
    cRet = STRING(produccion_jugo.Acidez_w_v) + CHR(1) + 
           STRING(produccion_jugo.Acidez_w_w) + CHR(1) + 
           STRING(produccion_jugo.bx_20_20)   + CHR(1) + 
           STRING(produccion_jugo.bx_correg)  + CHR(1) + 
           STRING(produccion_jugo.Pulpa)      + CHR(1) + 
           STRING(produccion_jugo.litros)     + CHR(1) +  
           STRING(produccion_jugo.Sodio)      + CHR(1) + 
           STRING(produccion_jugo.vitaminac)  + CHR(1) + 
           STRING(produccion_jugo.nitrogeno)  + CHR(1) + 
           STRING(produccion_jugo.abs520)     + CHR(1) + 
           STRING(produccion_jugo.abs_8_430)  + CHR(1) + 
           STRING(produccion_jugo.benzoato)   + CHR(1) + 
           STRING(produccion_jugo.bisulfito)  + CHR(1) + 
           STRING(produccion_jugo.pulpa_85)   + CHR(1) + 
           STRING(produccion_jugo.ratio)      + CHR(1) + 
           STRING(produccion_jugo.t_600)      + CHR(1) + 
           STRING(produccion_jugo.pesticida).
           
  END.
  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValoresLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValoresLote Procedure 
FUNCTION getValoresLote RETURNS CHARACTER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0".

  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmp
                          AND lotes_jugo.id_sucursal   = piSuc
                          AND lotes_jugo.id_tipotambor = piTip
                          AND lotes_jugo.nromov        = piNro
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    cRet = STRING(lotes_jugo.id_lote)     + CHR(1) + 
           STRING(lotes_jugo.anio)        + CHR(1) + 
           STRING(lotes_jugo.id_articulo) + CHR(1) + 
           STRING(lotes_jugo.id_calidad)  + CHR(1) + 
           STRING(lotes_jugo.id_envase)   + CHR(1) + 
           STRING(lotes_jugo.Pulpa)       + CHR(1) + 
           STRING(lotes_jugo.concentracion_mesh).
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

