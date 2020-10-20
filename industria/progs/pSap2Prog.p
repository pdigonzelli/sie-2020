DEFINE VARIABLE chXls    AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSht    AS COM-HANDLE         NO-UNDO.
DEFINE VARIABLE hLibCom  AS HANDLE.
DEFINE VARIABLE hLibCon  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLib     AS HANDLE     NO-UNDO.
DEFINE VARIABLE i        AS INTEGER        NO-UNDO.
DEFINE VARIABLE iLastRow AS INTEGER        NO-UNDO.

/* variables cabecera */
DEFINE VARIABLE cContrato   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAnio       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTipoCont   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPlazo      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTipoPlazo  AS INTEGER    NO-UNDO.
DEFINE VARIABLE lMercado    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iCliente    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrdenFab   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iContSap    AS INTEGER    NO-UNDO.
/* variables de parte */
DEFINE VARIABLE iPosicion   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iArticulo   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCalidad    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEnvase     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTipoUniVta AS INTEGER    NO-UNDO.
DEFINE VARIABLE dEmbarque   AS DATE       NO-UNDO.
DEFINE VARIABLE iFrom       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iDestino    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSemana     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCantidad   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iClausula   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iVapor      AS INTEGER    NO-UNDO.
DEFINE VARIABLE fPrecioOrig AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iTipoUniOri AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMoneda     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cMatnr      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cConPro  AS CHARACTER  NO-UNDO.

RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libProg2Sap.p'). 
DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().
chXls = DYNAMIC-FUNCTION('openExcelApplication' IN hLibCom, 'D:\facundoj\sap\interfaseindustria\ContratosIndustriaSap.xls', TRUE).
chSht = chXls:Sheets:ITEM(1).
iLastRow = 4.
/* el excel debe venir ordenado por contrato y posicion */
DO i = 2 TO iLastRow:
  /* tomo datos de excel */
  /* variables cabecera */
  cContrato   = chSht:Range("A" + STRING(i)):VALUE.
  iAnio       = YEAR(DATE(chSht:Range("R" + STRING(i)):VALUE)).
  iTipoCont   = 1.
  iPlazo      = INTEGER(ENTRY(1, DYNAMIC-FUNCTION('getPlazo' IN hLib, chSht:Range("G" + STRING(i)):VALUE))).
  iTipoPlazo  = INTEGER(ENTRY(2, DYNAMIC-FUNCTION('getPlazo' IN hLib, chSht:Range("G" + STRING(i)):VALUE))).
  lMercado    = IF chSht:Range("F" + STRING(i)):VALUE = 'EX' THEN TRUE ELSE FALSE.
  iCliente    = DYNAMIC-FUNCTION('getClienteSap' IN hLib, chSht:Range("E" + STRING(i)):VALUE).
  iOrdenFab   = chSht:Range("A" + STRING(i)):VALUE.
  iContSap    = chSht:Range("A" + STRING(i)):VALUE.

  /* variables items */
  cMatnr      = chSht:Range("C" + STRING(i)):VALUE.
  iPosicion   = INTEGER(chSht:Range("B" + STRING(i)):VALUE).
  iArticulo   = INTEGER(ENTRY(1, DYNAMIC-FUNCTION('getProductoProgress' IN hLib, cMatnr))).
  iCalidad    = INTEGER(ENTRY(2, DYNAMIC-FUNCTION('getProductoProgress' IN hLib, cMatnr))).
  iEnvase     = INTEGER(ENTRY(3, DYNAMIC-FUNCTION('getProductoProgress' IN hLib, cMatnr))).
  iTipoUniVta = INTEGER(DYNAMIC-FUNCTION('getTipoUnidadVta' IN hLib, chSht:Range("P" + STRING(i)):VALUE)).
  dEmbarque   = DATE(chSht:Range("S" + STRING(i)):VALUE).
  iFrom       = INTEGER(chSht:Range("K" + STRING(i)):VALUE).
  iDestino    = INTEGER(chSht:Range("L" + STRING(i)):VALUE).
  iSemana     = DYNAMIC-FUNCTION('getNroSemana' IN hLibCom, dEmbarque).
  iCantidad   = INTEGER(chSht:Range("Q" + STRING(i)):VALUE).
  iClausula   = DYNAMIC-FUNCTION('getClausula' IN hLib, chSht:Range("N" + STRING(i)):VALUE).
  iVapor      = INTEGER(chSht:Range("H" + STRING(i)):VALUE).
  fPrecioOrig = DECIMAL(chSht:Range("T" + STRING(i)):VALUE).
  iTipoUniOri = iTipoUniVta.
  iMoneda     = DYNAMIC-FUNCTION('getMoneda' IN hLib, chSht:Range("" + STRING(i)):VALUE).

  FIND FIRST contratos
       WHERE contratos.id_contrato_sap = INTEGER(cContrato).
  /* no existe el contrato en industria */
    /* insert de contratos */
  IF NOT AVAILABLE contratos THEN CREATE contratos.
  ASSIGN contratos.id_contrato       = cContrato
         contratos.anio              = iAnio
         contratos.id_tipo_contrato  = iTipoCont
         contratos.fecha             = dEmbarque
         contratos.id_contrato_sap   = iContSap
         contratos.id_cliente        = iCliente
         contratos.mercado           = lMercado
         contratos.plazo             = iPlazo
         contratos.id_tipo_plazo     = iTipoPlazo
         contratos.orden_fabricacion = iOrdenFab
         contratos.c_usuario         = USERID('UserDB')
         contratos.c_fecha           = TODAY
         contratos.c_hora            = STRING(TIME, 'HH:MM:SS').

  /* insert items_contratos */
  FIND FIRST items_contratos
       WHERE items_contratos.id_contrato      = cContrato
         AND items_contratos.anio             = iAnio
         AND items_contratos.id_tipo_contrato = iTipoCont
         AND items_contratos.ITEM             = iPosicion.
  IF NOT AVAILABLE items_contratos THEN CREATE items_contratos.
  ASSIGN items_contratos.id_contrato                  = cContrato
         items_contratos.anio                         = iAnio
         items_contratos.id_tipo_contrato             = iTipoCont
         items_contratos.ITEM                         = iPosicion
         items_contratos.id_articulo                  = iArticulo
         items_contratos.id_calidad                   = iCalidad
         items_contratos.id_envase                    = iEnvase
         items_contratos.id_tipo_unidad_venta         = iTipoUniVta
         items_contratos.embarque_estimado            = dEmbarque
         items_contratos.id_from                      = iFrom
         items_contratos.id_destino                   = iDestino
         items_contratos.semana_entrega               = iSemana
         items_contratos.anio_semana_entrega          = iAnio
         items_contratos.cantidad                     = iCantidad
         items_contratos.id_clausula                  = iClausula
         items_contratos.id_vapor                     = iVapor
         items_contratos.precio_origen                = fPrecioOrig
         items_contratos.id_moneda_origen             = iMoneda
         items_contratos.id_tipo_unidad_venta_origen  = iTipoUniVta.
         
END.







