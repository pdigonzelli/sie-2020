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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-zebraBarCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraBarCode Procedure 
FUNCTION zebraBarCode RETURNS CHARACTER
  (pcL AS CHARACTER, 
   pcM AS CHARACTER,
   piX AS INTEGER, 
   piY AS INTEGER,    
   pcO AS CHARACTER,        /*orientacion N Horizontal R Vertical*/       
   piH AS INTEGER,          /*alto de las barras*/
   piW AS INTEGER,          /*ancho del codigo barra*/
   pcN AS CHARACTER,        /*imprime el nro del codigo Y/N*/
   pcU AS CHARACTER,        /*imprime el nro arriba del codigo*/
   pcData AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraBoxLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraBoxLine Procedure 
FUNCTION zebraBoxLine RETURNS CHARACTER
  (pcX1 AS INTEGER, 
   pcY1 AS INTEGER, 
   pcX2 AS INTEGER, 
   pcY2 AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraClose Procedure 
FUNCTION zebraClose RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraFieldData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraFieldData Procedure 
FUNCTION zebraFieldData RETURNS CHARACTER
  (pcL AS CHARACTER, 
   pc0 AS CHARACTER,
   piH AS INTEGER,
   piW AS INTEGER, 
   piX AS INTEGER, 
   piY AS INTEGER,    
   pcData AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraInitialize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraInitialize Procedure 
FUNCTION zebraInitialize RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraLabelFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraLabelFooter Procedure 
FUNCTION zebraLabelFooter RETURNS CHARACTER
  (pcGrados AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraLabelHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraLabelHeader Procedure 
FUNCTION zebraLabelHeader RETURNS CHARACTER
  (pcPlanta AS CHARACTER, 
   pcRNPA   AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraPrintGraphic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zebraPrintGraphic Procedure 
FUNCTION zebraPrintGraphic RETURNS CHARACTER
  (pcZebra AS CHARACTER,
   pcGRF   AS CHARACTER, 
   piX     AS INTEGER, 
   piY     AS INTEGER, 
   piH     AS INTEGER,
   piW     AS INTEGER)  FORWARD.

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
         HEIGHT             = 13.19
         WIDTH              = 59.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-etqAperturaLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqAperturaLote Procedure 
PROCEDURE etqAperturaLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cZebra AS CHARACTER  NO-UNDO.


  cZebra = "\\192.168.2.11\zebra".
  
  /*zebra facundoj*/
  /*cZebra = "\\facundoj\ibm4400".*/
  
  FOR EACH tambores_industria 
      WHERE nromov = piNroMov.
      NO-LOCK.
      RUN etqLabelTambor (piEmpresa,
                          piSucursal,
                          piTipoTambor,
                          piNroMov, 
                          tambores_industria.id_tambor, 
                          cZebra).

  END.
  
 /* 
  FIND FIRST composicion_lote WHERE composicion_lote.id_empresa    = piEmpresa
                                AND composicion_lote.id_sucursal   = piSucursal
                                AND composicion_lote.id_tipotambor = piTipoTambor
                                AND composicion_lote.nromov        = piNroMov
                              NO-LOCK NO-ERROR.
  IF AVAILABLE composicion_lote THEN  
    RUN zebra_lotes_jugo.p (ROWID(composicion_lote)).
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqAperturaLote".
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqArrastre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqArrastre Procedure 
PROCEDURE etqArrastre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.


  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN  
    RUN zebra_lotes_jugo_arrastre.p (ROWID(lotes_jugo)).
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqArrastre".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqConAditivos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqConAditivos Procedure 
PROCEDURE etqConAditivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.


  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN  
    RUN zebra_inspecciones_lote_con_aditivos.p (ROWID(lotes_jugo)).
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqConAditivos".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqInspeccionesLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqInspeccionesLote Procedure 
PROCEDURE etqInspeccionesLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcAdditives  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHeader     AS LOGICAL    NO-UNDO.


  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN  
    RUN etqJugo (piEmpresa, 
                 piSucursal,
                 piTipoTambor,
                 piNroMov, 
                 pcAdditives, 
                 plHeader).

    /*RUN zebra_inspecciones_lote.p (ROWID(lotes_jugo)).*/
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF lotes_jugo.id_articulo <> 52 THEN RETURN.
  /*me manda un mail cada vez que se rebatchean tambores en cargas*/
  RUN ..\industria\sendMail.p("",                               
                              2,                                
                              "se imprimieron las etiquetas del lote " + STRING(lotes_jugo.id_lote) + "/" + STRING(lotes_jugo.anio),          
                              "", 
                              "facundoj@sa-sanmiguel.com,vanesat@sa-sanmiguel.com", 
                              "").               


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqInspeccionesLoteRango) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqInspeccionesLoteRango Procedure 
PROCEDURE etqInspeccionesLoteRango :
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
  DEFINE INPUT  PARAMETER pcAdditives  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHeader     AS LOGICAL    NO-UNDO.


  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN  
    RUN etqRangoJugo (piEmpresa, 
                      piSucursal,
                      piTipoTambor,
                      piNroMov, 
                      piDesde, 
                      piHasta, 
                      pcAdditives, 
                      plHeader).

    /*RUN zebra_inspecciones_lote_rango2.p (ROWID(lotes_jugo), piDesde, piHasta).*/
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqJugo Procedure 
PROCEDURE etqJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcAdd AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHea AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cPlanta AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cZebra  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnal   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRnpa   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQtys   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAddit  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTbs    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrixCo AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcGpl  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGall   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTara   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPulp   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  IF piSuc = 96 THEN 
    cZebra = "c:\lpt1".  
  ELSE
    cZebra = "\\192.168.2.11\zebra".

  /*zebra facundoj*/
  /*cZebra = "\\192.168.1.104\ibm4400".*/
  
  

  ASSIGN cAnal   = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
         dBrix   = DECIMAL(ENTRY(3, cAnal, CHR(1))).
         dBrixCo = DECIMAL(ENTRY(4, cAnal, CHR(1))).
         dAcid   = DECIMAL(ENTRY(2, cAnal, CHR(1))).
         dAcGpl  = DECIMAL(ENTRY(1, cAnal, CHR(1))).
         dPulp   = DECIMAL(ENTRY(8, cAnal, CHR(1))).
         cQtys   = DYNAMIC-FUNCTION('getCantidadesLote' IN hLib, piEmp, piSuc, piTip, piNro).
         iTbs    = INTEGER(ENTRY(1, cQtys, CHR(1))).


  /*etiqueta nro 0*/
  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    iDesde = tambores_industria.id_tambor.
    IF tambores_industria.id_sucursal = 95 THEN
      cPlanta = "Ruta Provincial 301Km33-Famailla-Tucuman".
    ELSE
      cPlanta = "Industrial Plant N 1-Lavalle 4001-Tel: (0381) 4512600-Fax: (0381) 4512612".

    ASSIGN cRnpa = DYNAMIC-FUNCTION('getRNPA' IN hLib, tambores_industria.id_articulo, tambores_industria.id_calidad)
           cArt  = DYNAMIC-FUNCTION('getDescArticuloIngles' IN hLib, tambores_industria.id_articulo)
           dGall = DYNAMIC-FUNCTION('getGalonesTambor' IN hLib, dBrixCo, tambores_industria.kilos_tambor)
           dTara = DYNAMIC-FUNCTION('getTaraEnvase' IN hLib, tambores_industria.id_envase)
           .

    IF tambores_industria.id_articulo = 52 THEN
      cArt = "FROZEN CONCENTRATED CLOUDY LEMON JUICE".

    IF tambores_industria.id_articulo = 53 THEN
      cArt = "FROZEN CONCENTRATED CLEAR LEMON JUICE".

    cCod = DYNAMIC-FUNCTION('getComposeNroLote' IN hLibTam, tambores_industria.id_sucursal,
                                                            tambores_industria.id_articulo,
                                                            tambores_industria.id_lote,
                                                            tambores_industria.anio).

    cCod = ENTRY(3, cCod, ".") + "." +
           ENTRY(4, cCod, ".").

    /*cCod = "". eliminar esta linea cuando se libere la codificacion nueva de lotes*/

    RUN etqLabelJugo (cZebra, 
                      cPlanta, 
                      cRnpa, 
                      cArt, 
                      tambores_industria.fecha, 
                      tambores_industria.id_lote,
                      tambores_industria.anio - 2000, 
                      dBrix, 
                      0, 
                      iTbs, 
                      dAcid,
                      dGall, 
                      dBrixCo, 
                      tambores_industria.kilos_tambor + dTara, 
                      tambores_industria.kilos_tambor, 
                      dTara, 
                      dAcGpl, 
                      dPulp, 
                      cCod, 
                      pcAdd, 
                      plHea).


  END. /*if available tambores_industria ...*/

  FIND LAST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                 AND tambores_industria.id_sucursal   = piSuc
                                 AND tambores_industria.id_tipotambor = piTip
                                 AND tambores_industria.nromov        = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    iHasta = tambores_industria.id_tambor.
  END.


  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro
                                AND tambores_industria.id_tambor    >= iDesde
                                AND tambores_industria.id_tambor    <= iHasta
                              NO-LOCK.
    dTara = DYNAMIC-FUNCTION('getTaraEnvase' IN hLib, tambores_industria.id_envase).
    dGall = DYNAMIC-FUNCTION('getGalonesTambor' IN hLib, dBrixCo, tambores_industria.kilos_tambor).

    RUN etqLabelJugo (cZebra, 
                      cPlanta, 
                      cRnpa, 
                      cArt, 
                      tambores_industria.fecha, 
                      tambores_industria.id_lote,
                      tambores_industria.anio - 2000, 
                      dBrix, 
                      tambores_industria.id_tambor, 
                      iTbs, 
                      dAcid,
                      dGall, 
                      dBrixCo, 
                      tambores_industria.kilos_tambor + dTara, 
                      tambores_industria.kilos_tambor, 
                      dTara, 
                      dAcGpl, 
                      dPulp, 
                      cCod, 
                      pcAdd, 
                      plHea).

    RUN etqLabelJugo (cZebra, 
                      cPlanta, 
                      cRnpa, 
                      cArt, 
                      tambores_industria.fecha, 
                      tambores_industria.id_lote,
                      tambores_industria.anio - 2000, 
                      dBrix, 
                      tambores_industria.id_tambor, 
                      iTbs, 
                      dAcid,
                      dGall, 
                      dBrixCo, 
                      tambores_industria.kilos_tambor + dTara, 
                      tambores_industria.kilos_tambor, 
                      dTara, 
                      dAcGpl, 
                      dPulp, 
                      cCod, 
                      pcAdd, 
                      plHea).
  END. /*for each tambores_industria ...*/
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelAceiteComercialPreimp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelAceiteComercialPreimp Procedure 
PROCEDURE etqLabelAceiteComercialPreimp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piUnidades  AS INTEGER    NO-UNDO.

  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  zebraBoxLine(05, 70, 750, 1190).
  zebraPrintGraphic(pcZebra, "..\industria\lgsmver.grf", 625, 500, 2, 2).
  zebraFieldData("0","R", 20, 20, 620, 650, "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria y Financiera").

  zebraFieldData("0","R", 25, 25, 700, 100, "C.I. 23.214").
  zebraFieldData("0","R", 25, 25, 630, 100, "R.N.E. 23000573").

  zebraBoxLine(610, 70, 1, 1190).

  zebraFieldData("0","R", 30, 30, 565, 650, "PLANTA INDUSTRIAL:").
  zebraFieldData("0","R", 25, 25, 520, 220, "Ruta Provincial 301 Km 33 - Famailla - Tucuman - Argentina (4132) Tel: 54-3863-461427").

  zebraBoxLine(510, 70, 1, 1190).

  zebraFieldData("0","R", 130, 110, 250, 150, "ESSENTIAL LEMON OIL").
  zebraFieldData("0","R", 130, 100, 50, 150, "PRODUCT OF ARGENTINA").
  

  zebraClose().
  OUTPUT CLOSE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelJugo Procedure 
PROCEDURE etqLabelJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcPlanta    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcRNPA      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcArticulo  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ptFecha     AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBrix      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambor    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambores  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdAcidez    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdGallDrum  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBrixCorr  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdPesoBruto AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdPesoNeto  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdGpl       AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdPulp      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pcCodigo    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcAdditives AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHeader    AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod  AS CHARACTER  NO-UNDO.

  IF pcCodigo <> "" THEN
    cCod = pcCodigo.
  ELSE
    cCod = STRING(piLote) + "/" + STRING(piAnio, "99").


  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  /*zebraBoxLine(05, 10, 750, 1170).*/
  zebraBoxLine(05, 70, 750, 1130).

  /*header*/
  IF plHeader THEN
    zebraLabelHeader(pcPlanta, pcRNPA).
  
  /*body*/
  zebraFieldData("0", "", 40, 30, 10, 183, "PRODUCT: " + pcArticulo).
  zebraBoxLine(05, 220, 750, 3).
  
  zebraBoxLine(385, 220, 3, 780).  /*linea vertical*/

  zebraFieldData("0", "", 30, 25, 10, 225, "Fill Date:"). 
  zebraFieldData("0", "", 70, 70, 110, 265, STRING(ptFecha)). /*fecha*/
  zebraFieldData("0", "", 30, 25, 390, 225, "Tracedbility Data:"). 
  zebraFieldData("0", "", 70, 60, 410, 265, cCod). /*lote*/

  
  zebraBoxLine(05, 350, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 355, "Brix Uncorrected:"). 
  zebraFieldData("0", "", 70, 70, 110, 415, STRING(pdBrix, ">>>,>>9.99")). /*brix*/
  zebraFieldData("0", "", 30, 25, 390, 355, "Drum N:"). 
  zebraFieldData("0", "", 70, 70, 410, 415, STRING(piTambor) + "/" + STRING(piTambores)). /*id_tambor*/


  zebraBoxLine(05, 480, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 485, "Acidity w/w:"). 
  zebraFieldData("0", "", 70, 70, 110, 545, STRING(pdAcidez, ">>>,>>9.99")). /*acidez*/
  zebraFieldData("0", "", 30, 25, 390, 485, "Gall/Drum:"). 
  zebraFieldData("0", "", 70, 70, 410, 545, TRIM(STRING(pdGallDrum, ">>>,>>>,>>9.99"))). /*gall_drumr*/
  
  
  zebraBoxLine(05, 610, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 615, "Brix Corrected:"). 
  zebraFieldData("0", "", 70, 70, 110, 665, STRING(pdBrixCorr, ">>>,>>9.99")). /*brix corregido*/
  zebraFieldData("0", "", 30, 25, 390, 615, "Gross Weight:"). 
  zebraFieldData("0", "", 70, 70, 410, 665, TRIM(STRING(pdPesoBruto))). /*peso_bruto*/
  zebraFieldData("0", "", 30, 25, 700, 700, "Kg").

  
  zebraBoxLine(05, 740, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 745, "Acidity (GPL):"). 
  zebraFieldData("0", "", 30, 25, 10, 775, "as anhydrous citirc acid (ACA)"). 
  zebraFieldData("0", "", 70, 70, 110, 805, STRING(pdGpl, ">>>,>>9.99")). /*acidez gpl*/  
  zebraFieldData("0", "", 30, 25, 390, 745, "Tare:"). 
  zebraFieldData("0", "", 70, 70, 410, 805, TRIM(STRING(pdTara))). /*peso_bruto*/
  zebraFieldData("0", "", 30, 25, 700, 840, "Kg").

  
  zebraBoxLine(05, 870, 750, 3).
  IF pdPulp > 0 THEN DO:  
    zebraFieldData("0", "", 30, 25, 50, 885, "Pulp %:"). 
    zebraFieldData("0", "", 40, 35, 150, 882, STRING(pdPulp, ">9.99") ). 
  END.
  zebraBoxLine(05, 915, 382, 3).
  IF LENGTH(pcAdditives) >= 22 THEN
    zebraFieldData("0", "", 30, 16, 50, 930, "Additives: " + pcAdditives). 
  ELSE
    zebraFieldData("0", "", 30, 25, 50, 930, "Additives: " + pcAdditives). 

  zebraBoxLine(05, 957, 382, 3).
  zebraFieldData("0", "", 30, 25, 50, 972, "Product Code: " + ENTRY(1, cCod, ".")). 

  zebraFieldData("0", "", 30, 25, 390, 875, "Net Weight:"). 
  zebraFieldData("0", "", 70, 70, 410, 935, TRIM(STRING(pdPesoNeto))). /*peso_neto*/
  zebraFieldData("0", "", 30, 25, 700, 970, "Kg").

  zebraBoxLine(05, 1000, 750, 3).

  /*footer*/
  zebraLabelFooter("(-15 +/- 3)").
    
  zebraClose().
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelPepsiPreimpresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelPepsiPreimpresa Procedure 
PROCEDURE etqLabelPepsiPreimpresa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piUnidades  AS INTEGER    NO-UNDO.

  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  zebraBoxLine(05, 70, 750, 1190).
  zebraPrintGraphic(pcZebra, "..\industria\lgsmver.grf", 625, 90, 2, 2).
  zebraFieldData("0","R", 30, 30, 680, 880, "A.G.I.C.I. y F.").
  zebraFieldData("0","R", 20, 20, 620, 250, "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria y Financiera").

  zebraFieldData("0","R", 20, 20, 590, 100, "INDUSTRIAL PLANT:").
  zebraFieldData("0","R", 20, 20, 565, 100, "Ruta Prov. 301 Km. 32 - Estacion Padilla").
  zebraFieldData("0","R", 20, 20, 540, 100, "Famailla - TUCUMAN - ARGENTINA").
  zebraFieldData("0","R", 20, 20, 515, 100, "TEL: (0381) 4512650 / /03863) 461366").

  zebraFieldData("0","R", 20, 20, 565, 1000, "R.N.E. 23001973").
  zebraFieldData("0","R", 20, 20, 540, 1000, "R.N.P.A. 23026750").

  zebraFieldData("0","R", 50, 70, 450, 480, "UNA UNIDAD").
  zebraFieldData("0","R", 40, 50, 415, 210, "JUGO CLARIFICADO DE LIMON (BRIX 55 / 56 gr)").
  zebraFieldData("0","R", 30, 30, 380, 320, "CON DIOXIDO DE AZUFRE COMO ANTIOXIDANTE").
  zebraFieldData("0","R", 30, 30, 345, 320, "CON CONSERVANTE PERMITIDO").
  zebraFieldData("0","R", 30, 30, 310, 320, "DILUCION 1 + 7").
  zebraFieldData("0","R", 30, 30, 275, 320, "(17D 15 LJD)").

  zebraFieldData("0","R", 25, 25, 230, 150, "FABRICADO PARA PEPSI COLA ARGENTINA S.A.C.I.").
  zebraFieldData("0","R", 25, 25, 200, 150, "PARA SUS PRODUCTOS:").
  zebraFieldData("0","R", 25, 25, 170, 400, "- 7UP R.N.P.A. 01007264 - DIET UP R.N.P.A. 02310030").
  zebraFieldData("0","R", 25, 25, 140, 400, "- 7UP LIGHT R.N.P.A. Nro CARP. 017/96 - MIR. LIMONADA R.N.P.A. 01009785").

  zebraFieldData("0","R", 25, 25, 100, 100, "PARTIDA Nro").
  zebraFieldData("0","R", 25, 25, 100, 850, "FECHA DE ELABORACION").

  zebraFieldData("0","R", 30, 30, 60, 100, "PESO BRUTO: 23.35 KILOS").
  zebraFieldData("0","R", 30, 30, 60, 850, "PESO NETO: 22 KILOS").

  zebraBoxLine(50, 70, 1, 1190).

  zebraFieldData("0","R", 30, 30, 10, 100, "PARA USO INDUSTRIAL").
  zebraFieldData("0","R", 30, 30, 10, 850, "INDUSTRIA ARGENTINA").




  zebraClose().
  OUTPUT CLOSE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelPrueba Procedure 
PROCEDURE etqLabelPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.

  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  

  /*impresion vertical*/
/*  PUT CONTROL "^F1,1^A0R,100,N,100,N,N^FD" + "  Patricio Van Messem" "^FS". */
  
  zebraFieldData("0", "R", 150, 100, 450, 120, "Entregar a: "). /*impresion horizontal en el sentido de la etiqueta*/
  zebraFieldData("0", "R", 150, 100, 250, 120, "Patricio Van Messem"). /*impresion horizontal en el sentido de la etiqueta*/
  zebraFieldData("0", "R", 100, 50, 50, 120, "Rte: Facundo Juarez - Sistemas"). /*impresion horizontal en el sentido de la etiqueta*/

  zebraClose().
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelPulpa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelPulpa Procedure 
PROCEDURE etqLabelPulpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcPlanta    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcRNPA      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcArticulo  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER ptFecha     AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piLote      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piAnio      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBrix      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdBrixCorr  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambor    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTambores  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdAcidez    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdGallDrum  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pcRatio     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pdPesoBruto AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdPesoNeto  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdTara      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdMesh      AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pcCodigo    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcTemperat  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHeader    AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod  AS CHARACTER  NO-UNDO.


  IF pcCodigo <> "" THEN
    cCod = pcCodigo.
  ELSE
    cCod = STRING(piLote) + "/" + STRING(piAnio, "99").


  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  /*zebraBoxLine(05, 10, 750, 1170).*/
  zebraBoxLine(05, 70, 750, 1130).

  /*header*/
  IF plHeader THEN
    zebraLabelHeader(pcPlanta, pcRNPA).


  /*body*/
  zebraFieldData("0", "", 40, 30, 10, 183, "PRODUCT: " + pcArticulo).
  zebraBoxLine(05, 220, 750, 3).
  
  zebraBoxLine(385, 220, 3, 780).  /*linea vertical*/

  zebraFieldData("0", "", 30, 25, 10, 225, "Fill Date:"). 
  zebraFieldData("0", "", 70, 70, 110, 265, STRING(ptFecha)). /*fecha*/
  zebraFieldData("0", "", 30, 25, 390, 225, "Tracedbility Data:"). 
  zebraFieldData("0", "", 70, 60, 410, 265, cCod). /*lote*/

  
  zebraBoxLine(05, 350, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 355, "Brix Uncorrected:"). 
  zebraFieldData("0", "", 70, 70, 110, 415, STRING(pdBrix, ">>>,>>9.99")). /*brix*/
  zebraFieldData("0", "", 30, 25, 390, 355, "Drum N:"). 
  zebraFieldData("0", "", 70, 70, 410, 415, STRING(piTambor) + "/" + STRING(piTambores)). /*id_tambor*/


  zebraBoxLine(05, 480, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 485, "Ratio:"). 
  zebraFieldData("0", "", 70, 70, 110, 545, pcRatio). /*ratio*/
  zebraFieldData("0", "", 30, 25, 390, 485, "Gall/Drum:"). 
  zebraFieldData("0", "", 70, 70, 410, 545, TRIM(STRING(pdGallDrum, ">>>,>>>,>>9.99"))). /*gall_drumr*/
  
  
  zebraBoxLine(05, 610, 750, 3).
  zebraFieldData("0", "", 30, 25, 10, 615, "Concentration:"). 
  zebraFieldData("0", "", 70, 70, 110, 665, STRING(pdMesh) + "%"). /*concentration*/
  zebraFieldData("0", "", 30, 25, 390, 615, "Gross Weight:"). 
  zebraFieldData("0", "", 70, 70, 410, 665, TRIM(STRING(pdPesoBruto))). /*peso_bruto*/
  zebraFieldData("0", "", 30, 25, 700, 700, "Kg").

  
  zebraBoxLine(05, 740, 750, 3).
  
  zebraFieldData("0", "", 30, 25, 10, 745, ""). 
  zebraFieldData("0", "", 70, 70, 110, 805, ""). /**/  
  zebraFieldData("0", "", 30, 25, 390, 745, "Tare:"). 
  zebraFieldData("0", "", 70, 70, 410, 805, TRIM(STRING(pdTara))). /*peso_bruto*/
  zebraFieldData("0", "", 30, 25, 700, 840, "Kg").

  zebraBoxLine(05, 915, 382, 3).
  zebraFieldData("0", "", 30, 25, 50, 930, "Additives: Not Applicable" ). 
  zebraBoxLine(05, 957, 382, 3).
  zebraFieldData("0", "", 30, 25, 50, 972, "Product Code: " + ENTRY(1, cCod, ".")). 


  
  zebraBoxLine(05, 870, 750, 3).
   

  zebraFieldData("0", "", 30, 25, 390, 875, "Net Weight:"). 
  zebraFieldData("0", "", 70, 70, 410, 935, TRIM(STRING(pdPesoNeto))). /*peso_neto*/
  zebraFieldData("0", "", 30, 25, 700, 970, "Kg").

  zebraBoxLine(05, 1000, 750, 3).

  /*footer*/
  zebraLabelFooter("(-15 +/- 3)").
    
  zebraClose().
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqLabelTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqLabelTambor Procedure 
PROCEDURE etqLabelTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcZebra AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTam AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTbs AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa     = piEmp
        AND tambores_industria.id_sucursal    = piSuc
        AND tambores_industria.id_tipotambor  = piTip
        AND tambores_industria.nromov         = piNro
      NO-LOCK.
    iTbs = iTbs + 1.

  END.

  FOR FIRST tambores_industria
      WHERE tambores_industria.id_empresa     = piEmp
        AND tambores_industria.id_sucursal    = piSuc
        AND tambores_industria.id_tipotambor  = piTip
        AND tambores_industria.nromov         = piNro
        AND tambores_industria.id_tambor      = piTam
      NO-LOCK.

    cLot = STRING(tambores_industria.id_lote, "999") + "/" + SUBSTRING(STRING(tambores_industria.anio), 3, 2).
    cTam = STRING(tambores_industria.id_tambor, "999") + "/" + STRING(iTbs).

    OUTPUT TO VALUE(pcZebra).
    zebraInitialize().   
    zebraBoxLine(05, 70, 750, 1130).

    zebraFieldData("0", "R", 350, 210, 350, 100, cLot). 
    zebraFieldData("0", "R", 350, 210, 10, 100, cTam). 

    zebraBoxLine(05, 900, 750, 1).

    zebraBarCode("0", "N", 150, 920, "N", 250, 3, "Y", "N", STRING(tambores_industria.id_etiqueta)). 

    zebraClose().
    OUTPUT CLOSE.
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqOilCoca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqOilCoca Procedure 
PROCEDURE etqOilCoca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha      AS DATE       NO-UNDO.


  FIND FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmpresa
                            AND lotes_aceite.id_sucursal   = piSucursal
                            AND lotes_aceite.id_tipotambor = piTipoTambor
                            AND lotes_aceite.nromov        = piNroMov
                          NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_aceite THEN  
    RUN zebra_lotes_aceite_coca.p (ROWID(lotes_aceite),
                                   pdFecha).
  ELSE 
    MESSAGE "No se encontro el lote de aceite" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqOilCoca".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqOilNoCoca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqOilNoCoca Procedure 
PROCEDURE etqOilNoCoca :
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

  FIND FIRST lotes_aceite WHERE lotes_aceite.id_empresa    = piEmpresa
                            AND lotes_aceite.id_sucursal   = piSucursal
                            AND lotes_aceite.id_tipotambor = piTipoTambor
                            AND lotes_aceite.nromov        = piNroMov
                          NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_aceite THEN  
    RUN zebra_lotes_aceite_no_coca.p (ROWID(lotes_aceite),
                                      piDesde, 
                                      piHasta).
  ELSE 
    MESSAGE "No se encontro el lote de aceite" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqOilNoCoca".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqOilSobrante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqOilSobrante Procedure 
PROCEDURE etqOilSobrante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.


  FIND FIRST sobrante_lotes_aceite WHERE sobrante_lotes_aceite.id_empresa             = piEmpresa
                                     AND sobrante_lotes_aceite.id_sucursal            = piSucursal
                                     AND sobrante_lotes_aceite.id_tipotambor_sobrante = piTipoTambor
                                     AND sobrante_lotes_aceite.nromov_sobrante        = piNroMov
                                   NO-LOCK NO-ERROR.
  IF AVAILABLE sobrante_lotes_aceite THEN  
    RUN zebra_lotes_aceite_sobrante.p (ROWID(sobrante_lotes_aceite)).
  ELSE 
    MESSAGE "No se encontro el lote de aceite" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqOilSobrante".




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqPruebaEtqInviolable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqPruebaEtqInviolable Procedure 
PROCEDURE etqPruebaEtqInviolable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcZebra AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcEtiq  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.


  OUTPUT TO VALUE(pcZebra).
  zebraInitialize().   
  zebraBoxLine(01, 60, 760, 1170).

  zebraBarCode("0", "N", 25, 225, "R", 80, 3, "Y", "N", pcEtiq). 
  zebraBarCode("0", "N", 650, 225, "R", 80, 3, "Y", "N", pcEtiq). 
  
  zebraClose().
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqPulpa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqPulpa Procedure 
PROCEDURE etqPulpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcTem AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cPlanta AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cZebra  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnal   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRnpa   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQtys   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRatio  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTbs    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrixCo AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGall   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTara   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cCod    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  


  IF piSuc = 96 THEN 
    cZebra = "c:\lpt1".  
  ELSE
    cZebra = "\\192.168.2.11\zebra".

  /*cZebra = "\\192.168.1.104\ibm4400".*/
  
  

  ASSIGN cAnal   = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
         dBrix   = DECIMAL(ENTRY(3, cAnal, CHR(1))).
         dBrixCo = DECIMAL(ENTRY(4, cAnal, CHR(1))).
         dAcid   = DECIMAL(ENTRY(2, cAnal, CHR(1))).
         cQtys   = DYNAMIC-FUNCTION('getCantidadesLote' IN hLib, piEmp, piSuc, piTip, piNro).
         iTbs    = INTEGER(ENTRY(1, cQtys, CHR(1))).
         cRatio  = ENTRY(7, cAnal, CHR(1)).

  /*etiqueta nro 0*/
  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    iDesde = tambores_industria.id_tambor.
    IF tambores_industria.id_sucursal = 95 THEN
      cPlanta = "Ruta Provincial 301Km33-Famailla-Tucuman".
    ELSE
      cPlanta = "Industrial Plant N 1-Lavalle 4001-Tel: (0381) 4512600-Fax: (0381) 4512612".

    ASSIGN cRnpa = DYNAMIC-FUNCTION('getRNPA' IN hLib, tambores_industria.id_articulo, tambores_industria.id_calidad)
           cArt  = DYNAMIC-FUNCTION('getDescArticuloIngles' IN hLib, tambores_industria.id_articulo)
           dGall = DYNAMIC-FUNCTION('getGalonesTambor' IN hLib, dBrixCo, tambores_industria.kilos_tambor)
           dTara = DYNAMIC-FUNCTION('getTaraEnvase' IN hLib, tambores_industria.id_envase).

    cCod = DYNAMIC-FUNCTION('getComposeNroLote' IN hLibTam, tambores_industria.id_sucursal,
                                                            tambores_industria.id_articulo,
                                                            tambores_industria.id_lote,
                                                            tambores_industria.anio).

    cCod = ENTRY(3, cCod, ".") + "." +
           ENTRY(4, cCod, ".").

    /*cCod = "". eliminar esta linea cuando se libere la codificacion nueva de lotes*/

    RUN etqLabelPulpa (cZebra, 
                       cPlanta, 
                       cRnpa, 
                       cArt, 
                       tambores_industria.fecha, 
                       tambores_industria.id_lote,
                       tambores_industria.anio - 2000, 
                       dBrix, 
                       0, 
                       iTbs, 
                       dAcid,
                       dGall, 
                       cRatio, 
                       tambores_industria.kilos_tambor + dTara, 
                       tambores_industria.kilos_tambor, 
                       dTara, 
                       0.00, 
                       cCod, 
                       pcTem, 
                       TRUE).


  END.

  FIND LAST tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                  AND tambores_industria.id_sucursal   = piSuc
                                  AND tambores_industria.id_tipotambor = piTip
                                  AND tambores_industria.nromov        = piNro
                                NO-LOCK NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    iHasta = tambores_industria.id_tambor.
  END.


  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro
                                AND tambores_industria.id_tambor    >= iDesde
                                AND tambores_industria.id_tambor    <= iHasta
                              NO-LOCK.

    RUN etqLabelPulpa (cZebra, 
                       cPlanta, 
                       cRnpa, 
                       cArt, 
                       tambores_industria.fecha, 
                       tambores_industria.id_lote,
                       tambores_industria.anio - 2000, 
                       dBrix, 
                       tambores_industria.id_tambor, 
                       iTbs, 
                       dAcid,
                       dGall, 
                       "Yellow", 
                       tambores_industria.kilos_tambor + dTara, 
                       tambores_industria.kilos_tambor, 
                       dTara, 
                       0.00, 
                       cCod,
                       pcTem, 
                       TRUE).

    RUN etqLabelPulpa (cZebra, 
                       cPlanta, 
                       cRnpa, 
                       cArt, 
                       tambores_industria.fecha, 
                       tambores_industria.id_lote,
                       tambores_industria.anio - 2000, 
                       dBrix, 
                       tambores_industria.id_tambor, 
                       iTbs, 
                       dAcid,
                       dGall, 
                       "Yellow", 
                       tambores_industria.kilos_tambor + dTara, 
                       tambores_industria.kilos_tambor, 
                       dTara, 
                       0.00, 
                       cCod,
                       pcTem).
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqRangoJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqRangoJugo Procedure 
PROCEDURE etqRangoJugo :
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
  DEFINE INPUT  PARAMETER pcAdd AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plHea AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cPlanta AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cZebra  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnal   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRnpa   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQtys   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTbs    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrixCo AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcGpl  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGall   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTara   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPulp   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  IF piSuc = 96 THEN 
    cZebra = "c:\lpt1".  
  ELSE
    cZebra = "\\192.168.2.11\zebra".

  /*zebra facundoj*/
  /*cZebra = "\\facundoj\ibm4400".*/
    
  

  ASSIGN cAnal   = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
         dBrix   = DECIMAL(ENTRY(3, cAnal, CHR(1))).
         dBrixCo = DECIMAL(ENTRY(4, cAnal, CHR(1))).
         dAcid   = DECIMAL(ENTRY(2, cAnal, CHR(1))).
         dAcGpl  = DECIMAL(ENTRY(1, cAnal, CHR(1))).
         dPulp   = DECIMAL(ENTRY(8, cAnal, CHR(1))).
         cQtys   = DYNAMIC-FUNCTION('getCantidadesLote' IN hLib, piEmp, piSuc, piTip, piNro).
         iTbs    = INTEGER(ENTRY(1, cQtys, CHR(1))).

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro
                                AND tambores_industria.id_tambor    >= piDes
                                AND tambores_industria.id_tambor    <= piHas
                              NO-LOCK .
    IF tambores_industria.id_sucursal = 95 THEN
      cPlanta = "Ruta Provincial 301Km33-Famailla-Tucuman".
    ELSE
      cPlanta = "Industrial Plant N 1-Lavalle 4001-Tel: (0381) 4512600-Fax: (0381) 4512612".

    ASSIGN cRnpa = DYNAMIC-FUNCTION('getRNPA' IN hLib, tambores_industria.id_articulo, tambores_industria.id_calidad)
           cArt  = DYNAMIC-FUNCTION('getDescArticuloIngles' IN hLib, tambores_industria.id_articulo)
           dGall = DYNAMIC-FUNCTION('getGalonesTambor' IN hLib, dBrixCo, tambores_industria.kilos_tambor)
           dTara = DYNAMIC-FUNCTION('getTaraEnvase' IN hLib, tambores_industria.id_envase)
           .

    IF tambores_industria.id_articulo = 52 THEN
      cArt = "FROZEN CONCENTRATED CLOUDY LEMON JUICE".

    IF tambores_industria.id_articulo = 53 THEN
      cArt = "FROZEN CONCENTRATED CLEAR LEMON JUICE".

    cCod = DYNAMIC-FUNCTION('getComposeNroLote' IN hLibTam, tambores_industria.id_sucursal,
                                                            tambores_industria.id_articulo,
                                                            tambores_industria.id_lote,
                                                            tambores_industria.anio).

    cCod = ENTRY(3, cCod, ".") + "." +
           ENTRY(4, cCod, ".").

    /*cCod = "". eliminar esta linea cuando se libere la codificacion nueva de lotes*/

    
    RUN etqLabelJugo (cZebra, 
                      cPlanta, 
                      cRnpa, 
                      cArt, 
                      tambores_industria.fecha, 
                      tambores_industria.id_lote,
                      tambores_industria.anio - 2000, 
                      dBrix, 
                      tambores_industria.id_tambor, 
                      iTbs, 
                      dAcid,
                      dGall, 
                      dBrixCo, 
                      tambores_industria.kilos_tambor + dTara, 
                      tambores_industria.kilos_tambor, 
                      dTara, 
                      dAcGpl, 
                      dPulp, 
                      cCod, 
                      pcAdd, 
                      plHea).


  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqRangoPulpa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqRangoPulpa Procedure 
PROCEDURE etqRangoPulpa :
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
  DEFINE INPUT  PARAMETER pcTem AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cPlanta AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cZebra  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnal   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRnpa   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQtys   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRatio  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTbs    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrixCo AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGall   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTara   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cCod    AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  IF piSuc = 96 THEN 
    cZebra = "c:\lpt1".  
  ELSE
    cZebra = "\\192.168.2.11\zebra".
  
  /*cZebra = "\\192.168.1.104\ibm4400".*/

  ASSIGN cAnal   = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, piEmp, piSuc, piTip, piNro).
         dBrixCo = DECIMAL(ENTRY(4, cAnal, CHR(1))).
         dBrix   = DECIMAL(ENTRY(3, cAnal, CHR(1))).
         dAcid   = DECIMAL(ENTRY(2, cAnal, CHR(1))).
         cQtys   = DYNAMIC-FUNCTION('getCantidadesLote' IN hLib, piEmp, piSuc, piTip, piNro).
         iTbs    = INTEGER(ENTRY(1, cQtys, CHR(1))).
         cRatio  = ENTRY(7, cAnal, CHR(1)).

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = piEmp
                                AND tambores_industria.id_sucursal   = piSuc
                                AND tambores_industria.id_tipotambor = piTip
                                AND tambores_industria.nromov        = piNro
                                AND tambores_industria.id_tambor    >= piDes
                                AND tambores_industria.id_tambor    <= piHas
                              NO-LOCK.
    IF tambores_industria.id_sucursal = 95 THEN
      cPlanta = "Ruta Provincial 301Km33-Famailla-Tucuman".
    ELSE
      cPlanta = "Industrial Plant Nr 1-Lavalle 4001-Tel: (0381) 4512600-Fax: (0381) 4512612".

    ASSIGN cRnpa = DYNAMIC-FUNCTION('getRNPA' IN hLib, tambores_industria.id_articulo, tambores_industria.id_calidad)
           cArt  = DYNAMIC-FUNCTION('getDescArticuloIngles' IN hLib, tambores_industria.id_articulo)
           dGall = DYNAMIC-FUNCTION('getGalonesTambor' IN hLib, dBrixCo, tambores_industria.kilos_tambor)
           dTara = DYNAMIC-FUNCTION('getTaraEnvase' IN hLib, tambores_industria.id_envase).

    cCod = DYNAMIC-FUNCTION('getComposeNroLote' IN hLibTam, tambores_industria.id_sucursal,
                                                            tambores_industria.id_articulo,
                                                            tambores_industria.id_lote,
                                                            tambores_industria.anio).

    cCod = ENTRY(3, cCod, ".") + "." +
           ENTRY(4, cCod, ".").

    /*cCod = "". eliminar esta linea cuando se libere la codificacion nueva de lotes*/


    RUN etqLabelPulpa (cZebra, 
                       cPlanta, 
                       cRnpa, 
                       cArt, 
                       tambores_industria.fecha, 
                       tambores_industria.id_lote,
                       tambores_industria.anio - 2000, 
                       dBrix, 
                       dBrixCo,
                       tambores_industria.id_tambor, 
                       iTbs, 
                       dAcid,
                       dGall, 
                       cRatio, 
                       tambores_industria.kilos_tambor + dTara, 
                       tambores_industria.kilos_tambor, 
                       dTara, 
                       0.00, 
                       cCod, 
                       pcTem,
                       TRUE).
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqSobranteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqSobranteJugo Procedure 
PROCEDURE etqSobranteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.


  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN  
    RUN zebra_lotes_jugo_sobrante.p (ROWID(lotes_jugo)).
  ELSE 
    MESSAGE "No se encontro el lote de jugo" VIEW-AS ALERT-BOX INFO  BUTTONS OK TITLE "libImpresionEtiquetas.etqSobranteJugo".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-etqTamboresLoteJugo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE etqTamboresLoteJugo Procedure 
PROCEDURE etqTamboresLoteJugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.


  

  define var i as integer.
define var total as integer.
define var f as integer.
define buffer comp for composicion_lote.

find FIRST composicion_lote where composicion_lote.id_empresa    = piEmpresa
                              AND composicion_lote.id_sucursal   = piSucursal
                              AND composicion_lote.id_tipotambor = piTipoTambor
                              AND composicion_lote.nromov        = piNroMov
                            no-lock no-error.
if available composicion_lote then
 do:
    do i = composicion_lote.numeracion_desde to  composicion_lote.numeracion_desde + 1: /*composicion_lote.numeracion_hasta:*/
        total = 0.        
        
        if composicion_lote.id_sucursal = 96 then 
            output to c:\lpt1. 
        else
            output to \\192.168.2.11\zebra. 
        
        
        /*zebra facundoj*/
        /*OUTPUT TO \\facundoj\ibm4400.*/
        
        
    
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
                
        put control "^FO400,50^A0R,300,280^FD" string(composicion_lote.id_lote,">999")  + "/" + SUBSTRING(STRING(composicion_lote.fecha),7,2,"CHARACTER") "^FS".
            for each comp where comp.id_empresa = composicion_lote.id_empresa and
                                comp.id_sucursal = composicion_lote.id_sucursal and
                                comp.id_tipotambor = composicion_lote.id_tipotambor AND
                                comp.nromov = composicion_lote.nromov no-lock.
                total = total + comp.cantidad_tambores.
            end.
        
        find tambores_industria where tambores_industria.id_empresa = composicion_lote.id_empresa and
                                      tambores_industria.id_sucursal = composicion_lote.id_sucursal and
                                      tambores_industria.id_tipotambor = composicion_lote.id_tipotambor and
                                      tambores_industria.nromov = composicion_lote.nromov and
                                      tambores_industria.id_tambor = i no-lock no-error.
        
        if available tambores_industria then
         do:
            put control "^FO725,735^A0R,72,42^FD" substr(string(composicion_lote.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
         end.
        else
         do:
            put control "^FO725,850^A0R,72,42^FD" "ERROR" "^FS".
         end.
        
        if total > 99 then
            put control "^FO24,100^A0R,300,250^FD" string(i,"999") + "/" + string(total,"999") "^FS".
        else
            put control "^FO24,100^A0R,300,320^FD" string(i,"99") + "/" + string(total,"99") "^FS".

        put control "^FO150,970^A0N,^BY3^B3N,N,230,N,N^FD" "03" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        put control "^FO10,965^GB780,0,3^FS".
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
        
        if composicion_lote.id_sucursal = 96 then
          do:  
            f = 1.
            do f = 1 to 30000.
            
            end.    
               
          end.
        else
          do:
            f = 1.
            do f = 1 to 5000.
            
            end.               
            
           end.
          
   end.
end.







END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-zebraBarCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraBarCode Procedure 
FUNCTION zebraBarCode RETURNS CHARACTER
  (pcL AS CHARACTER, 
   pcM AS CHARACTER,
   piX AS INTEGER, 
   piY AS INTEGER,    
   pcO AS CHARACTER,        /*orientacion N Horizontal R Vertical*/       
   piH AS INTEGER,          /*alto de las barras*/
   piW AS INTEGER,          /*ancho del codigo barra*/
   pcN AS CHARACTER,        /*imprime el nro del codigo Y/N*/
   pcU AS CHARACTER,        /*imprime el nro arriba del codigo*/
   pcData AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "^FO" + 
         STRING(piX) + 
         "," + 
         STRING(piY) + 
         "^A" + pcL + pcM + 
         "," + 
         "^B" + STRING(piW) + pcO + ",N," + STRING(piH) + "," + pcN + "," + pcU + 
         "^FD" + 
         pcData + 
         "^FS".

  PUT CONTROL cRet.
  /*PUT CONTROL "^FO50,225^A0R,^BY3^B3R,N,200,R,R^FD" + pcData +  "^FS" .*/
  RETURN cRet.

END FUNCTION.


/*

cRet = "^FO" + 
         STRING(piX) + 
         "," + 
         STRING(piY) + 
         "^A" + pcL + pcO + 
         "," + 
         STRING(piH) + 
         "," + 
         STRING(piW) + 
         "^FD" + 
         pcData + 
         "^FS".
         
         */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraBoxLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraBoxLine Procedure 
FUNCTION zebraBoxLine RETURNS CHARACTER
  (pcX1 AS INTEGER, 
   pcY1 AS INTEGER, 
   pcX2 AS INTEGER, 
   pcY2 AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  /*"^FO25,20^GB785,1200,3^FS".*/
  

  cRet = "^FO" + 
         STRING(pcX1) + 
         "," + 
         STRING(pcY1) + 
         "^GB" + 
         STRING(pcX2) + 
         "," + 
         STRING(pcY2) + 
         ",3" + 
         "^FS".


  PUT CONTROL cRet.




  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraClose Procedure 
FUNCTION zebraClose RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "^XZ".

  PUT CONTROL cRet.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraFieldData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraFieldData Procedure 
FUNCTION zebraFieldData RETURNS CHARACTER
  (pcL AS CHARACTER, 
   pc0 AS CHARACTER,
   piH AS INTEGER,
   piW AS INTEGER, 
   piX AS INTEGER, 
   piY AS INTEGER,    
   pcData AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "^FO" + 
         STRING(piX) + 
         "," + 
         STRING(piY) + 
         "^A" + pcL + pc0 + 
         "," + 
         STRING(piH) + 
         "," + 
         STRING(piW) + 
         "^FD" + 
         pcData + 
         "^FS".

  PUT CONTROL cRet.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraInitialize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraInitialize Procedure 
FUNCTION zebraInitialize RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "^XA" + 
         "^LH0,0" + 
         "^PQ1^FS" +
         "^FO1,1^XGR:Logo.grf,1,1^FS" + 
         "^PR6,6,6^FS".

  PUT CONTROL cRet.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraLabelFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraLabelFooter Procedure 
FUNCTION zebraLabelFooter RETURNS CHARACTER
  (pcGrados AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  /*footer*/
  zebraFieldData("0", "", 35, 25, 340, 1010, "IMPORTANT").
  zebraFieldData("0", "", 30, 19, 30, 1060, "KEEP THIS PRODUCT REFRIGERATED, AT " + pcGrados + " C. IF THESE TEMPERATURES ARE"). 
  zebraFieldData("0", "", 30, 25, 40, 1077, ","). 
  zebraFieldData("0", "", 30, 20, 30, 1090,  " NOT KEPT, S.A. SAN MIGUEL BE COMES NOT RESPONSIBLE FOR THIS PRODUCT"). 
  zebraFieldData("0", "", 35, 25, 60, 1150, "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM"). 



  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraLabelHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraLabelHeader Procedure 
FUNCTION zebraLabelHeader RETURNS CHARACTER
  (pcPlanta AS CHARACTER, 
   pcRNPA   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  /*header*/

  zebraPrintGraphic("", "..\industria\lgsmhor.grf", 8, 75, 1, 1).
  zebraFieldData("0", "", 18, 18, 82, 120, "A.G.I.C.I.C.y F.").

  zebraFieldData("0", "", 22, 22, 420, 80, "RNE: 23001973 - RNPA: " + pcRNPA).

  zebraFieldData("0", "", 24, 17, 8, 150, "Industrial Plant:" + pcPlanta + "ARGENTINA(4132)Tel:54-3863461427").
                                             
  zebraBoxLine(5, 170, 750, 1).
  
                                            
  RETURN "OK".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zebraPrintGraphic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zebraPrintGraphic Procedure 
FUNCTION zebraPrintGraphic RETURNS CHARACTER
  (pcZebra AS CHARACTER,
   pcGRF   AS CHARACTER, 
   piX     AS INTEGER, 
   piY     AS INTEGER, 
   piH     AS INTEGER,
   piW     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFile   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGRF    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  cGRF = DYNAMIC-FUNCTION('getFileNameFromPath' IN hLibCom, pcGRF).

  /*elimina grafico de impresora*/
  PUT CONTROL "^IDR:" + UPPER(cGRF).

  /*carga grafico en impresora*/
  INPUT FROM VALUE(pcGRF).
  REPEAT:
    IMPORT UNFORMATTED cAux .
    cFile = cFile + cAux.
  END.
  INPUT CLOSE.

  PUT CONTROL cFile. 

  /*** Imprime Grafico ***/
  PUT CONTROL "^FO" + 
              STRING(piX) + "," + 
              STRING(piY) + 
              "^XGR:" + 
              UPPER(cGRF) + ","  +
              STRING(piH) + "," + 
              STRING(piW) + "^FS".
  
  DELETE OBJECT hLibCom.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
