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

DEFINE VARIABLE lResult AS LOGICAL    NO-UNDO.

DEFINE TEMP-TABLE tt NO-UNDO /*dummy TT*/
  FIELD f1 AS INTEGER.

DEFINE TEMP-TABLE ttReglas
    FIELD nro               AS CHARACTER COLUMN-LABEL "Nro Regla"
    FIELD regla             AS CHARACTER COLUMN-LABEL "Regla"
    FIELD valor             AS CHARACTER COLUMN-LABEL "Valor"
    FIELD antecedente       AS CHARACTER COLUMN-LABEL "Antecedente"
    FIELD consecuente       AS CHARACTER COLUMN-LABEL "Consecuente"  
    FIELD id_orden_entrega  AS INTEGER COLUMN-LABEL "OE"
    FIELD id_gasto          AS INTEGER COLUMN-LABEL "idGasto".



DEFINE QUERY q FOR tt. /* dummy query, but required */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-calcAgp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcAgp Procedure 
FUNCTION calcAgp RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcAmc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcAmc Procedure 
FUNCTION calcAmc RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcBL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcBL Procedure 
FUNCTION calcBL RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcBunker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcBunker Procedure 
FUNCTION calcBunker RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcCustomBrokerFee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcCustomBrokerFee Procedure 
FUNCTION calcCustomBrokerFee RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDcr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcDcr Procedure 
FUNCTION calcDcr RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDelivery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcDelivery Procedure 
FUNCTION calcDelivery RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDestinationSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcDestinationSecurity Procedure 
FUNCTION calcDestinationSecurity RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEbaf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcEbaf Procedure 
FUNCTION calcEbaf RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcEntry Procedure 
FUNCTION calcEntry RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntryEuropa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcEntryEuropa Procedure 
FUNCTION calcEntryEuropa RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntryUsa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcEntryUsa Procedure  _DB-REQUIRED
FUNCTION calcEntryUsa RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcFcaCosts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcFcaCosts Procedure 
FUNCTION calcFcaCosts RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcFlete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcFlete Procedure 
FUNCTION calcFlete RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcGateOut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcGateOut Procedure 
FUNCTION calcGateOut RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcIks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcIks Procedure 
FUNCTION calcIks RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcInbalance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcInbalance Procedure 
FUNCTION calcInbalance RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcInland) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcInland Procedure 
FUNCTION calcInland RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcPalletizing) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcPalletizing Procedure 
FUNCTION calcPalletizing RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcPanamaToll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcPanamaToll Procedure 
FUNCTION calcPanamaToll RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcReproceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcReproceso Procedure 
FUNCTION calcReproceso RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcSecurityCharge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcSecurityCharge Procedure 
FUNCTION calcSecurityCharge RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcSeguro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcSeguro Procedure 
FUNCTION calcSeguro RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcServiceFee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcServiceFee Procedure 
FUNCTION calcServiceFee RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcThcDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcThcDestino Procedure 
FUNCTION calcThcDestino RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcThcOrigen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcThcOrigen Procedure 
FUNCTION calcThcOrigen RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcToll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcToll Procedure 
FUNCTION calcToll RETURNS DECIMAL
  (ipcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcVarios) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcVarios Procedure 
FUNCTION calcVarios RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcWareHouse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcWareHouse Procedure 
FUNCTION calcWareHouse RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcWharfage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcWharfage Procedure 
FUNCTION calcWharfage RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcZero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcZero Procedure 
FUNCTION calcZero RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_aceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_aceite Procedure 
FUNCTION es_aceite RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_canada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_canada Procedure 
FUNCTION es_canada RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cfr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_cfr Procedure 
FUNCTION es_cfr RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cif) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_cif Procedure 
FUNCTION es_cif RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cliente_iks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_cliente_iks Procedure 
FUNCTION es_cliente_iks RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_ddp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_ddp Procedure 
FUNCTION es_ddp RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_ddu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_ddu Procedure 
FUNCTION es_ddu RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_eddp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_eddp Procedure 
FUNCTION es_eddp RETURNS LOGICAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_europa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_europa Procedure 
FUNCTION es_europa RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_fca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_fca Procedure 
FUNCTION es_fca RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_fob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_fob Procedure 
FUNCTION es_fob RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_maersk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_maersk Procedure 
FUNCTION es_maersk RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_miami) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_miami Procedure 
FUNCTION es_miami RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_usa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD es_usa Procedure 
FUNCTION es_usa RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateExpression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD evaluateExpression Procedure 
FUNCTION evaluateExpression RETURNS LOGICAL
  (pcRuleAntecedent AS CHARACTER, 
   pcArgs           AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEntryttReglas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEntryttReglas Procedure 
FUNCTION getEntryttReglas RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGastoContrato Procedure 
FUNCTION getGastoContrato RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLogical) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLogical Procedure 
FUNCTION getLogical RETURNS LOGICAL
  (plValue AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareExpression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD prepareExpression Procedure 
FUNCTION prepareExpression RETURNS CHARACTER
  (pcExp AS CHARACTER, 
   pcArg AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-siempre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD siempre Procedure 
FUNCTION siempre RETURNS LOGICAL
  (pcArgs AS CHARACTER)  FORWARD.

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

&IF DEFINED(EXCLUDE-motorInferencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE motorInferencia Procedure 
PROCEDURE motorInferencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcArchivoReglas AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcArgs          AS CHARACTER NO-UNDO.
  
  
  DEFINE VARIABLE cReg AS CHARACTER FORMAT "X(50)" NO-UNDO.
  DEFINE VARIABLE cAnt AS CHARACTER FORMAT "X(50)" NO-UNDO.
  DEFINE VARIABLE cCon AS CHARACTER FORMAT "X(50)" NO-UNDO.
  DEFINE VARIABLE cRun AS CHARACTER FORMAT "X(50)" NO-UNDO.
  DEFINE VARIABLE cNro AS CHARACTER FORMAT "X(3)"  NO-UNDO.
  DEFINE VARIABLE cNom AS CHARACTER FORMAT "X(30)" NO-UNDO.  
  DEFINE VARIABLE cRul AS CHARACTER FORMAT "X(70)" NO-UNDO.
  DEFINE VARIABLE dVal AS DECIMAL    NO-UNDO.
  
  
  DEFINE VARIABLE iPos AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFun AS LOGICAL    NO-UNDO.
  
  DEFINE VARIABLE dCon AS DECIMAL    NO-UNDO.
  
  FOR EACH ttReglas. 
    DELETE ttReglas.
  END.
  
  INPUT FROM VALUE(pcArchivoReglas).
  REPEAT :
    IMPORT UNFORMATTED cReg.
    cReg = TRIM(cReg).
    IF LENGTH(cReg) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    iPos = INDEX(cReg, "#").
    IF iPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.
    
  
    /*extraigo nro*/
    cNro = ENTRY(1, cReg, "-").
    cReg = ENTRY(2, cReg, "-").
    /*extraigo nombre*/
    cNom = ENTRY(1, cReg, ";").
    cReg = ENTRY(2, cReg, ";").
    /*extraigo antecedente y consecuente*/
    cAnt = TRIM(ENTRY(1, cReg, ">")).
    cCon = TRIM(ENTRY(2, cReg, ">")).

 
    /*opero el antecedente*/
    lFun = evaluateExpression(cAnt, pcArgs).

    /*ejecutrar consecuente si antecedente es verdadero*/
    IF lFun AND LENGTH(cCon) > 0 THEN DO: 
      dVal = DYNAMIC-FUNCTION(cCon IN THIS-PROCEDURE, pcArgs) .

      /*DISP cNro cNom vdCon.*/
      CREATE ttReglas.
      ASSIGN ttReglas.nro              = cNro
             ttReglas.regla            = cNom
             ttReglas.valor            = STRING(dVal, ">>>,>>>,>99.999")
             ttReglas.antecedente      = cAnt
             ttReglas.consecuente      = cCon
             ttReglas.id_orden_entrega = INTEGER(pcArgs)
             ttReglas.id_gasto         = INTEGER(cNro).
      
    END.

  END.
    
  INPUT CLOSE.

/*  
  RUN generateExcel.p (INPUT TABLE ttReglas,
                       INPUT " Reglas de Inferencia ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).
                       
*/                       
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-calcAgp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcAgp Procedure 
FUNCTION calcAgp RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dkil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
  

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).  
  dKil = DYNAMIC-FUNCTION('getKilosOE' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 29).
  dRet = dGas * (dKil / 1000). 

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcAmc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcAmc Procedure 
FUNCTION calcAmc RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dRet = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 32).

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcBL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcBL Procedure 
FUNCTION calcBL RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
  
  
  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 19).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  
  IF dGas <> 0 THEN
    dRet = dGas * dCnt.
  ELSE 
    dRet = 0 * dCnt. /*propuesto por magali 25/10/2005*/
                     /*propuesto por facundo y esteban 03/05/06*/

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcBunker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcBunker Procedure 
FUNCTION calcBunker RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
  
  
  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 17).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  
  IF dGas <> 0 THEN
    dRet = dGas * dCnt.
  
  /*ELSE 
    dRet = 550 * dCnt.*/ /*propuesto por magali 06/07/2006 */



  /*esta es una tabla de excepcciones*/
  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                          NO-LOCK.
    FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = orden_entrega.id_orden_entrega
                                 NO-LOCK.      
      FIND FIRST r_agencias_contenedores WHERE r_agencias_contenedores.id_agencia         = orden_entrega.id_agencia
                                           AND r_agencias_contenedores.id_tipo_contenedor = items_orden_entrega.id_tipo_contenedor
                                         NO-LOCK NO-ERROR.
      IF AVAILABLE r_agencias_contenedores THEN DO:
        dRet = dRet + r_agencias_contenedores.importe * items_orden_entrega.contenedores.
      END.    
    END.
  END.
  

  RETURN dRet.

END FUNCTION.


/*


  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                          NO-LOCK.
    FIND FIRST r_agencias_contenedores OF orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE r_agencias_contenedores THEN DO:
      dRet = r_agencias_contenedores.importe * orden_entrega.contenedores.
    END.
    ELSE DO:
      FIND agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
      IF AVAILABLE agencias THEN DO:
          MESSAGE "Falta cargar la tarifa del Bunker de los contenedores para la Agencia " 
                  agencias.descripcion VIEW-AS ALERT-BOX.
      END.
    END.
  END.


                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcCustomBrokerFee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcCustomBrokerFee Procedure 
FUNCTION calcCustomBrokerFee RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",33").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDcr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcDcr Procedure 
FUNCTION calcDcr RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTax AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGal AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
  

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                               NO-LOCK.
    FOR FIRST items_contratos OF items_orden_entrega NO-LOCK.
      FOR EACH r_gastos_items_contrato OF items_contratos 
                                        WHERE r_gastos_items_contrato.id_gasto = 6 
                                           OR r_gastos_items_contrato.id_gasto = 7
                                           OR r_gastos_items_contrato.id_gasto = 23
                                           OR r_gastos_items_contrato.id_gasto = 24
                                           OR r_gastos_items_contrato.id_gasto = 28
                                         NO-LOCK.
        dTax = dTax + r_gastos_items_contrato.importe.
      END.
    END.    
  END.

  dGal = DYNAMIC-FUNCTION('getGalonesOE' IN hLib, INTEGER(pcArgs)).
  dRet = dGal * dTax.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDelivery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcDelivery Procedure 
FUNCTION calcDelivery RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",6").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDestinationSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcDestinationSecurity Procedure 
FUNCTION calcDestinationSecurity RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",38").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEbaf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcEbaf Procedure 
FUNCTION calcEbaf RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).  
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 31).
  dRet = dGas * dCnt. 

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcEntry Procedure 
FUNCTION calcEntry RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  se calcula solo para jugos y es el 10.9 % del costo y flete.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fTot    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fCyF    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fEnt    AS DECIMAL    NO-UNDO.

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs) NO-LOCK.
    FOR EACH tambores_industria OF items_orden_entrega
                                BREAK BY tambores_industria.nromov.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        /*para jugos*/
        IF tambores_industria.id_articulo = 52 OR tambores_industria.id_articulo = 53 THEN DO:
          fCyF = calcFlete(pcArgs). /*agregar aqui los gastos que faltan para completar un cfr*/
          fEnt = fCyF * 0.109.
          dRet = dRet + fEnt.           
        END.
      END.      
    END.       
  END.

RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntryEuropa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcEntryEuropa Procedure 
FUNCTION calcEntryEuropa RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  se calcula solo para jugos y es el 10.9 % del costo y flete.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fTot    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fCyF    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fEnt    AS DECIMAL    NO-UNDO.

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs) NO-LOCK.
    FOR EACH tambores_industria OF items_orden_entrega
                                BREAK BY tambores_industria.nromov.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        /*para jugos*/
        IF tambores_industria.id_articulo = 52 OR tambores_industria.id_articulo = 53 THEN DO:
          fCyF = calcFlete(pcArgs).
          fEnt = fCyF * 10.9.
          dRet = dRet + fEnt.           
        END.
      END.      
    END.       
  END.

RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEntryUsa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcEntryUsa Procedure 
FUNCTION calcEntryUsa RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  parece ser un calculo en funcion de los grados brix del lote para jugos
            para aceites es un 3.8 % del total de la factura.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrx    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fTot    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAnl    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibOE  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

  /*primero busca en el contrato*/
  dRet = getGastoContrato(pcArgs + ",34").
  IF dRet > 0 THEN
    RETURN dRet.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibOE  = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  hLibRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').

  DELETE OBJECT hLibCom.

  fTot = DYNAMIC-FUNCTION('getTotalFacturaOE' IN hLibOE, INTEGER(pcArgs)).


  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs) NO-LOCK.
    FOR EACH tambores_industria OF items_orden_entrega
                                BREAK BY tambores_industria.nromov.
      dKil = dKil + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        /*para jugos*/
        IF tambores_industria.id_articulo = 52 OR tambores_industria.id_articulo = 53 THEN DO:
          cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLibRep, tambores_industria.id_empresa,
                                                                   tambores_industria.id_sucursal,
                                                                   tambores_industria.id_tipotambor,
                                                                   tambores_industria.nromov).
          IF cAnl <> "" AND cAnl <> ? THEN DO:

            dBrx = DECIMAL(ENTRY(4, cAnl, CHR(1))).
            FIND FIRST tabla_entry WHERE tabla_entry.Brix_desde <= dBrx
                                     AND tabla_entry.Brix_hasta >= dBrx
                                     AND tabla_entry.id_articulo = tambores_industria.id_articulo
                                   NO-LOCK NO-ERROR.
            IF AVAILABLE tabla_entry THEN DO:
              dLit = dKil / DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLibTam, dBrx).
              dRet = dRet + (dLit * tabla_entry.grados * 0.079).
            END.
          END.            
        END.

        /*para aceites*/
        IF tambores_industria.id_articulo = 51 OR tambores_industria.id_articulo = 57 THEN DO:
          dRet = dRet + ((fTot * items_orden_entrega.contenedores) * 0.038).
        END.

        /*para water phase*/
        IF tambores_industria.id_articulo = 58 THEN DO:
          /*esto es entry = (total factura - gastos por clausula) * 0.00333881*/

        END.


        dKil = 0.
      END.
      
    END.

        
  END.

RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcFcaCosts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcFcaCosts Procedure 
FUNCTION calcFcaCosts RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",22").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcFlete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcFlete Procedure 
FUNCTION calcFlete RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.


  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).  
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 5).
  dRet = dGas * dCnt. 

  IF iAge = 176 THEN
    RETURN dRet.

  FIND LAST gastos_flete 
       WHERE gastos_flete.id_agencia = iAge
       NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_flete THEN
    dRet = dCnt * gastos_flete.importe.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcGateOut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcGateOut Procedure 
FUNCTION calcGateOut RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.



  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  dRet = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 15) * dCnt.


  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcIks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcIks Procedure 
FUNCTION calcIks RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.



  dTot = DYNAMIC-FUNCTION('getTotalFacturaOE' IN hLib, INTEGER(pcArgs)).
  dRet = dTot * 0.0175.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcInbalance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcInbalance Procedure 
FUNCTION calcInbalance RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  
  FOR LAST gastos_flete WHERE gastos_flete.id_agencia = iAge
                        NO-LOCK.
    dRet = gastos_flete.inbalance_surcharge * dCnt.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcInland) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcInland Procedure 
FUNCTION calcInland RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  dRet = 60 * dCnt.
  
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcPalletizing) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcPalletizing Procedure 
FUNCTION calcPalletizing RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",24").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcPanamaToll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcPanamaToll Procedure 
FUNCTION calcPanamaToll RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",39").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcReproceso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcReproceso Procedure 
FUNCTION calcReproceso RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",7").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcSecurityCharge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcSecurityCharge Procedure 
FUNCTION calcSecurityCharge RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",37").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcSeguro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcSeguro Procedure 
FUNCTION calcSeguro RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  dTot = DYNAMIC-FUNCTION('getTotalFacturaOE' IN hLib, INTEGER(pcArgs)).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).

  FIND LAST gastos_seguros NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_seguros THEN DO:
    dRet = ((gastos_seguros.importe * dTot) * dCnt ) / 100000.
  END.
  
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcServiceFee) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcServiceFee Procedure 
FUNCTION calcServiceFee RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",35").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcThcDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcThcDestino Procedure 
FUNCTION calcThcDestino RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 20).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(pcArgs)).
  
  IF dGas <> 0 THEN DO:
    dRet = dGas * dCnt.
  END.
  /* propuesto por magali 06/07/2006 sii no  esta en la tarifa que no calcule nada
  ELSE DO:
    FOR EACH items_orden_entrega 
        WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
        NO-LOCK.
      FOR FIRST tipo_contenedor 
          OF items_orden_entrega 
          NO-LOCK.
        dRet = dRet + (tipo_contenedor.precio_thc * items_orden_entrega.contenedores).
      END.      
    END.
  END.
  */
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcThcOrigen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcThcOrigen Procedure 
FUNCTION calcThcOrigen RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(pcArgs)).
  dGas = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 9).

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                               NO-LOCK.
    IF dGas > 0 THEN DO:
      dRet = dRet + (dGas * items_orden_entrega.contenedores).
    END.
    ELSE DO:
      FIND FIRST tipo_contenedor OF items_orden_entrega NO-LOCK NO-ERROR.
      IF AVAILABLE tipo_contenedor THEN DO:
        dRet = dRet + (tipo_contenedor.precio_thc * items_orden_entrega.contenedores).
      END.
    END.    
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcToll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcToll Procedure 
FUNCTION calcToll RETURNS DECIMAL
  (ipcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iAge AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  iAge = DYNAMIC-FUNCTION('getAgenciaId' IN hLib, INTEGER(ipcArgs)).
  dCnt = DYNAMIC-FUNCTION('getContenedoresOE' IN hLib, INTEGER(ipcArgs)).
  dRet = DYNAMIC-FUNCTION('getGastoAgencia' IN hLib, iAge, 14) * dCnt.


  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcVarios) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcVarios Procedure 
FUNCTION calcVarios RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",12").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcWareHouse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcWareHouse Procedure 
FUNCTION calcWareHouse RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",28").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcWharfage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcWharfage Procedure 
FUNCTION calcWharfage RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  fRet = getGastoContrato(pcArgs + ",41").

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcZero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcZero Procedure 
FUNCTION calcZero RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_aceite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_aceite Procedure 
FUNCTION es_aceite RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST tambores_industria
      WHERE tambores_industria.id_tipotambor = 6 .
    lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_canada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_canada Procedure 
FUNCTION es_canada RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                            AND (orden_entrega.id_destino = 484)
                          NO-LOCK .
    lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cfr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_cfr Procedure 
FUNCTION es_cfr RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                                NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 2 THEN
      lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cif) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_cif Procedure 
FUNCTION es_cif RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                                NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 3 THEN
      lRet = TRUE.
  END.

  RETURN lRet.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_cliente_iks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_cliente_iks Procedure 
FUNCTION es_cliente_iks RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FOR FIRST orden_entrega 
      WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs) NO-LOCK, 
      FIRST contratos 
         OF orden_entrega
      WHERE contratos.id_cliente = 100073 /*es al unico cliente que le corresponde iks*/
      NO-LOCK.

    lRet = TRUE.
  END.

  

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_ddp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_ddp Procedure 
FUNCTION es_ddp RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve verdadero cuando todos los items de la oe son ddp
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = TRUE.
  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                               NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 12  OR /*DDP*/
       items_orden_entrega.id_condicion_venta = 112 OR /*DDP CANADA*/
       items_orden_entrega.id_condicion_venta = 113 OR /*DDP MIAMI*/
       items_orden_entrega.id_condicion_venta = 114 OR /*DDP EUROPA*/
       items_orden_entrega.id_condicion_venta = 115 THEN
      lRet = lRet AND TRUE.
    ELSE 
      lRet = FALSE.
  END.

  RETURN lRet.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_ddu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_ddu Procedure 
FUNCTION es_ddu RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                                NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 11 THEN
      lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_eddp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_eddp Procedure 
FUNCTION es_eddp RETURNS LOGICAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = TRUE.
  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                               NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 14  THEN /*EDDP*/
      lRet = lRet AND TRUE.
    ELSE 
      lRet = FALSE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_europa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_europa Procedure 
FUNCTION es_europa RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                           NO-LOCK NO-ERROR.
  IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST destinos OF orden_entrega 
                        WHERE (destinos.id_destino_grupo = 29 OR
                               destinos.id_destino_grupo = 30 OR
                               destinos.id_destino_grupo = 31 OR
                               destinos.id_destino_grupo = 32 OR
                               destinos.id_destino_grupo = 33 OR
                               destinos.id_destino_grupo = 34 OR
                               destinos.id_destino_grupo = 35 OR
                               destinos.id_destino_grupo = 36 OR
                               destinos.id_destino_grupo = 37 OR
                               destinos.id_destino_grupo = 39 OR
                               destinos.id_destino_grupo = 43 OR
                               destinos.id_destino_grupo = 46 OR
                               destinos.id_destino_grupo = 47 OR
                               destinos.id_destino_grupo = 49 OR
                               destinos.id_destino_grupo = 52 OR
                               destinos.id_destino_grupo = 53 OR
                               destinos.id_destino_grupo = 55 OR
                               destinos.id_destino_grupo = 59 OR
                               destinos.id_destino_grupo = 60 OR
                               destinos.id_destino_grupo = 61 OR
                               destinos.id_destino_grupo = 62 OR
                               destinos.id_destino_grupo = 65 OR
                               destinos.id_destino_grupo = 70 OR
                               destinos.id_destino_grupo = 71 OR
                               destinos.id_destino_grupo = 78 OR
                               destinos.id_destino_grupo = 80 OR
                               destinos.id_destino_grupo = 87 OR
                               destinos.id_destino_grupo = 89)
                        NO-LOCK NO-ERROR.
    IF AVAILABLE destinos THEN DO:
      lRet = TRUE.
    END.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_fca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_fca Procedure 
FUNCTION es_fca RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                                NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 8 THEN
      lRet = TRUE.
  END.

  RETURN lRet.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_fob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_fob Procedure 
FUNCTION es_fob RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                                NO-LOCK .
    IF items_orden_entrega.id_condicion_venta = 1 THEN
      lRet = TRUE.
  END.

  RETURN lRet.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_maersk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_maersk Procedure 
FUNCTION es_maersk RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.


  FOR FIRST orden_entrega
      WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
        AND (orden_entrega.id_agencia      = 9 OR
             orden_entrega.id_agencia      = 50 OR
             orden_entrega.id_agencia      = 99 OR
             orden_entrega.id_agencia      = 140 OR
             orden_entrega.id_agencia      = 145 OR
             orden_entrega.id_agencia      = 157 OR
             orden_entrega.id_agencia      = 161 OR
             orden_entrega.id_agencia      = 163 OR
             orden_entrega.id_agencia      = 164 OR
             orden_entrega.id_agencia      = 181 OR
             orden_entrega.id_agencia      = 185 OR
             orden_entrega.id_agencia      = 186)
      NO-LOCK.
      lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_miami) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_miami Procedure 
FUNCTION es_miami RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  lRet = FALSE.
  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                            AND (orden_entrega.id_destino = 70
                             OR  orden_entrega.id_destino = 403
                             OR  orden_entrega.id_destino = 445)
                          NO-LOCK .
    lRet = TRUE.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-es_usa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION es_usa Procedure 
FUNCTION es_usa RETURNS LOGICAL
  (INPUT pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                           NO-LOCK NO-ERROR.
  IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST destinos OF orden_entrega 
                        WHERE destinos.id_destino_grupo = 25
                        NO-LOCK NO-ERROR.
    IF AVAILABLE destinos THEN DO:
      lRet = TRUE.
    END.
  END.

  RETURN lRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-evaluateExpression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION evaluateExpression Procedure 
FUNCTION evaluateExpression RETURNS LOGICAL
  (pcRuleAntecedent AS CHARACTER, 
   pcArgs           AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQry        AS CHARACTER  NO-UNDO.
  

  /*Evaluate a 4GL logical expression */
  cExpression = prepareExpression(pcRuleAntecedent, pcArgs).
  cQry        = "FOR EACH tt WHERE DYNAMIC-FUNCTION('getLogical' IN THIS-PROCEDURE, " + cExpression + ") = TRUE".
  
  QUERY q:QUERY-PREPARE(cQry).
  QUERY q:QUERY-OPEN().
  QUERY q:QUERY-CLOSE.
  

  RETURN lResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEntryttReglas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEntryttReglas Procedure 
FUNCTION getEntryttReglas RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH ttReglas.
    cRet = cRet + STRING(ttReglas.id_orden_entrega) + CHR(1) +
                  STRING(ttReglas.id_gasto)         + CHR(1) + 
                  ttReglas.regla                    + CHR(1) + 
                  STRING(ttReglas.valor)            + CHR(1) + 
                  ttReglas.nro                      + CHR(1) + 
                  ttReglas.antecedente              + CHR(1) + 
                  ttReglas.consecuente              + CHR(10).
    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGastoContrato Procedure 
FUNCTION getGastoContrato RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  
  
  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = INTEGER(ENTRY(1, pcArgs))
      NO-LOCK.

    FOR FIRST items_contratos 
        OF items_orden_entrega
        NO-LOCK.

      FOR FIRST r_gastos_items_contrato 
             OF items_contratos                              
          WHERE r_gastos_items_contrato.id_gasto = INTEGER(ENTRY(2, pcArgs))
          NO-LOCK.

        CASE items_contratos.id_tipo_unidad_venta_origen:
          WHEN 1 then /* TONELADAS */ DO:
            dRet = dRet + ((items_orden_entrega.kgs_netos_tambores / 1000) * r_gastos_items_contrato.importe).
          END.
          WHEN 2 then /* KILOS */ DO:
            dRet = dRet + (items_orden_entrega.kgs_netos_tambores * r_gastos_items_contrato.importe).
          END.
          WHEN 3 then /* GALONES */ DO:
            dRet = dRet + (DYNAMIC-FUNCTION('getGalonesItemOE' IN hLib, items_orden_entrega.id_orden_entrega, items_orden_entrega.ITEM_oe) * r_gastos_items_contrato.importe).
          END.
          WHEN 4 THEN /* LIBRAS */ DO:
            dRet = dRet + ((items_orden_entrega.kgs_netos_tambores * 2.20462) * r_gastos_items_contrato.importe).
          END.
        END CASE.

      END.

    END.

  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLogical) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLogical Procedure 
FUNCTION getLogical RETURNS LOGICAL
  (plValue AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  lResult = plValue.

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prepareExpression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION prepareExpression Procedure 
FUNCTION prepareExpression RETURNS CHARACTER
  (pcExp AS CHARACTER, 
   pcArg AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cWrd AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExp AS CHARACTER  NO-UNDO.


  cExp = pcExp.
  DO i = 1 TO NUM-ENTRIES(pcExp, " "): /*recorro la expresion separando las palabras por un espacion en blanco*/
    cWrd = TRIM(ENTRY(i, pcExp, " ")).

    /*evaluo que no sea un operador logico*/
    CASE cWrd:
      WHEN "AND" THEN.
      WHEN "OR" THEN.
      WHEN "NOT" THEN.
      OTHERWISE DO: /*calculo el valor de verdad del operando y armo una expresion del tipo TRUE AND TRUE AND FALSE ...*/
        cExp = REPLACE(cExp, cWrd, STRING(DYNAMIC-FUNCTION(cWrd IN THIS-PROCEDURE, pcArg))).
      END.
    END CASE.
  END.

  RETURN cExp.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-siempre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION siempre Procedure 
FUNCTION siempre RETURNS LOGICAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

