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

&IF DEFINED(EXCLUDE-ctrlHora) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ctrlHora Procedure 
FUNCTION ctrlHora RETURNS CHARACTER
  (pcHora AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ctrlKilosCajonera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ctrlKilosCajonera Procedure 
FUNCTION ctrlKilosCajonera RETURNS CHARACTER
  (pcKilos AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ctrlMin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ctrlMin Procedure 
FUNCTION ctrlMin RETURNS CHARACTER
  (pcMin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosSupervisor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosSupervisor Procedure 
FUNCTION getDatosSupervisor RETURNS CHARACTER
  (pcCodigo AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescripcionLinea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescripcionLinea Procedure 
FUNCTION getDescripcionLinea RETURNS CHARACTER
  (piLin AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIngresoBalanzaDia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIngresoBalanzaDia Procedure 
FUNCTION getIngresoBalanzaDia RETURNS DECIMAL
  (pdFec AS DATE,
   piLin AS INTEGER,
   piTur AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIngresoBalanzaTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIngresoBalanzaTurno Procedure 
FUNCTION getIngresoBalanzaTurno RETURNS DECIMAL
  (pdFec AS DATE,
   piLin AS INTEGER,
   piTur AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaDia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoliendaDia Procedure 
FUNCTION getMoliendaDia RETURNS DECIMAL
  (pdFec AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaDiaLinea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoliendaDiaLinea Procedure 
FUNCTION getMoliendaDiaLinea RETURNS DECIMAL
  (pdFec AS DATE, 
   piLin AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaSilos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoliendaSilos Procedure 
FUNCTION getMoliendaSilos RETURNS CHARACTER
  (pdFec AS DATE, 
   piLin AS INTEGER,
   piTur AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoliendaTurno Procedure 
FUNCTION getMoliendaTurno RETURNS DECIMAL
  (pdFec AS DATE,
   piTur AS INTEGER, 
   piLin AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPesoFromNroPesada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPesoFromNroPesada Procedure 
FUNCTION getPesoFromNroPesada RETURNS DECIMAL
  (piNroPesada AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStockSilos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStockSilos Procedure 
FUNCTION getStockSilos RETURNS DECIMAL
  (pdFec AS DATE, 
   piLin AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalesTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalesTurno Procedure 
FUNCTION getTotalesTurno RETURNS CHARACTER
  (piLin AS INTEGER,
   pdFec AS DATE,
   piTur AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDescartePacking) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDescartePacking Procedure 
FUNCTION setDescartePacking RETURNS INTEGER
  (pdFec AS DATE,
   piLin AS INTEGER,
   pcEnt AS CHARACTER,
   pcSal AS CHARACTER,
   pfKil AS DECIMAL,
   piSil AS INTEGER,
   piCal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validarSupervisorTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validarSupervisorTurno Procedure 
FUNCTION validarSupervisorTurno RETURNS LOGICAL
  (piTurno AS INTEGER,
   piSuper AS INTEGER, 
   pdFecha AS DATE)  FORWARD.

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

&IF DEFINED(EXCLUDE-auditoriaBorrado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auditoriaBorrado Procedure 
PROCEDURE auditoriaBorrado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteDescartePacking) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDescartePacking Procedure 
PROCEDURE deleteDescartePacking :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piPesada AS INTEGER    NO-UNDO.

  FOR EACH balanza_pesadas
      WHERE balanza_pesadas.id_balanza = 3
        AND balanza_pesadas.id_pesada  = piPesada.

    /* borro items transporte */
    FOR EACH items_mov_transp
        WHERE items_mov_transp.nromov = balanza_pesadas.nromov.
      DELETE items_mov_transp.
    END.
    /* borro balanza tickets */
    FOR EACH balanza_tickets
        WHERE balanza_tickets.id_pesada = balanza_pesadas.id_pesada.
      DELETE balanza_tickets.
    END.
    /* borro cabecera transporte */
    FOR EACH mov_transp_cabecera
        WHERE mov_transp_cabecera.nromov = balanza_pesadas.nromov.
      DELETE mov_transp_cabecera.
    END.
    /* borro balanza_pesadas */
    DELETE balanza_pesadas.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteMoliendaSilo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteMoliendaSilo Procedure 
PROCEDURE deleteMoliendaSilo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piLin AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTur AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSil AS INTEGER    NO-UNDO.

  FOR EACH items_molienda_silo
      WHERE items_molienda_silo.id_linea  = piLin
        AND items_molienda_silo.fecha     = pdFec
        AND items_molienda_silo.id_turno  = piTur
        AND items_molienda_silo.id_silo   = piSil.

    IF items_molienda_silo.id_linea = 6 AND items_molienda_silo.kilos_descarte_packing <> 0 THEN
      RUN deleteDescartePacking (items_molienda_silo.id_pesada).
    
    DELETE items_molienda_silo.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMoliendaSilo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMoliendaSilo Procedure 
PROCEDURE setMoliendaSilo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piLin AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTur AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piSil AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcHIn AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcHFi AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pfKil AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pfDes AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iNextId AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPesada AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fKilDes AS DECIMAL    NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  /* creo el registro cabecera de dia si no existe */
  FIND FIRST molienda_silo
       WHERE molienda_silo.id_linea  = piLin
         AND molienda_silo.fecha     = pdFec
         AND molienda_silo.id_turno  = piTur
       NO-ERROR.
  IF NOT AVAILABLE molienda_silo THEN DO:
    iNextId = DYNAMIC-FUNCTION('getNextSequence' IN hLibCom, 'molienda_silo', 'id_molienda').
    CREATE molienda_silo.
    ASSIGN  molienda_silo.id_molienda = iNextId
            molienda_silo.id_linea    = piLin
            molienda_silo.fecha       = pdFec
            molienda_silo.id_turno    = piTur
            molienda_silo.c_fecha     = TODAY
            molienda_silo.c_hora      = STRING(TIME, "HH:MM")
            molienda_silo.c_usuario   = USERID('userdb')
            .
  END.

  fKilDes = pfDes.
  IF piSil = 98 OR piSil = 99 THEN DO: /*silos de molienda directa */
    fKilDes = 0.
  END.

  /* busco molienda del silo para ese dia en ese truno, si existe modifico sino, creo */
  FIND FIRST items_molienda_silo
       WHERE items_molienda_silo.id_linea       = piLin
         AND items_molienda_silo.fecha          = pdFec
         AND items_molienda_silo.id_turno       = piTur
         AND items_molienda_silo.id_silo        = piSil
         AND items_molienda_silo.hora_comienzo  = pcHIn
         AND items_molienda_silo.hora_termino   = pcHFi
       NO-ERROR.
  IF NOT AVAILABLE(items_molienda_silo) THEN DO:
    CREATE items_molienda_silo.
  END.

  ASSIGN  items_molienda_silo.id_linea        = piLin
          items_molienda_silo.fecha           = pdFec 
          items_molienda_silo.id_turno        = piTur
          items_molienda_silo.id_molienda     = molienda_silo.id_molienda
          items_molienda_silo.id_silo         = piSil
          items_molienda_silo.hora_comienzo   = pcHIn
          items_molienda_silo.hora_termino    = pcHFi
          items_molienda_silo.kilos           = pfKil
          items_molienda_silo.kilos_descarte  = fKilDes
          items_molienda_silo.id_pesada       = piNro
          items_molienda_silo.c_fecha         = TODAY
          items_molienda_silo.c_hora          = STRING(TIME, "HH:MM")
          items_molienda_silo.c_usuario       = USERID('userdb')
          .

  /* grabo descarte packing en balanza */
  IF piLin = 6 AND pfDes <> 0 THEN DO:
    iPesada = setDescartePacking (pdFec,
                                  1, /*la linea 6 industria es igual a la linea 1 de packing*/
                                  pcHIn, 
                                  pcHFi,
                                  fKilDes, 
                                  piSil, 
                                  9).
    /* grabo nro de pesada en silo */
    ASSIGN items_molienda_silo.id_pesada = iPesada.
  END.


  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ctrlHora) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ctrlHora Procedure 
FUNCTION ctrlHora RETURNS CHARACTER
  (pcHora AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF INTEGER(pcHora) > 23 THEN
    cRet = "La Hora debe estar entre 0 y 24".


  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ctrlKilosCajonera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ctrlKilosCajonera Procedure 
FUNCTION ctrlKilosCajonera RETURNS CHARACTER
  (pcKilos AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF INTEGER(pcKilos) > 30000 THEN
    cRet = "Los Kilos de una Cajonera deben ser mayores que 0 y no pueden superar los Kg. 30000".

  /*  
  IF INTEGER(pcKilos) <= 0 THEN
    cRet = "Los Kilos de una Cajonera deben ser mayores que 0 y no pueden superar los Kg. 30000".
  */
  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ctrlMin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ctrlMin Procedure 
FUNCTION ctrlMin RETURNS CHARACTER
  (pcMin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  IF INTEGER(pcMin) > 59 THEN
    cRet = "Los Minutos van entre 0 y 59".


  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosSupervisor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosSupervisor Procedure 
FUNCTION getDatosSupervisor RETURNS CHARACTER
  (pcCodigo AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "Codigo Incorrecto".
  FOR FIRST supervisores_proceso
      WHERE supervisores_proceso.codigo_personal = pcCodigo
      NO-LOCK.
    cRet = STRING(supervisores_proceso.id_supervisor) + CHR(10) + 
           supervisores_proceso.apellido + ", " + supervisores_proceso.nombre.
  END.
  

  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescripcionLinea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescripcionLinea Procedure 
FUNCTION getDescripcionLinea RETURNS CHARACTER
  (piLin AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST lineas_produccion
      WHERE lineas_produccion.id_linea = piLin
      NO-LOCK.
    cRet = lineas_produccion.abreviatura.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIngresoBalanzaDia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIngresoBalanzaDia Procedure 
FUNCTION getIngresoBalanzaDia RETURNS DECIMAL
  (pdFec AS DATE,
   piLin AS INTEGER,
   piTur AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fPes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.

  DEFINE BUFFER buRo FOR balanza_tickets.

  IF piLin = 5 THEN iLin = 2.
  IF piLin = 6 THEN iLin = 1.

  FOR EACH buRo
      WHERE buRo.id_balanza       = 3
        AND buRo.id_materia_prima = 1
        AND buRo.fecha_operativa  = pdFec 
      NO-LOCK, 
      EACH balanza_pesadas
        OF buRo
      WHERE balanza_pesadas.id_pesada_ctf = iLin
      NO-LOCK.
      fPes = fPes + buRo.peso_neto_ticket.
  END.


  RETURN fPes.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIngresoBalanzaTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIngresoBalanzaTurno Procedure 
FUNCTION getIngresoBalanzaTurno RETURNS DECIMAL
  (pdFec AS DATE,
   piLin AS INTEGER,
   piTur AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fPes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.

  DEFINE BUFFER buRo FOR balanza_tickets.

  IF piLin = 5 THEN iLin = 1.
  IF piLin = 6 THEN iLin = 2.

  FOR EACH buRo
      WHERE buRo.id_balanza       = 3
        AND buRo.id_materia_prima = 1
        AND buRo.fecha_operativa  = DATE('06/08/2007') 
      NO-LOCK, 
      EACH balanza_pesadas
        OF buRo
      WHERE balanza_pesadas.id_pesada_ctf = iLin
        AND (IF piTur = 1 THEN INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) >= 6  AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 13 AND (INTEGER(ENTRY(2, balanza_pesadas.hora_entrada, ":")) > 0 AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 59) ELSE TRUE) 
        AND (IF piTur = 2 THEN INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) >= 14 AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 21 AND (INTEGER(ENTRY(2, balanza_pesadas.hora_entrada, ":")) > 0 AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 59) ELSE TRUE)
        AND ((IF piTur = 3 THEN INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) >= 22 AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 24 ELSE TRUE) OR (IF piTur = 3 THEN INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) > 0  AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 5 AND INTEGER(ENTRY(2, balanza_pesadas.hora_entrada, ":")) > 0 AND INTEGER(ENTRY(1, balanza_pesadas.hora_entrada, ":")) <= 59  ELSE TRUE))
      NO-LOCK.
      fPes = fPes + buRo.peso_neto_ticket.
  END.


  RETURN fPes.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaDia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMoliendaDia Procedure 
FUNCTION getMoliendaDia RETURNS DECIMAL
  (pdFec AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  
  FOR EACH molienda_silo
      WHERE molienda_silo.fecha    = pdFec
      NO-LOCK.
    FOR EACH items_molienda_silo 
          OF molienda_silo 
        NO-LOCK.
      fKil = fKil + items_molienda_silo.kilos + items_molienda_silo.kilos_descarte_packing .

    END.

  END.


  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaDiaLinea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMoliendaDiaLinea Procedure 
FUNCTION getMoliendaDiaLinea RETURNS DECIMAL
  (pdFec AS DATE, 
   piLin AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  
  FOR EACH molienda_silo
      WHERE molienda_silo.fecha    = pdFec
        AND molienda_silo.id_linea = piLin
      NO-LOCK.
    FOR EACH items_molienda_silo 
          OF molienda_silo 
        NO-LOCK.
      fKil = fKil + items_molienda_silo.kilos + items_molienda_silo.kilos_descarte_packing .
    END.
  END.


  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaSilos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMoliendaSilos Procedure 
FUNCTION getMoliendaSilos RETURNS CHARACTER
  (pdFec AS DATE, 
   piLin AS INTEGER,
   piTur AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStream AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf    AS CHARACTER  NO-UNDO.

  crlf = CHR(10).

  FOR EACH molienda_silo
      WHERE molienda_silo.fecha     = pdFec
        AND molienda_silo.id_linea  = piLin
        AND molienda_silo.id_turno  = piTur
      NO-LOCK,
      EACH items_molienda_silo
        OF molienda_silo
      BY items_molienda_silo.id_silo.

    cStream = cStream 
            + STRING(molienda_silo.id_linea) + CHR(1)
            + STRING(items_molienda_silo.id_silo) + CHR(1)
            + items_molienda_silo.hora_comienzo + CHR(1) 
            + items_molienda_silo.hora_termino + CHR(1) 
            + STRING(items_molienda_silo.kilos) + CHR(1) 
            + STRING(items_molienda_silo.kilos_descarte) 
            + crlf.
    
  END.

  cStream = SUBSTRING(cStream, 1, LENGTH(cStream) - 1).

  RETURN cStream.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMoliendaTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMoliendaTurno Procedure 
FUNCTION getMoliendaTurno RETURNS DECIMAL
  (pdFec AS DATE,
   piTur AS INTEGER, 
   piLin AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  
  FOR FIRST molienda_silo
      WHERE molienda_silo.fecha    = pdFec
        AND molienda_silo.id_turno = piTur
        AND molienda_silo.id_linea = piLin
      NO-LOCK.
    FOR EACH items_molienda_silo OF molienda_silo NO-LOCK.
      fKil = fKil + (items_molienda_silo.kilos + items_molienda_silo.kilos_descarte_packing).
    END.

  END.


  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPesoFromNroPesada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPesoFromNroPesada Procedure 
FUNCTION getPesoFromNroPesada RETURNS DECIMAL
  (piNroPesada AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  FOR EACH balanza_tickets
      WHERE balanza_tickets.id_balanza = 3
        AND balanza_tickets.id_pesada  = piNroPesada
      NO-LOCK.
      fRet = fRet + balanza_tickets.peso_neto.
  END.

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStockSilos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStockSilos Procedure 
FUNCTION getStockSilos RETURNS DECIMAL
  (pdFec AS DATE, 
   piLin AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fLav AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fFam AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  
  FOR EACH subd_produccion
      WHERE subd_produccion.fecha = pdFec
      NO-LOCK.
    fLav = fLav + subd_produccion.stock_silos_lavalle.
    fFam = fFam + subd_produccion.stock_silos_famailla.
  END.

  IF piLin = 0 THEN 
    fRet = fFam + fLav.
  ELSE
    fRet = IF piLin = 5 THEN fLav ELSE fFam.


  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalesTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalesTurno Procedure 
FUNCTION getTotalesTurno RETURNS CHARACTER
  (piLin AS INTEGER,
   pdFec AS DATE,
   piTur AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dDes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "0" + CHR(1) + "0" + CHR(1) + "0".

  FOR EACH items_molienda_silo
      WHERE items_molienda_silo.id_linea = piLin
        AND items_molienda_silo.fecha    = pdFec
        AND items_molienda_silo.id_turno = piTur
      NO-LOCK.
    dKil = dKil + items_molienda_silo.kilos.
    dDes = dDes + items_molienda_silo.kilos_descarte.
    dTot = dTot + (items_molienda_silo.kilos + items_molienda_silo.kilos_descarte).

  END.

  cRet = STRING(dKil) + CHR(1) + STRING(dDes) + CHR(1) + STRING(dTot).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDescartePacking) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDescartePacking Procedure 
FUNCTION setDescartePacking RETURNS INTEGER
  (pdFec AS DATE,
   piLin AS INTEGER,
   pcEnt AS CHARACTER,
   pcSal AS CHARACTER,
   pfKil AS DECIMAL,
   piSil AS INTEGER,
   piCal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.
  

  DEFINE VARIABLE x_importe LIKE tarifas_transporte.importe.
  
  /* Buffer para balanza_pesadas correlativos por balanza */
  DEFINE BUFFER aux_pesadas FOR balanza_pesadas.  
  /* Buffer para balanza_tickets correlativos por balanza_pesada */
  DEFINE BUFFER aux_tickets FOR balanza_tickets. 
  
  FIND LAST aux_pesadas WHERE aux_pesadas.id_balanza = 3 NO-LOCK NO-ERROR.
  FIND LAST mov_transp_cabecera WHERE mov_transp_cabecera.nromov < 5000000 NO-LOCK NO-ERROR.
  FIND transportes_proveedor WHERE transportes_proveedor.id_transporte = 2 NO-LOCK NO-ERROR.

  iRet = aux_pesadas.id_pesada + 1.
  CREATE balanza_pesadas.
  ASSIGN
      balanza_pesadas.id_balanza              = 3
      balanza_pesadas.id_pesada               = aux_pesadas.id_pesada + 1
      balanza_pesadas.nromov                  = mov_transp_cabecera.nromov + 1
      balanza_pesadas.id_proveedor            = 1
      balanza_pesadas.id_transporte           = 2
      balanza_pesadas.peso_entrada            = 1
      balanza_pesadas.id_tipo_transporte      = transportes_proveedor.id_tipo_transporte
      balanza_pesadas.marca                   = transportes_proveedor.marca
      balanza_pesadas.modelo                  = transportes_proveedor.modelo
      balanza_pesadas.patente                 = transportes_proveedor.patente
      balanza_pesadas.tara                    = transportes_proveedor.tara
      balanza_pesadas.nromov_0                = true
      balanza_pesadas.peso_envases_entrada    = 0
      .
  /* parametros */
  ASSIGN
      balanza_pesadas.id_pesada_ctf           = piLin
      balanza_pesadas.fecha_entrada           = pdFec
      balanza_pesadas.hora_entrada            = pcEnt
      balanza_pesadas.fecha_salida            = pdFec
      balanza_pesadas.hora_salida             = pcSal
      balanza_pesadas.fecha_operativa         = pdFec
      balanza_pesadas.peso_salida             = pfKil + 1
      balanza_pesadas.peso_neto               = ABSOLUTE(balanza_pesadas.peso_entrada - balanza_pesadas.peso_salida)
      .
  
  
  /* movimientos en transporte */
  CREATE mov_transp_cabecera.
  ASSIGN
      mov_transp_cabecera.id_transporte       = balanza_pesadas.id_transporte
      mov_transp_cabecera.id_tipo_transporte  = balanza_pesadas.id_tipo_transporte
      mov_transp_cabecera.marca               = balanza_pesadas.marca
      mov_transp_cabecera.modelo              = balanza_pesadas.modelo
      mov_transp_cabecera.patente             = balanza_pesadas.patente
      mov_transp_cabecera.tara                = balanza_pesadas.tara
      mov_transp_cabecera.nromov              = balanza_pesadas.nromov
      mov_transp_cabecera.id_proveedor        = balanza_pesadas.id_proveedor
      mov_transp_cabecera.fecha               = balanza_pesadas.fecha_salida
      mov_transp_cabecera.fecha_entrada       = balanza_pesadas.fecha_entrada
      mov_transp_cabecera.hora_entrada        = balanza_pesadas.hora_entrada
      mov_transp_cabecera.fecha_salida        = balanza_pesadas.fecha_salida
      mov_transp_cabecera.hora_salida         = balanza_pesadas.hora_salida
      mov_transp_cabecera.fecha_operativa     = balanza_pesadas.fecha_operativa
      .
   
  
  FIND LAST aux_tickets 
       WHERE aux_tickets.id_balanza = 3 
       NO-LOCK NO-ERROR.
  
  CREATE balanza_tickets.
  ASSIGN
      balanza_tickets.id_balanza              = balanza_pesadas.id_balanza
      balanza_tickets.id_pesada               = balanza_pesadas.id_pesada
      balanza_tickets.nro_ticket              = aux_tickets.nro_ticket + 1
      balanza_tickets.nro_remito              = "888888888888"
      balanza_tickets.id_materia_prima        = 1
      balanza_tickets.id_proveedor            = 1
      balanza_tickets.id_proveedor_origen     = 1
      balanza_tickets.id_origen               = 97
      balanza_tickets.id_origen_origen        = 97
      balanza_tickets.id_lote                 = 0
      balanza_tickets.id_tipo_cosecha         = 0
      balanza_tickets.id_tipo_servicio        = 1
      balanza_tickets.id_envase               = 0
      balanza_tickets.cant_env_entrada        = 0
      balanza_tickets.silo2                   = 0
      balanza_tickets.silo3                   = 0
      balanza_tickets.silo4                   = 0
      balanza_tickets.fecha_remito            = balanza_pesada.fecha_entrada
      balanza_tickets.id_descarte             = 0
      balanza_tickets.peso_descarte           = 0
      balanza_tickets.id_sucursal_packing     = 0
      balanza_tickets.id_sucursal             = 95
      balanza_tickets.fecha_entrada           = balanza_pesadas.fecha_entrada
      balanza_tickets.hora_entrada            = balanza_pesadas.hora_entrada
      balanza_tickets.fecha_salida            = balanza_pesadas.fecha_salida
      balanza_tickets.hora_salida             = balanza_pesadas.hora_salida
      balanza_tickets.fecha_operativa         = balanza_pesadas.fecha_operativa
      balanza_tickets.peso_neto_ticket        = balanza_pesadas.peso_neto
      .
  
  /* parametros */
  ASSIGN
      balanza_tickets.id_calidad              = piCal
      balanza_tickets.silo1                   = piSil
      .
  
  
  
  FIND FIRST proveedores OF balanza_tickets NO-LOCK NO-ERROR.

  FIND FIRST origenes 
       WHERE origenes.id_proveedor = balanza_tickets.id_proveedor 
         AND origenes.id_origen    = balanza_tickets.id_origen 
       NO-LOCK NO-ERROR.

  FIND FIRST lote 
       WHERE lote.id_proveedor = balanza_tickets.id_proveedor 
         AND lote.id_origen = balanza_tickets.id_origen 
         AND lote.id_lote = balanza_tickets.id_lote 
        NO-LOCK NO-ERROR.

  FIND FIRST sucursales OF balanza_tickets NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF balanza_tickets NO-LOCK NO-ERROR.

  FIND FIRST r_origenes_sucursales 
       WHERE r_origenes_sucursales.id_proveedor = balanza_tickets.id_proveedor 
         AND r_origenes_sucursales.id_origen = balanza_tickets.id_origen 
         AND r_origenes_sucursales.id_sucursal = balanza_tickets.id_sucursal 
       NO-LOCK NO-ERROR.

  FIND FIRST r_servicios_sucursales 
       WHERE r_servicios_sucursales.id_tipo_servicio = balanza_tickets.id_tipo_servicio 
         AND r_servicios_sucursales.id_sucursal = balanza_tickets.id_sucursal
       NO-LOCK NO-ERROR.

  FIND FIRST tarifas_transporte WHERE tarifas_transporte.id_tarifa = 0 NO-LOCK NO-ERROR.
  
  
  CREATE items_mov_transp.
  ASSIGN
      items_mov_transp.nromov                 = balanza_pesadas.nromov
      items_mov_transp.item                   = balanza_tickets.nro_ticket
      items_mov_transp.nro_remito             = balanza_tickets.nro_remito
      items_mov_transp.fecha_remito           = balanza_tickets.fecha_remito
      items_mov_transp.id_proveedor           = balanza_tickets.id_proveedor
      items_mov_transp.nombre                 = proveedores.nombre
      items_mov_transp.id_origen              = balanza_tickets.id_origen
      items_mov_transp.origen                 = origenes.descripcion
      items_mov_transp.id_lote                = balanza_tickets.id_lote
      items_mov_transp.lote                   = lote.descripcion
      items_mov_transp.id_sucursal            = balanza_tickets.id_sucursal
      items_mov_transp.id_proveedor_destino   = 1
      items_mov_transp.id_origen_destino      = 95
      items_mov_transp.destino                = sucursales.nombre
      items_mov_transp.id_tarifa              = r_origenes_sucursales.id_tarifa
      items_mov_transp.valor                  = x_importe
      items_mov_transp.id_envase              = balanza_tickets.id_envase
      items_mov_transp.envase                 = envases_prod.descripcion
      items_mov_transp.cant_envases           = balanza_tickets.cant_env_entrada
      items_mov_transp.peso                   = balanza_tickets.peso_neto_ticket
      items_mov_transp.id_centro_costo        = r_servicios_sucursales.id_centro_costo
      items_mov_transp.id_detalle_transporte  = 0
      items_mov_transp.importe                = (balanza_tickets.peso_neto_ticket / 1000)
                                                * tarifas_transporte.importe1
      items_mov_transp.fecha_operativa        = balanza_tickets.fecha_operativa
      items_mov_transp.fecha_entrada          = balanza_tickets.fecha_entrada
      items_mov_transp.hora_entrada           = balanza_tickets.hora_entrada
      items_mov_transp.fecha_salida           = balanza_tickets.fecha_salida
      items_mov_transp.hora_salida            = balanza_tickets.hora_salida.
  
  FIND FIRST proveedores WHERE proveedores.id_proveedor = 1 NO-LOCK NO-ERROR.

  ASSIGN items_mov_transp.nombre_prov_destino = proveedores.nombre.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validarSupervisorTurno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validarSupervisorTurno Procedure 
FUNCTION validarSupervisorTurno RETURNS LOGICAL
  (piTurno AS INTEGER,
   piSuper AS INTEGER, 
   pdFecha AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FIND FIRST turnos_supervisor
       WHERE turnos_supervisor.id_supervisor = piSuper
         AND turnos_supervisor.id_turno      = piTurno
         AND turnos_supervisor.fecha         = pdFecha
       NO-LOCK NO-ERROR.
  IF AVAILABLE turnos_supervisor THEN 
    lRet = TRUE.
  ELSE 
    lRet = FALSE.

  /* OOOOOOJJJJJJOOOOOOOOOO
     cambiar aqui  */
  lRet = TRUE.
  
  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

