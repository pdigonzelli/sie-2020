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

&IF DEFINED(EXCLUDE-getNextNroProduccion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNextNroProduccion Procedure 
PROCEDURE getNextNroProduccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER piNroProduccion AS INTEGER NO-UNDO.
  
DEFINE VARIABLE iSeq AS INTEGER NO-UNDO.
FOR FIRST produccion_cascara BY id_produccion DESC .
  iSeq = produccion_cascara.id_produccion + 1.
END.

IF iSeq <= 0 THEN
  iSeq = 1.

piNroProduccion = iSeq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setProduccionDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProduccionDestino Procedure 
PROCEDURE setProduccionDestino :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prRegistro AS ROWID      NO-UNDO.
  DEFINE INPUT  PARAMETER piEmp      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro      AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcAction   AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i  AS INTEGER    NO-UNDO.

  FOR FIRST registro_produccion_cascara
      WHERE ROWID(registro_produccion_cascara) = prRegistro.

    IF pcAction = "+" THEN    
      ASSIGN registro_produccion_cascara.id_empresa_destino    = piEmp
             registro_produccion_cascara.id_sucursal_destino   = piSuc
             registro_produccion_cascara.id_tipotambor_destino = piTip
             registro_produccion_cascara.nromov_destino        = piNro
             .
    IF pcAction = "-" THEN
      ASSIGN registro_produccion_cascara.id_empresa_destino    = 0
             registro_produccion_cascara.id_sucursal_destino   = 0
             registro_produccion_cascara.id_tipotambor_destino = 0
             registro_produccion_cascara.nromov_destino        = 0
             .


  END.

  FOR FIRST produccion_cascara
      WHERE produccion_cascara.id_empresa     = piEmp
        AND produccion_cascara.id_sucursal    = piSuc
        AND produccion_cascara.id_tipotambor  = piTip
        AND produccion_cascara.nromov         = piNro.
    
    FOR EACH registro_produccion_cascara
        WHERE registro_produccion_cascara.nromov_destino = produccion_cascara.nromov
        NO-LOCK.
      i = i + registro_produccion_cascara.cantidad_bolsas.
    END.

    ASSIGN produccion_cascara.cantidad_bolsas = i.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

