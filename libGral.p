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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getCotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCotizacion Procedure 
FUNCTION getCotizacion RETURNS DECIMAL
  ( INPUT porigen AS INTEGER, INPUT pmoneda AS INTEGER , INPUT pfecha AS DATE , pcomprador AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTextoFlete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTextoFlete Procedure 
FUNCTION getTextoFlete RETURNS CHARACTER
  ( INPUT pflete AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-monedaExistente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD monedaExistente Procedure 
FUNCTION monedaExistente RETURNS LOGICAL
  ( INPUT pmoneda AS INTEGER )  FORWARD.

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


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCotizacion Procedure 
FUNCTION getCotizacion RETURNS DECIMAL
  ( INPUT porigen AS INTEGER, INPUT pmoneda AS INTEGER , INPUT pfecha AS DATE , pcomprador AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND LAST cotizacion WHERE cotizacion.id_moneda_origen = porigen   AND
                        cotizacion.id_moneda_cambio = pmoneda          AND
                        cotizacion.fecha <= pfecha NO-LOCK NO-ERROR.

  IF AVAILABLE cotizacion THEN
  DO:
      IF pcomprador THEN
        RETURN importe_comprador.
      ELSE
          RETURN importe_vendedor.
  END.
  RETURN 1.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTextoFlete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTextoFlete Procedure 
FUNCTION getTextoFlete RETURNS CHARACTER
  ( INPUT pflete AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR v_flete AS CHARACTER NO-UNDO INITIAL '****SIN ASIGNAR****'.

FIND flete WHERE flete.id_flete = pflete NO-LOCK NO-ERROR.
IF AVAILABLE flete THEN
v_flete = flete.razon_social + chr(010) +
         flete.direccion + chr(010) +
         "GALPON 5 - PUERTA 1 " + chr(010) +
         flete.localidad + " - (" + string(flete.postal) + ") " + chr(010) +
         "TELEFONO " + string(flete.telefono)  + chr(010).

  RETURN V_FLETE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-monedaExistente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION monedaExistente Procedure 
FUNCTION monedaExistente RETURNS LOGICAL
  ( INPUT pmoneda AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND monedas WHERE monedas.id_moneda = pmoneda NO-LOCK NO-ERROR.
  IF AVAILABLE monedas THEN
      RETURN TRUE.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

