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
DEFINE INPUT  PARAMETER prowid AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER pfecha AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pmonto AS CHARACTER  NO-UNDO.

DEFINE BUFFER b_subd FOR subd_vtas.
DEFINE VARIABLE vtipocambio AS DECIMAL  DECIMALS 6  NO-UNDO.
DEFINE VARIABLE vfecha AS DATE       NO-UNDO.
DEFINE VARIABLE vimporte AS DECIMAL    NO-UNDO.

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

FIND b_subd WHERE ROWID(b_subd) = prowid NO-LOCK NO-ERROR.
IF AVAILABLE b_subd THEN DO:

    FOR EACH r_cobranzas_comp
         WHERE r_cobranzas_comp.id_punto_venta = b_subd.id_punto_venta AND
               r_cobranzas_comp.id_operacion   = b_subd.id_operacion   AND
               r_cobranzas_comp.nromov         = b_subd.nromov NO-LOCK:

        RUN getTipoCambio (BUFFER r_cobranzas_comp,
                           OUTPUT vtipocambio,
                           OUTPUT vfecha).

        vimporte = r_cobranzas_comp.importe_pago.
        pmonto = pmonto + STRING(vimporte)  + ";".
        pfecha = pfecha + STRING(vfecha,"99/99/99") + ";" .
      

    END.

        pmonto = SUBSTRING(pmonto,1,LENGTH(pmonto) - 1).
        pfecha = SUBSTRING(pfecha,1,LENGTH(pfecha) - 1).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getTipoCambio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTipoCambio Procedure 
PROCEDURE getTipoCambio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER b_rcob FOR r_cobranzas_comp.
DEFINE OUTPUT PARAMETER ptipocambio AS DECIMAL DECIMALS 6 NO-UNDO.
DEFINE OUTPUT PARAMETER pfecha AS DATE       NO-UNDO.

    FIND FIRST cobranzas_me 
          WHERE cobranzas_me.id_tipocomp  = b_rcob.id_tipocomp_cobranza  AND
                cobranzas_me.id_operacion = b_rcob.id_operacion_cobranza AND 
                cobranzas_me.nromov       = b_rcob.nromov_cobranza AND
                cobranzas_me.id_sucursal  = b_rcob.id_sucursal_cobranza NO-LOCK NO-ERROR.
    IF AVAILABLE cobranzas_me THEN DO:
         ptipocambio = cobranzas_me.tipo_cambio.
         pfecha      = cobranzas_me.fecha_comp.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

