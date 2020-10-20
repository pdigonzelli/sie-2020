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
DEFINE INPUT  PARAMETER p-rowid AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER p-tipo  AS CHARACTER  NO-UNDO.

DEFINE BUFFER b_rfact1 FOR r_fact_ventas.
DEFINE BUFFER b_rfact2 FOR r_fact_ventas.
DEFINE BUFFER b_subd   FOR subd_vtas.

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

/*
  1.- Chequeo que el comprobante vinculado al comprobante a analizar sea valido (impreso = yes)
*/

FIND FIRST subd_vtas WHERE ROWID(subd_vtas) = p-rowid NO-LOCK NO-ERROR.

FIND FIRST b_rfact1 /* Facturas Proformas */
             WHERE b_rfact1.id_punto_venta_ventas = subd_vtas.id_punto_venta AND
                   b_rfact1.nromov_ventas         = subd_vtas.nromov NO-LOCK NO-ERROR.
IF AVAILABLE b_rfact1 THEN DO:
    FIND FIRST b_subd WHERE b_subd.id_punto_venta = b_rfact1.id_punto_venta AND
                            b_subd.nromov         = b_rfact1.nromov         NO-LOCK NO-ERROR.

    IF AVAILABLE b_subd THEN DO: 

       IF b_subd.id_tipocomp <> 26 AND  /* Determino si posee definitiva */ 
          b_subd.nro_proforma = 0  AND 
          b_subd.impreso           THEN 
            p-tipo = "PD".
       ELSE DO: /* Puede estar vinculado a un Comp. no valido - Determino si es proforma o normal*/
          IF subd_vtas.nro_proforma <> 0  THEN
            p-tipo = "P".
          ELSE
            p-tipo = "N".
       END.
    END.
END.
ELSE DO:          /* Notas de Credito - Facturas Definitivas */
    IF subd_vtas.id_tipocomp = 26 THEN DO:
            p-tipo = "NC".
    END.
    ELSE DO:
        
        FIND FIRST b_rfact1
                 WHERE b_rfact1.id_punto_venta  = subd_vtas.id_punto_venta AND
                       b_rfact1.nromov          = subd_vtas.nromov NO-LOCK NO-ERROR.
        IF AVAILABLE b_rfact1 THEN  
            IF subd_vtas.nro_proforma = 0 THEN 
                p-tipo = "D".
            ELSE
                p-tipo = "P".
        ELSE DO:
            IF subd_vtas.nro_proforma <> 0 THEN
                p-tipo = "P".
            ELSE
                p-tipo = "N".
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


