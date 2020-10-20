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
DEFINE INPUT  PARAMETER P-ROWID  AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER P-ESTADO AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE BUFFER b_subd FOR subd_vtas.

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

FIND FIRST subd_vtas WHERE ROWID(subd_vtas) = p-rowid NO-LOCK NO-ERROR.
FIND FIRST R_FACT_VENTAS WHERE R_FACT_VENTAS.ID_PUNTO_VENTA_VENTAS   = SUBD_VTAS.ID_PUNTO_VENTA AND
                               R_FACT_VENTAS.NROMOV_VENTAS           = SUBD_VTAS.NROMOV NO-LOCK NO-ERROR.
IF AVAILABLE r_fact_ventas THEN DO:

   FIND FIRST b_subd WHERE b_subd.id_punto_venta = r_fact_ventas.id_punto_venta and
                           b_subd.nromov         = r_fact_ventas.nromov NO-LOCK NO-ERROR.
   IF AVAILABLE b_subd THEN DO:   
       CASE b_subd.id_tipocomp:
           WHEN 24 THEN
               p-estado = subd_vtas.estado.
           WHEN 26 THEN
               p-estado = FALSE.
       END CASE.
   END.
END.
ELSE
    p-estado = subd_vtas.estado.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


