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
DEFINE INPUT  PARAMETER prowid       AS ROWID      NO-UNDO.
DEFINE INPUT  PARAMETER ptipo        AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcompvincula AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER ppevincula   AS CHARACTER  NO-UNDO.

DEFINE BUFFER b_subd FOR subd_vtas.
DEFINE BUFFER b_subdvin FOR subd_vtas.

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

CASE ptipo:
    WHEN "NC" THEN DO:
          FOR EACH R_FACT_VENTAS WHERE R_FACT_VENTAS.ID_PUNTO_VENTA = b_SUBD.ID_PUNTO_VENTA AND
                                         R_FACT_VENTAS.NROMOV        = b_SUBD.NROMOV NO-LOCK:
                    FIND b_subdvin WHERE b_subdvin.id_punto_venta = r_fact_ventas.id_punto_venta_ventas AND
                                         b_subdvin.nromov         = r_fact_ventas.nromov_ventas NO-LOCK NO-ERROR.
                    IF AVAILABLE b_subdvin THEN
                        pcompvincula = pcompvincula + STRING(b_subdvin.id_punto_venta) + "-" + STRING(b_subdvin.nro_proforma) + " ". 
           END.                             
    END.
    WHEN "P" THEN DO:
            FOR EACH R_FACT_VENTAS WHERE R_FACT_VENTAS.ID_PUNTO_VENTA_VENTAS = b_SUBD.ID_PUNTO_VENTA AND
                                         R_FACT_VENTAS.NROMOV_VENTAS         = b_SUBD.NROMOV NO-LOCK:
                FIND b_subdvin WHERE b_subdvin.id_punto_venta = r_fact_ventas.id_punto_venta AND
                                    b_subdvin.nromov         = r_fact_ventas.nromov NO-LOCK NO-ERROR.
                IF AVAILABLE b_subdvin THEN
                    pcompvincula = pcompvincula + STRING(b_subdvin.id_punto_venta) + "-" + STRING(b_subdvin.nro_proforma) + " ". 
             END.

    END.
    WHEN "PD" OR WHEN "N" THEN DO:
        FOR EACH R_FACT_VENTAS WHERE R_FACT_VENTAS.ID_PUNTO_VENTA_VENTAS = b_SUBD.ID_PUNTO_VENTA AND
                                     R_FACT_VENTAS.NROMOV_VENTAS         = b_SUBD.NROMOV NO-LOCK:
            FIND b_subdvin WHERE b_subdvin.id_punto_venta = r_fact_ventas.id_punto_venta AND
                                 b_subdvin.nromov         = r_fact_ventas.nromov NO-LOCK NO-ERROR.
            IF AVAILABLE b_subdvin THEN DO:
            
                pcompvincula = pcompvincula + STRING(b_subdvin.id_punto_venta) + "-" + STRING(b_subdvin.nro_comp) + " ". 

                FOR EACH r_subd_ventas_embarque 
                            WHERE r_subd_ventas_embarque.id_punto_venta = b_subdvin.id_punto_venta AND
                                  r_subd_ventas_embarque.nromov         = b_subdvin.nromov NO-LOCK:
                    ppevincula = ppevincula + r_subd_ventas_embarque.nro_embarque + " ".
                END.
            END.
        END.                             
    END.
    WHEN "D" THEN DO:
        FOR EACH R_FACT_VENTAS WHERE R_FACT_VENTAS.ID_PUNTO_VENTA = b_SUBD.ID_PUNTO_VENTA AND
                                     R_FACT_VENTAS.NROMOV        = b_SUBD.NROMOV NO-LOCK:
                FIND b_subdvin WHERE b_subdvin.id_punto_venta = r_fact_ventas.id_punto_venta_ventas AND
                                     b_subdvin.nromov         = r_fact_ventas.nromov_ventas NO-LOCK NO-ERROR.
                IF AVAILABLE b_subdvin THEN
                    pcompvincula = pcompvincula + STRING(b_subdvin.id_punto_venta) + "-" + STRING(b_subdvin.nro_proforma) + " ". 
       END.                             

    END.
END CASE.

IF pcompvincula <> "" THEN
    pcompvincula  = "(" + pcompvincula + ")".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


