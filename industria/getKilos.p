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
DEFINE OUTPUT PARAMETER pkilos AS DECIMAL    NO-UNDO.

DEFINE BUFFER b_items FOR ventas.items_venta.
DEFINE BUFFER b_subd  FOR ventas.subd_vtas.
DEFINE BUFFER b_aux   FOR ventas.aux_subd_ventas.

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

 FIND FIRST b_items WHERE ROWID(b_items) = prowid NO-LOCK NO-ERROR.
 IF AVAILABLE b_items THEN
 DO:
    pkilos = 0.
    CASE b_items.tipo_unidad:
      WHEN "K" THEN
          pkilos = b_items.cantidad.
      WHEN "T" THEN
         pkilos = b_items.cantidad * 1000.
      WHEN "G" OR WHEN "L" THEN
       DO:
         FOR EACH r_items_venta_pack_list OF b_items NO-LOCK:
            FIND FIRST items_packing_list WHERE 
                items_packing_list.id_sucursal     = r_items_venta_pack_list.id_sucursal AND
                items_packing_list.id_packing_list = r_items_venta_pack_list.id_packing_list AND
                items_packing_list.ITEM            = r_items_venta_pack_list.ITEM_pack NO-LOCK NO-ERROR.
            IF AVAILABLE items_packing_list THEN
                DO:
                 IF items_packing_list.cantidad = r_items_venta_pack_list.cantidad THEN
                       pkilos = pkilos + items_packing_list.kilos.
                END.
         END.
      END.
    END CASE.

   IF pkilos = 0 THEN DO:
       IF b_items.ITEM = 1 THEN
       DO:
          FIND FIRST b_subd OF b_items NO-LOCK NO-ERROR.
          IF AVAILABLE b_subd THEN
              FIND FIRST b_aux OF b_subd NO-LOCK NO-ERROR.
              IF AVAILABLE b_aux THEN
                pkilos = b_aux.peso_neto.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


