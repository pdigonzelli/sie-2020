DEFINE VAR i AS INTEGER.
DEF VAR iCant AS INTEGER.

FOR EACH ITEM_ingreso_lote_ubicacion 
    WHERE ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 428, 
    EACH lotes_ubicacion 
      OF ITEM_ingreso_lote_ubicacion 
    WHERE lotes_ubicacion.id_articulo = 53
    BREAK BY STRING(ITEM_ingreso_lote_ubicacion.nromov) + ITEM_ingreso_lote_ubicacion.id_lote_deposito
    .
  
  i = i + ITEM_ingreso_lote_ubicacion.cantidad.
  IF LAST-OF(STRING(ITEM_ingreso_lote_ubicacion.nromov) + ITEM_ingreso_lote_ubicacion.id_lote_deposito) THEN DO:

  /*calculo la cantidad de tambores comprometidos de ese lote pero en otros releases*/
  iCant = 0.
  FOR EACH items_release_delivery WHERE items_release_delivery.id_empresa             = ITEM_ingreso_lote_ubicacion.id_empresa
                                    AND items_release_delivery.id_sucursal            = ITEM_ingreso_lote_ubicacion.id_sucursal
                                    AND items_release_delivery.id_tipotambor          = ITEM_ingreso_lote_ubicacion.id_tipotambor
                                    AND items_release_delivery.nromov                 = ITEM_ingreso_lote_ubicacion.nromov
                                    AND items_release_delivery.id_lote_deposito       = ITEM_ingreso_lote_ubicacion.id_lote_deposito
                                  NO-LOCK.
    FIND FIRST RELEASE_delivery OF items_release_delivery NO-LOCK.
    
    iCant = iCant + items_release_delivery.tambores.        
 
   DISPLAY  ITEM_ingreso_lote_ubicacion.id_lote items_release_delivery.id_release items_release_delivery.tambores 
       numero_RELEASE. 
   

   END.



     FIND FIRST tambores_industria 
           WHERE tambores_industria.nromov = ITEM_ingreso_lote_ubicacion.nromov 
           NO-LOCK NO-ERROR.
      FIND FIRST calidades 
           OF tambores_industria 
           NO-LOCK NO-ERROR.
      FIND FIRST envases_prod 
           OF tambores_industria 
           NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN
      DO:
          IF AVAILABLE calidades THEN
          DO:
          /*DISPLAY i  iCant item_ingreso_lote_ubicacion.id_lote_deposito
              tambores_industria.id_lote tambores_industria.id_calidad tambores_industria.id_envase
              ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion 
              ITEM_ingreso_lote_ubicacion.nromov WITH WIDTH 200 FRAME X */ .
          END.
      END.
  END.
  i = 0.
END.

