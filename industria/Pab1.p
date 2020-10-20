
FOR EACH  MUESTRAS WHERE MUESTRAS.FECHA  >= DATE("01/01/2013") NO-LOCK , EACH general.items_muestras WHERE general.items_muestras.id_muestra = general.muestras.id_muestra
  AND general.items_muestras.anio_muestra = general.muestras.anio_muestra NO-LOCK,
  EACH general.envases_muestras OF general.items_muestras NO-LOCK.
/*    EACH general.estados_muestra WHERE general.estados_muestra.id_estado_muestra = general.items_muestras.id_estado_lote_enviado NO-LOCK. */

    DISP ITEMS_MUESTRAS.ID_MUESTRA ITEMS_MUESTRAS.ID_ESTADO_LOTE.
END.


FOR EACH ESTADOS_MUESTRA.
    DISP ESTADOS_MUESTRA.
END.

