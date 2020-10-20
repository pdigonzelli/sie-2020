/*  
DEFINE TEMP-TABLE ttPackList
  FIELD nro_pack      LIKE packing_list.nro_pack_list
  FIELD fecha         AS DATE
  FIELD id_sucursal   AS INTEGER
  FIELD id_pallet     AS INTEGER
  FIELD id_cliente    AS INTEGER
  FIELD id_vapor      AS INTEGER
  FIELD id_destino    AS INTEGER
  /* pallet_puerto */
  FIELD id_suc        AS INTEGER
  FIELD id_pal        AS INTEGER
  FIELD id_cli        AS INTEGER
  FIELD id_vap        AS INTEGER
  FIELD id_des        AS INTEGER
  FIELD id_des_pack   AS INTEGER
  /* pallet */
  FIELD id_suc_pal    AS INTEGER
  FIELD id_pal_pal    AS INTEGER
  FIELD nro_pack_pal  AS CHARACTER
  .*/
  
  DEFINE BUFFER ttPackList FOR packlist.
  
FOR EACH packing_list NO-LOCK,
    EACH items_packing_list OF packing_list NO-LOCK
    WHERE packing_list.fecha >= DATE('01/01/2007')
    BREAK BY items_packing_list.id_sucursal BY items_packing_list.id_pallet.

  IF LAST-OF(items_packing_list.id_pallet) THEN DO:
    /* 
    FIND FIRST pallets_puerto OF items_packing_list NO-LOCK NO-ERROR.
    FIND FIRST pallets OF items_packing_list NO-LOCK NO-ERROR.
    */
    CREATE ttPackList.
    ASSIGN  ttPackList.nro_pack           = packing_list.nro_pack_list
            ttPackList.fecha              = packing_list.fecha
            ttPackList.id_sucursal        = items_packing_list.id_sucursal_remito
            ttPackList.id_pallet          = items_packing_list.id_pallet
            ttPackList.id_cliente         = packing_list.id_cliente
            ttPackList.id_vapor           = packing_list.id_vapor
            ttPackList.id_destino         = packing_list.id_destino 
            ttPackList.id_sucursal_remito = items_packing_list.id_sucursal_remito
            ttPackList.id_tipo_movsto     = items_packing_list.id_tipo_movsto
            ttPackList.nro                = items_packing_list.nro
            ttPackList.ITEM_remito        = items_packing_list.ITEM_remito
            .
    /* 
    IF AVAILABLE pallets_puerto  THEN DO:
      ASSIGN ttPackList.id_suc       = pallets_puerto.id_sucursal
            ttPackList.id_pal        = pallets_puerto.id_pallet
            ttPackList.id_cli        = pallets_puerto.id_cliente
            ttPackList.id_vap        = pallets_puerto.id_vapor
            ttPackList.id_des        = pallets_puerto.id_destino
            ttPackList.id_des_pack   = pallets_puerto.id_destino_packing
            .
    END.

    IF AVAILABLE pallets  THEN DO:
      ASSIGN ttPackList.id_suc_pal       = pallets.id_sucursal
            ttPackList.id_pal_pal        = pallets.id_pallet
            ttPackList.nro_pack_pal      = pallets.nro_pack_list 
            .
    END.
   */
  END.
  
END.

/* 
RUN generateExcel.p (INPUT TABLE ttPackList,
                       INPUT " Comparativo Packing List ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Arial",
                       INPUT 10).
  */
