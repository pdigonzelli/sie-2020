TRIGGER PROCEDURE FOR REPLICATION-WRITE OF items_packing_list OLD BUFFER old_items.
define buffer b_items for items_factura.
define buffer b_tambores for tambores_industria.
define buffer b_r_envases for r_envases_prod.
define buffer b_permisos for permisos_embarque.
DEFINE BUFFER b_pack FOR packing_list.
DEFINE BUFFER b_envases FOR envases_prod.

DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_bruto AS DECIMAL.
DEFINE VAR v_galones AS DECIMAL.
DEFINE VAR v_lote AS CHARACTER FORMAT "x(8)".
DEFINE VAR v_peso_neto AS DECIMAL.
DEFINE VAR v_peso_bruto AS DECIMAL.
DEF VAR h AS HANDLE.


if available items_packing_list Then
  DO:
    v_kilos = 0.
    v_galones = 0.
    v_bruto = 0.
    IF items_packing_list.kilos_por_envase = 0 THEN
      DO:
      FIND FIRST b_items WHERE 
           b_items.id_sucursal = items_packing_list.id_sucursal_remito AND
           b_items.id_tipo_movsto = items_packing_list.id_tipo_movsto AND
           b_items.nro = items_packing_list.nro AND
           b_items.ITEM = items_packing_list.ITEM_remito NO-LOCK NO-ERROR.
          IF AVAILABLE b_items THEN
          DO:
              /*****Se va a modificar posteriormente la estructura ******/
              v_lote = b_items.nro_lote.
              FIND FIRST b_tambores WHERE b_tambores.id_lote = integer(ENTRY(1,v_lote,"/")) AND
                   b_tambores.anio = integer(ENTRY(2,v_lote,"/")) + 2000 AND
                   (b_tambores.id_tipotambor = 3 OR b_tambores.id_tipotambor = 6) AND
                   b_tambores.id_locacion_ubicacion <> 10 AND 
                   b_tambores.id_articulo = b_items.id_articulo NO-LOCK NO-ERROR.
              IF AVAILABLE b_tambores THEN
                 DO:
                     v_kilos = v_kilos + (items_packing_list.cantidad * b_tambores.kilos_tambor).
                     v_bruto = v_bruto + (items_packing_list.cantidad * (b_tambores.kilos_tambor + b_tambores.tara)).
                     FIND FIRST lotes_jugo OF b_tambores NO-LOCK NO-ERROR.
                     IF  AVAILABLE lotes_jugo THEN 
                     DO:
                       FIND LAST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
                       IF AVAILABLE inspecciones_lote THEN
                       DO:
                           FIND LAST brix WHERE brix.brix <= ROUND(inspecciones_lote.bx_correg,1) NO-LOCK NO-ERROR.
                           IF AVAILABLE brix THEN 
                           DO:
                             IF brix.pe > 0 THEN
                               v_galones = items_packing_list.cantidad * (ROUND((b_tambores.kilos_tambor / brix.pe) / 3.785, 2)).
                           END.
                       END.
                     END.
                 END.
             ELSE
                 DO:
                    FIND FIRST b_r_envases WHERE 
                        b_r_envases.id_articulo = b_items.id_articulo AND 
                        b_r_envases.id_envase = b_items.id_envase 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b_r_envases THEN
                    DO:
                        v_kilos = v_kilos + (items_packing_list.cantidad * b_r_envases.kilos).
                        FIND FIRST b_envases WHERE b_envases.id_articulo = b_items.id_articulo NO-LOCK NO-ERROR.
                        IF AVAILABLE b_envases THEN
                        v_bruto = v_bruto + (items_packing_list.cantidad * (b_r_envases.kilos + b_envases.tara)).
                    END.
                 END.
          END.
   END.
   ELSE
    DO:
      v_kilos = v_kilos + (items_packing_list.cantidad * items_packing_list.kilos_por_envase).
    END.
    

    ASSIGN items_packing_list.kilos   = v_kilos
           items_packing_list.kilos_brutos = v_bruto 
           items_packing_list.galones = v_galones.
    
   
    /* Chequeo si los valores del permiso de embarque han cambiado */
    IF items_packing_list.anio <> OLD_items.anio OR
       items_packing_list.id_aduana <> OLD_items.id_aduana OR
       items_packing_list.nro_permiso_embarque <> OLD_items.nro_permiso_embarque THEN
    DO:
        
        /* Borro permiso de embarque viejo */

        find first b_permisos where b_permisos.anio = OLD_items.anio and
             b_permisos.id_aduana = OLD_items.id_aduana and
             b_permisos.id_permiso_embarque = OLD_items.nro_permiso_embarque
             no-error.
        IF AVAILABLE b_permisos THEN
           DELETE b_permisos.
        

        /* Creo nuevo permiso de embarque */       
        find first b_permisos where b_permisos.anio = items_packing_list.anio and
             b_permisos.id_aduana = items_packing_list.id_aduana and
             b_permisos.id_permiso_embarque = items_packing_list.nro_permiso_embarque
             no-lock no-error.
        if not available b_permisos Then
           do:
              FIND FIRST b_pack OF items_packing_list NO-LOCK NO-ERROR. 
              create b_permisos.
              assign b_permisos.anio = items_packing_list.anio
                     b_permisos.id_aduana = items_packing_list.id_aduana
                     b_permisos.id_permiso_embarque = items_packing_list.nro_permiso_embarque
                     b_permisos.tipo_pe = b_pack.id_tipo_pack_list
                     b_permisos.id_orden_entrega = items_packing_list.nro_orden_embarque
                     b_permisos.fecha = b_pack.fecha_salida_vapor
                     b_permisos.id_despachante = b_pack.id_despachante
                     b_permisos.id_articulo = items_packing_list.id_articulo
                     b_permisos.c_usuario = "PK" + USERID("userdb")
                     b_permisos.c_fecha   = TODAY
                     b_permisos.c_hora = STRING(TIME,"HH:MM:SS").
           end. 
     END.
END.
