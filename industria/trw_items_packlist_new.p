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
DEFINE VAR v_total_tambores AS INTEGER.
DEF VAR h AS HANDLE.


if available items_packing_list Then
  DO:
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
