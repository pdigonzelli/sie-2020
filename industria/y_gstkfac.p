/**********************************************************/
/* RUTINA DE CREACION DE MOVIMIENTOS DE STOCK DE TAMBORES */
/* DANIEL REYNA                                           */
/**********************************************************/
DEFINE INPUT PARAMETER v_punto_vta LIKE subd_vtas.id_punto_venta.
DEFINE INPUT PARAMETER v_nromov    LIKE subd_vtas.nromov.
DEFINE INPUT PARAMETER xtipo       LIKE tipos_movi.id_tipo_movimiento.
/* 5 Facturacion 6 Anulacion de Facturacion */

define buffer stock for stock_historico_tambores.
define buffer bstock for stock_historico_tambores.

DEFINE BUFFER xit FOR aux_items_venta.
DEFINE VARI vid_lote AS INTEGER.
DEFINE VARI vanio AS INTEGER.


find tipos_movi where 
     tipos_movi.id_tipo_movimiento = xtipo no-lock no-error.
if not available tipos_movi then do:
    message "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " xtipo
    view-as alert-box.
    return "error".
end.    


FIND subd_vtas WHERE subd_vtas.id_punto_venta = v_punto_vta AND
                     subd_vtas.nromov         = v_nromov
                     NO-LOCK NO-ERROR.
IF NOT AVAILABLE subd_vtas THEN DO:
        MESSAGE "ATENCION No Existe Factura" VIEW-AS ALERT-BOX.
        RETURN "error".
END.
FOR EACH xit OF subd_vtas NO-LOCK.
    FIND FIRST items_venta OF xit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE items_venta THEN DO:
        MESSAGE "Inconsistencia en items_venta" VIEW-AS ALERT-BOX.
        RETURN "error".
    END.
    ASSIGN vid_lote = INTEGER(SUBSTRING(xit.nro_lote,1,4))
           vanio    = INTEGER(SUBSTRING(xit.nro_lote,6,2)) + 2000.

    
/*    DISPLAY items_venta.id_articulo
            vid_lote 
            vanio 
            xit.nro_lote 
            desde_lote 
            hasta_lote WITH SCROLLABLE.
  */  
    FIND FIRST tambores_industria WHERE
            tambores_industria.id_articulo = items_venta.id_articulo AND
            tambores_industria.id_lote     = vid_lote AND
            tambores_industria.anio        = vanio AND
            tambores_industria.id_tambor   = xit.desde_lote
            NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tambores_industria THEN DO:
        MESSAGE "ATENCION incosistencia en tambores_industria" VIEW-AS ALERT-BOX.
        RETURN "error".
    END.

     
    create stock.
    assign
        stock.id_articulo        = items_venta.id_articulo
        stock.fecha              = today
        stock.id_tipo_movimiento = xtipo
        stock.id_lote            = vid_lote
        stock.anio               = vanio
        stock.tambor_desde       = xit.desde_lote
        stock.tambor_hasta       = xit.hasta_lote
        stock.id_suc_origen      = 85 /* verificar si siempre es 85*/
        stock.id_suc_des         = tambores_industria.id_sucursal_ubicacion
        stock.datos_adicionales  = tipos_movi.descripcion            
        stock.c_usuario          = userid("userdb")
        stock.c_fecha            = today
        stock.c_hora             = string(time,"hh:mm:ss")
        stock.id_empresa         = tambores_industria.id_empresa
        stock.id_sucursal        = tambores_industria.id_sucursal
        stock.id_tipotambor      = tambores_industria.id_tipotambor
        stock.nromov             = tambores_industria.nromov
        stock.id_serial          = next-value(serial-stock-tambores) 
        stock.signo              = if tipos_movi.codigo = "+" 
                                    then "-"
                                    else "+".
    create bstock.
    buffer-copy stock except stock.id_serial to bstock.
    assign
        bstock.id_serial         = next-value(serial-stock-tambores)
        bstock.id_suc_des        = 85 /* verificar si siempre es 85*/
        bstock.id_suc_origen     = tambores_industria.id_sucursal_ubicacion 
        bstock.signo             = if tipos_movi.codigo = "+" 
                                    then "+"
                                    else "-".
    

END.
