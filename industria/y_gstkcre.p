/**************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES         */
/* DANIEL REYNA                                   */
/**************************************************/
define buffer stock for stock_historico_tambores.
define buffer bstock for stock_historico_tambores.


define input parameter x1 as integer.
define input parameter x2 as integer.
define input parameter x3 as integer.
define input parameter x4 as integer.
define input parameter pdesde as integer.
define input parameter phasta as integer.
define input parameter xtipo like tipos_movi.id_tipo_movimiento.

define vari tid_sucursal   like tambores_industria.id_sucursal.
define vari tid_empresa    like tambores_industria.id_empresa.
define vari tnromov        like tambores_industria.nromov.
define vari tid_tipotambor like tambores_industria.id_tipotambor.


define vari xxx as character.
define vari vdesde as integer.
define vari vhasta as integer.
define vari nomarch as character.

 
/* MESSAGE x1 " " x2 " " x3 " " x4 " " pdesde " " phasta " " xtipo VIEW-AS ALERT-BOX. */
find tipos_movi where 
    tipos_movi.id_tipo_movimiento = xtipo no-lock no-error.
if not available tipos_movi then do:
    message "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " xtipo
    view-as alert-box.
    return "error".
end.    

nomarch = "{1}".
/* find {1} where 
                   {1}.id_empresa = x1 and 
                   {1}.id_sucursal = x2 and
                   {1}.id_tipotambor = x3 and
                   {1}.nromov = x4 
                   no-lock no-error.
IF NOT AVAILABLE {1} THEN DO:
    MESSAGE "NO EXISTE CABECERA DE " nomarch VIEW-AS ALERT-BOX ERROR.
    RETURN "error".
END.
*/

if pdesde = 0 or phasta = 0 then do:
    message "ERROR en el Desde y/o Hasta de Tambores se anula" skip
            "la generacion de movimientos de Stock"
            view-as alert-box error.
    return "ADM-ERROR".            
end.
define buffer xbuf for tambores_industria.
define vari vrow as rowid.

    vdesde = 0.
    vhasta = 0.                       
    for each tambores_industria where
            tambores_industria.id_empresa    = x1 and
            tambores_industria.id_sucursal   = x2 and
            tambores_industria.id_tipotambor = x3 and
            tambores_industria.nromov        = x4 AND
            tambores_industria.id_tambor     >= pdesde AND
            tambores_industria.id_tambor     <= phasta 
            no-lock 
        break by tambores_industria.id_lote:

        vrow = rowid(tambores_industria).
        find first xbuf where 
         xbuf.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
       AND xbuf.id_sucursal_ubicacion = 4 /* AGREGADO POR ADRIAN EL 21/07/2003 */
       and  xbuf.id_articulo   = tambores_industria.id_articulo and
            xbuf.id_lote   = tambores_industria.id_lote  and
            xbuf.id_tipotambor = tambores_industria.id_tipotambor and
            xbuf.anio = tambores_industria.anio and
            xbuf.id_tambor = tambores_industria.id_tambor and
            rowid(xbuf) <> vrow
            no-lock no-error.
        if available xbuf then do:
            message "Tambor Duplicado para articulo: " xbuf.id_articulo skip
                    "Lote:" xbuf.id_lote 
                    "A¤o:" xbuf.anio "Tambor:" xbuf.id_tambor
                    "Tipo Tambor:" xbuf.id_tipotambor
                    view-as alert-box error.
                    return "ADM-ERROR".
        end.

        if first-of(tambores_industria.id_lote) then 
            assign vdesde = tambores_industria.id_tambor.
        if last-of(tambores_industria.id_lote) then DO:
            assign vhasta = tambores_industria.id_tambor.
            create stock.
            ASSIGN
                    stock.id_articulo        = tambores_industria.id_articulo
                    stock.fecha              = today
                    stock.id_tipo_movimiento = xtipo
                    stock.id_serial          = next-value(serial-stock-tambores) 
                    stock.id_lote            = tambores_industria.id_lote
                    stock.anio               = tambores_industria.anio
                    stock.tambor_desde       = vdesde
                    stock.tambor_hasta       = vhasta
                    stock.id_suc_origen      = IF tambores_industria.id_sucursal_ubicacion = 0 THEN (tambores_industria.id_sucursal * 10) ELSE (tambores_industria.id_sucursal_ubicacion * 10)
                    stock.id_suc_des         = IF tambores_industria.id_sucursal_ubicacion = 0 THEN tambores_industria.id_sucursal ELSE tambores_industria.id_sucursal_ubicacion
                    stock.datos_adicionales  = tipos_movi.descripcion
                    stock.c_usuario          = userid("userdb")
                    stock.c_fecha            = today
                    stock.c_hora             = string(time,"hh:mm:ss")
                    stock.id_empresa         = tambores_industria.id_empresa
                    stock.id_sucursal        = tambores_industria.id_sucursal
                    stock.id_tipotambor      = tambores_industria.id_tipotambor
                    stock.nromov             = tambores_industria.nromov
                    stock.signo              = if tipos_movi.codigo = "+" 
                                                then "-"
                                                else "+"
                    stock.id_envase          = tambores_industria.id_envase
                    stock.id_calidad         = tambores_industria.id_calidad.            
                create bstock.
                buffer-copy stock except stock.id_serial to bstock.
                assign
                    bstock.id_serial         = next-value(serial-stock-tambores)
                    bstock.id_suc_origen      = IF tambores_industria.id_sucursal_ubicacion = 0 THEN tambores_industria.id_sucursal ELSE tambores_industria.id_sucursal_ubicacion 
                    bstock.id_suc_des         = IF tambores_industria.id_sucursal_ubicacion = 0 THEN (tambores_industria.id_sucursal * 10) ELSE (tambores_industria.id_sucursal_ubicacion * 10)
                    bstock.signo              = if tipos_movi.codigo = "+" 
                                                then "+"
                                                else "-".
        END.
        accum tambores_industria.id_tambor (count).
    end.
return.

