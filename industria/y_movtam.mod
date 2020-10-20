/* Solo se validan los movimientos que no son ajustes ni StkIni */
if movimientos_tambores.id_tipo_movimiento <> 9 and
   movimientos_tambores.id_tipo_movimiento <> 10 and
   movimientos_tambores.id_tipo_movimiento <> 100 then do:
 /* Validacion de Movimientos de Tambores */
 i = movimientos_tambores.tambor_desde.
 for each tambores_industria where
 tambores_industria.id_sucursal_ubicacion = movimientos_tambores.id_suc_origen
 and tambores_industria.id_lote  = movimientos_tambores.id_lote and
    tambores_industria.anio     = movimientos_tambores.anio and
    tambores_industria.id_articulo = movimientos_tambores.id_articulo and
    tambores_industria.id_tambor   >= movimientos_tambores.tambor_desde and
    tambores_industria.id_tambor   <= movimientos_tambores.tambor_hasta
    no-lock by tambores_industria.id_tambor:
    accum tambores_industria.id_tambor (count).
    if i <> tambores_industria.id_tambor then do:
      message "Existen inconsistencias entre lo solicitado y lo existente" skip
              "El tambor solicitado " i " NO esta disponible"   
      view-as alert-box error.
      undo, retry.                
    end.
    i = i + 1.
 end.    
 if (accum count tambores_industria.id_tambor) <>
   (movimientos_tambores.tambor_hasta -
    movimientos_tambores.tambor_desde + 1) then do:
    message "ATENCION !!! Los Tambores solicitados no se encuentran todos" skip
            "Disponibles, solo existen " 
            (accum count tambores_industria.id_tambor) 
            " Tambores u Ud. solicito"   
            (movimientos_tambores.tambor_hasta -
            movimientos_tambores.tambor_desde + 1) 
            view-as alert-box error.
    undo, retry.
 end.    
end.    

find first tambores_industria where
 tambores_industria.id_sucursal = 82
 and tambores_industria.id_lote  = movimientos_tambores.id_lote and
    tambores_industria.id_tipotambor = movimientos_tambores.id_tipotambor and
    tambores_industria.anio     = movimientos_tambores.anio and
    tambores_industria.id_articulo = movimientos_tambores.id_articulo 
  no-lock no-error.
if not available tambores_industria then do:
 find first tambores_industria where
     tambores_industria.id_sucursal = 96
 and tambores_industria.id_lote  = movimientos_tambores.id_lote and
    tambores_industria.id_tipotambor = movimientos_tambores.id_tipotambor and
    tambores_industria.anio     = movimientos_tambores.anio and
    tambores_industria.id_articulo = movimientos_tambores.id_articulo 
  no-lock no-error.
 if not available tambores_industria then do:
    message "No existe ningun tambor para ese Lote" view-as alert-box.
    undo, retry.
 end.
end.  

update xserial with frame fing.



find tipos_movimientos where 
    tipos_movimientos.id_tipo_movimiento =     movimientos_tambores.id_tipo_movimiento
    no-lock no-error.
   
assign
        movimientos_tambores.c_fecha            = today
        movimientos_tambores.c_hora             = string(time,"hh:mm:ss")
        movimientos_tambores.c_usuario          = userid("userdb")
        movimientos_tambores.datos_adicionales  = tipos_movimientos.descripcion
/*        movimientos_tambores.fecha              = today */
        movimientos_tambores.id_empresa         = tambores_industria.id_empresa
        movimientos_tambores.id_sucursal     = tambores_industria.id_sucursal
        movimientos_tambores.id_tipotambor   = tambores_industria.id_tipotambor          movimientos_tambores.nromov          = tambores_industria.nromov.
        run s_nromov.p (output movimientos_tambores.nromov_mov).
        movimientos_tambores.nro_movimiento = movimientos_tambores.nromov_mov.
        
        message "lsalsal" view-as alert-box.
        /*------ rutina para generacion de Movimiento de Stock --------*/
        /*------ baja stock de sucursal origen                 --------*/                run y_gstkmov.p (input movimientos_tambores.id_empresa,
                         input movimientos_tambores.id_sucursal,
                         input movimientos_tambores.id_tipotambor,
                         input movimientos_tambores.nromov_mov,
                         input movimientos_tambores.id_tipo_movimiento,
                         input movimientos_tambores.fecha,
                         input xserial)
                         "movimientos_tambores".
        if return-value <> "" then do:
            message "Error en la grabacion de Stock Historico" 
            view-as alert-box.
            undo, return.
        end.
   
