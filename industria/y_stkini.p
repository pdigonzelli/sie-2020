/***************************************************/
/* PROGRAMA DE GENERACION DE STOCK INICIAL         */
/***************************************************/

define vari vlisart as character.
define vari vsuc AS INTEGER.
define vari i as integer.
define buffer btam for tambores_industria.
define temp-table auxi 
        field id_lote   as integer
        field id_tambor as integer
        field numero    as integer
        index ind id_tambor.
define buffer x for auxi.        
define vari vinicio as integer.
define vari vfin as integer.



i = 1.

assign vlisart = "52,53,66,71,70,50,51,512,513,514,517,518,519,520,41,57,58,61,74,76,761,762,763".

/* produccion de jugo 952,953,42,43,        */
/* Falta productos de tercero */
/* falta el articulo 48 materia_prima de aceite */

    /*
 {s_device1.i}

{s_priini.i
   &id_formulario = "STKFEC"
   &heading       = "s_nada.hea"
   &footing       = "s_nada.foo"
}
      */
      
vsuc = 86.
      
for each tipostambor where id_tipotambor = 3 no-lock:
for each productos_terminados where 
  (productos_terminados.id_articulo = 52 OR productos_terminados.id_articulo = 53) AND
 productos_terminados.id_tipo_articulo = 2   no-lock:
 for each btam where 
    btam.id_sucursal_ubicacion = vsuc and                                     
    btam.id_locacion_ubicacion = 4 and  
    btam.id_articulo           = productos_terminados.id_articulo and
    btam.id_tipotambor     = tipostambor.id_tipotambor /*  and
    btam.id_lote = 521 and btam.anio = 2003              */
         NO-LOCK with frame xx down
         BREAK BY btam.id_tipotambor
               BY btam.anio
               BY btam.id_lote
               BY btam.nromov
               BY btam.id_tambor.
    if first-of(btam.nromov) then do:
        for each auxi. delete auxi. end.
        i = 1.
    end.
    create auxi.
    assign auxi.id_lote   = btam.id_lote
           auxi.id_tambor = btam.id_tambor
           auxi.numero    = i 
           i              = i + 1.
    if last-of(btam.nromov) then do:
        for each auxi break by auxi.id_lote . 
            if first-of(auxi.id_lote) then vinicio = auxi.id_tambor.
            find x where x.numero = auxi.numero + 1 no-error.
            if available x then do:
             if auxi.id_tambor + 1 <> x.id_tambor then  vfin = auxi.id_tambor.
            end.                
            else vfin = auxi.id_tambor.
        end.
     display btam.id_articulo               format ">>>9" column-label "Art"
  /*              productos_terminados.descripcion format "x(20)" */
                btam.id_tipotambor column-label "T" format "9"
                tipostambor.descripcion
                btam.id_lote format ">>>9"
                btam.anio 
                btam.fecha_cierre
                btam.id_calidad
    /*            vinicio        format ">>>9"
                vfin           format ">>>9" */
                vfin - vinicio + 1 (total)  format ">>>9"
                btam.c_fecha
                btam.c_hora
                btam.c_usuario
                with scrollable.
              
        run genera_movimiento.                 
     end.
 end.
end.
end.

/*  {s_editar.i &titulo = 'Listado'}    
  */



procedure genera_movimiento.
 /*---- Creacion de Movimientos de Stock -----*/
        create movimientos_tambores.
        assign
        movimientos_tambores.anio               = btam.anio
        movimientos_tambores.c_fecha            = today
        movimientos_tambores.c_hora             = string(time,"hh:mm:ss")
        movimientos_tambores.c_usuario          = userid("userdb")
        movimientos_tambores.datos_adicionales  = "STOCK INICIAL"
        movimientos_tambores.fecha              = today
        movimientos_tambores.id_articulo        = btam.id_articulo
        movimientos_tambores.id_empresa         = btam.id_empresa
        movimientos_tambores.id_lote            = btam.id_lote
        movimientos_tambores.id_sucursal        = btam.id_sucursal
        movimientos_tambores.id_suc_origen      = vsuc
        movimientos_tambores.id_suc_des         = vsuc
        movimientos_tambores.id_tipotambor      = btam.id_tipotambor
        movimientos_tambores.id_tipo_movimiento = 100
        movimientos_tambores.nromov             = btam.nromov
        movimientos_tambores.tambor_desde       = vinicio
        movimientos_tambores.tambor_hasta       = vfin.
        run s_nromov.p (output movimientos_tambores.nro_movimiento).
        run s_nromov.p (output movimientos_tambores.nromov_mov).
        /*------ rutina para generacion de Movimiento de Stock --------*/
        /*------ baja stock de sucursal origen                 --------*/             
        run y_gstkmov.p (input movimientos_tambores.id_empresa,
                        input movimientos_tambores.id_sucursal,
                        input movimientos_tambores.id_tipotambor,
                        input movimientos_tambores.nromov_mov,
                        input 100,
                        INPUT TODAY,
                        INPUT 0)
                        "movimientos_tambores".
                        
        if return-value <> "" then do:
            message "Error en la grabacion de Stock Historico" 
            view-as alert-box.
            undo, return.
        end.
end.
