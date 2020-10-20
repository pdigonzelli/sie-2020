/********************************************/
/* STOCK A FECHA DE ARTICULOS               */
/* AUTOR: DANIEL REYNA                      */
/********************************************/
define buffer bstock for stock_historico_tambores.
DEFINE VARI vlotedestino AS CHARACTER FORMAT "x(20)".
define input parameter vtipo like tipos_movimientos.id_tipo_movimiento. 
define input parameter vsucursal like bstock.id_sucursal.
define input parameter varticulo like bstock.id_articulo.
define input parameter vlote like bstock.id_lote.
define input parameter vano like bstock.anio.
define input parameter vdesde as date.
define input parameter vfecha as date.
define input parameter vdetalle  as logical.
define input parameter vstock0 as logical .

define vari vtipod like tipos_movimientos.id_tipo_movimiento format ">>>9".
define vari vtipoh like tipos_movimientos.id_tipo_movimiento format ">>>9".
define vari vcontrol   as integer.

define vari vnomtipo like tipos_movimientos.descripcion.
define vari vez as logical.
define vari cnt_tmb as integer.
define vari vdebe  as integer.
define vari vhaber as integer.
define vari vsaldo as integer.
define vari vsaldo_t as integer.
define vari v_cantidad          as decimal.
define vari v_cantidad_400      as decimal.
define vari v_cantidad_t        as decimal.
define vari v_cantidad_400_t    as decimal.
define vari vnomcalidad         like calidades.descripcion. 
define vari verror as logical.
define vari vdes like bstock.id_suc_des.
define vari vori like bstock.id_suc_origen.
define vari lissuc as character initial
"73,CHESTE,74,PHOENI,75,USCOLD,76,MIDFLO,
 77,ALLENT,78,PREFER,79,GREGOR,80,LANTER,
 81,ALLTMP,83,VANBON,84,EXT.BV,85,DESPAC,
 86,HIWA  ,87,METAN ,88,COOLQU,89,SWIFF ,
 91,FACCLI,95,FAMAIL,96,LAVALL,960,PRDLAV,950,PRDFAM".
define vari vnomori as character format "x(6)".
define vari vnomdes as character format "x(6)".
define vari vnomaux as character format "x(6)".





define temp-table auxi
    field id_empresa  like bstock.id_empresa
    field id_sucursal like bstock.id_sucursal
    field id_tipotambor like bstock.id_tipotambor
    field nromov      like bstock.nromov
    field id_articulo like bstock.id_articulo
    field id_lote     like bstock.id_lote
    field anio        like bstock.anio
    field fecha       like bstock.fecha
    field cant_tmb    as integer
    field desde       as integer
    field hasta       as integer
    field c_usuario   like bstock.c_usuario
    field cantidad    as decimal format "->>>>,>>>,>>9"
    field cantidad_400 as decimal  format "->>>>,>>>,>>9"
    field nomcalidad     like calidades.descripcion
    field estado      as integer format ">9" 
    index ind is unique id_articulo id_tipotambor id_lote anio.

define vari vestado as integer.    
define vari xestado as character format "x(10)".
    
define vari vid_lote  like bstock.id_lote.
define vari vanio     like bstock.anio.
define vari vnomsuco like sucursales.nombre.


form               bstock.id_serial format ">>>>>>>9"
                   bstock.id_tipotambor
                   bstock.id_lote format ">>>>9"
                   bstock.anio
                   bstock.fecha
                   tipos_movimientos.alta format "x(6)" column-label "MOVSTK"
                   vori    column-label "Suc"   format ">>9"
                   vnomori column-label "Origen" 
                   vdes    column-label "Suc"   format ">>9"
                   vnomdes column-label "Destin"
                   vdebe   column-label "Ing." 
                   vhaber  column-label "Egr."  
                   vsaldo_t column-label "Stock"  format "->>>>9"
/*                   vsaldo  column-label "Acumul" format "->>>9" */
                   bstock.tambor_desde 
                   bstock.tambor_hasta
                   v_cantidad column-label "Kg.Nomin."
                   v_cantidad_400 column-label "Kg.400 gpl"
                   vnomcalidad column-label "Calidad" format "x(24)"
                   auxi.c_usuario 
                   with frame fdetalle down scrollable centered overlay
                        color white/black title 
                        string(bstock.id_articulo) + "-" +
                        upper(productos_terminados.descripcion) + 
                        " -----> Suc: " +
                        vnomsuco row 5.


form 
    vtipo     label "Tipo de Movimiento" 
              help "Ingrese Tipo Movimiento - <F3> Ayuda - 0 Todos" 
    vnomtipo  no-label skip(1)
    vsucursal label "Sucursal"  help "Ingrese Cod.Sucursal <F3> Ayuda"
    vnomsuco no-label skip(1)
    varticulo label "Cod.Articulo"  help "Ingrese Cod.Articulo <F3> Ayuda"
    productos_terminados.descripcion no-label  skip(1)
    vlote   label "Lote" help "Ingrese Nro.Lote para seguimiento"
    vano   label "A¤o"  help "Ingrese el A¤o del Lote" skip(1)
    vdesde    label "Movimientos desde" 
        help "Ingrese fecha desde la cual quiere ver los movimientos"
    vfecha    label "Stock a " help "Ingrese Fecha Tope" skip(1)
    vdetalle  label "Detallado?" help "Detallado? <S> o <N>"
    vstock0   label "Visualiza Stock en 0" help "Stock en Cero <S> o <N>"
    with frame fing color white/black 
    title color black/white " Stock a Fecha "
    side-labels centered overlay row 5.

on f3 of vsucursal do:
    assign vsucursal.
    run x_selsuco (input-output vsucursal).
    find sucursales where sucursales.id_sucursal = vsucursal no-lock no-error.
    if available sucursales then do:
    assign vnomsuco = sucursales.nombre.
    display vsucursal sucursales.nombre @ vnomsuco with frame fing.
    end.    
end.
on return of vsucursal do:
    assign vsucursal.                                                        
    find sucursales where sucursales.id_sucursal = vsucursal no-lock no-error. 
    if available sucursales then do:
        assign vnomsuco = sucursales.nombre.
        display vsucursal sucursales.nombre @ vnomsuco with frame fing.
    end.
    else do:
        message "Sucursal No HABILITADA para el tipo de movimiento"
        view-as alert-box error.
        return no-apply.
    end.                
end.
on f3 of vtipo do:
    assign vtipo.
    run x_seltipo (input-output vtipo).
    find tipos_movimientos where 
        tipos_movimientos.id_tipo_movimiento = vtipo
        no-lock no-error.
    if available tipos_movimientos then do:
    assign vnomtipo = tipos_movimientos.descripcion.
    display vtipo vnomtipo with frame fing.
    end.    
end.
on return of vtipo do:
    assign vtipo.
    find tipos_movimientos where 
        tipos_movimientos.id_tipo_movimiento = vtipo
        no-lock no-error.
    if available tipos_movimientos then do:
    assign vnomtipo = tipos_movimientos.descripcion.
    display vtipo vnomtipo with frame fing.
    end.    
    else do:
        if vtipo = 0 then display "TODOS" @ vnomtipo with frame fing.
        else do:
        message "ATENCION Tipo de Movimiento Inexistente" view-as alert-box.
        return no-apply.
        end.
    end.

end.



on f3 of varticulo do:
    assign varticulo.
    run x_proter.p (input-output varticulo).
    find productos_terminados where
        productos_terminados.id_articulo = varticulo
        no-lock no-error.
            
    display varticulo productos_terminados.descripcion with frame fing.
end.

on return of varticulo do:
    assign varticulo.
    if varticulo <> 0 then do:
    find productos_terminados where
        productos_terminados.id_articulo = varticulo
        no-lock no-error.
    display varticulo productos_terminados.descripcion with frame fing.
    end.
    else display "TODOS" @ productos_terminados.descripcion with frame fing.
end.
/* vdesde = date("01/05/03").
vfecha = today.
*/
/* 
update vtipo
       vsucursal 
       varticulo
       vlote
       vano
       vdesde
       vfecha
       vdetalle
       vstock0
       with frame fing.
   */
find productos_terminados where
     productos_terminados.id_articulo = varticulo
     no-lock no-error.
find sucursales where sucursales.id_sucursal = vsucursal 
     no-lock no-error. 
assign vnomsuco = sucursales.nombre.

/*
{s_device1.i}

{s_priini.i
   &id_formulario = "STKFEC"
   &heading       = "y_stock_fecha.hea"
   &footing       = "s_nada.foo"
}
*/
for each productos_terminados where 
    productos_terminados.id_tipo_articulo = 2 and
    if varticulo = 0 then true 
                     else productos_terminados.id_articulo = varticulo
    no-lock.
 assign varticulo = productos_terminados.id_articulo.    
 assign vsaldo = 0 v_cantidad_t = 0 v_cantidad_400_t = 0.
 for each auxi. delete auxi. end.
 if vtipo = 0 then assign vtipod  = 1 
                   vtipoh         = 999999999 
                   vdesde         = date("01/05/03").
              else assign vtipod = vtipo vtipoh = vtipo.
/* message varticulo "kdlahd" vsucursal vtipod vtipoh vdesde vfecha 
 vlote vano view-as alert-box. */
 for each bstock use-index serial_lote
        where bstock.id_articulo    = varticulo and
              bstock.id_suc_origen  = vsucursal and
              bstock.id_tipo_movimiento >= vtipod   and
              bstock.id_tipo_movimiento <= vtipoh and
              bstock.fecha          >= vdesde and
              bstock.fecha          <= vfecha and
              bstock.tambor_hasta   <> 0 and
              bstock.tambor_desde   <> 0 and
              if vlote <> 0 then bstock.id_lote = vlote and
                                 bstock.anio    = vano
                            else true
              no-lock
              break 
                    by bstock.id_articulo
                    by bstock.id_tipotambor
                    by bstock.id_lote
                    by bstock.anio
                    by bstock.id_serial:
       displa bstock.id_articulo bstock.fecha with frame fproc side-labels
       centered view-as dialog-box three-d title "Procesando".
       pause 0.
       verror = false.
       assign v_cantidad = 0 v_cantidad_400 = 0 vnomcalidad = "" .
       run determina_datos (input bstock.id_empresa,
                            input bstock.id_sucursal,
                            input bstock.id_tipotambor,
                            input bstock.nromov,
                            input bstock.tambor_desde,
                            input bstock.tambor_hasta,
                            input cnt_tmb,
                            output verror).
       if not verror then do:
        assign cnt_tmb = bstock.tambor_hasta - bstock.tambor_desde + 1.
        find auxi where auxi.id_articulo = bstock.id_articulo and
                        auxi.id_tipotambor = bstock.id_tipotambor and
                        auxi.id_lote     = bstock.id_lote and
                        auxi.anio        = bstock.anio
                        no-error.
        if not available auxi then create auxi.
        assign 
               auxi.id_empresa   = bstock.id_empresa
               auxi.id_sucursal  = bstock.id_suc_origen
               auxi.id_tipotambor = bstock.id_tipotambor
               auxi.nromov       = bstock.nromov
               auxi.id_articulo  = bstock.id_articulo 
               auxi.id_lote      = bstock.id_lote 
               auxi.anio         = bstock.anio
               auxi.fecha        = bstock.fecha
               auxi.c_usuario    = bstock.c_usuario
               auxi.nomcalidad   = vnomcalidad.
               
        if bstock.signo = "+" then 
                assign auxi.cant_tmb     = auxi.cant_tmb + cnt_tmb
                       auxi.cantidad     = auxi.cantidad + v_cantidad
                       auxi.cantidad_400 = auxi.cantidad_400 + v_cantidad_400
                       v_cantidad_t      = v_cantidad_t + v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t + v_cantidad_400.
        if bstock.signo = "-" then 
                assign auxi.cant_tmb     = auxi.cant_tmb - cnt_tmb
                       auxi.cantidad     = auxi.cantidad - v_cantidad
                       auxi.cantidad_400 = auxi.cantidad_400 - v_cantidad_400
                       v_cantidad_t      = v_cantidad_t - v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t - v_cantidad_400.
        if bstock.signo = "B" /* and
           bstock.id_tipotambor = 2 */ then do: 
                assign auxi.cant_tmb     = auxi.cant_tmb + cnt_tmb
                       auxi.cantidad     = auxi.cantidad + v_cantidad
                       auxi.cantidad_400 = auxi.cantidad_400 + v_cantidad_400
                       v_cantidad_t      = v_cantidad_t + v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t + v_cantidad_400.
        end.                                         
/*        if bstock.signo = "B" and
           bstock.id_tipotambor <> 2 then do:
            if bstock.c_usuario = "INISYS" then 
                assign auxi.cant_tmb     = auxi.cant_tmb + cnt_tmb
                       auxi.cantidad     = auxi.cantidad + v_cantidad
                       auxi.cantidad_400 = auxi.cantidad_400 + v_cantidad_400
                       v_cantidad_t      = v_cantidad_t + v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t + v_cantidad_400.
            else
                assign auxi.cant_tmb     = cnt_tmb
                       auxi.cantidad     = v_cantidad
                       auxi.cantidad_400 = v_cantidad_400
                       v_cantidad_t      = v_cantidad
                       v_cantidad_400_t  = v_cantidad_400.
        end.                       
  */              
                
                
        if vdetalle then do:               
           if first-of(bstock.id_lote) then vez = true.        
           if bstock.fecha >= vdesde and vez then do:
                /* ver saldo inicial cacha
                 display "Inicial" @ tipos_movimientos.alta
                        vsaldo_t 
                        with frame fdetalle.
                down with frame fdetalle.                        
                */
                vez = false.
           end.
        
           assign vdebe = 0 vhaber = 0.
           if bstock.signo = "+" then assign vdebe  = cnt_tmb
                                             vsaldo = vsaldo + cnt_tmb
                                             vsaldo_t = vsaldo_t + cnt_tmb.
           if bstock.signo = "-" then assign vhaber  = cnt_tmb
                                             vsaldo = vsaldo - cnt_tmb
                                             vsaldo_t = vsaldo_t - cnt_tmb.
           if bstock.signo = "B" 
/*              bstock.id_tipotambor = 2 */ then  
                                      assign vdebe  = cnt_tmb
                                             vsaldo = vsaldo + cnt_tmb
                                             vsaldo_t = vsaldo_t + cnt_tmb.
/*           if bstock.signo = "B" and
              bstock.id_tipotambor <> 2 then 
                                      assign vdebe  = cnt_tmb
                                             vsaldo = cnt_tmb
                                             vsaldo_t = cnt_tmb.
  */
           find tipos_movimientos of bstock no-lock no-error.                  
           /* Determina Abraviatura de Sucursales */
           if lookup(string(bstock.id_suc_origen),lissuc) <> 0 then 
            assign vnomori =
            entry(lookup(string(bstock.id_suc_origen),lissuc) + 1,lissuc).
           else do:
               find sucursales where 
                    sucursales.id_sucursal = bstock.id_suc_origen
                    no-lock no-error.
               assign vnomori = sucursales.nombre.               
           end.           
           if lookup(string(bstock.id_suc_des),lissuc) <> 0 then
            assign vnomdes =
            entry(lookup(string(bstock.id_suc_des),lissuc) + 1,lissuc).
           else do:
               find sucursales where 
                    sucursales.id_sucursal = bstock.id_suc_des
                    no-lock no-error.
               assign vnomdes = sucursales.nombre.               
           end.           
           if bstock.signo = "+" then assign vori = bstock.id_suc_des
                                             vdes = bstock.id_suc_origen
                                             vnomaux = vnomdes
                                             vnomdes = vnomori
                                             vnomori = vnomaux.
           if bstock.signo = "-" then assign vdes = bstock.id_suc_des
                                             vori = bstock.id_suc_origen.
           if bstock.signo = "B" then assign vdes = bstock.id_suc_origen
                                             vori = bstock.id_suc_origen
                                             vnomdes = vnomori.
                                             
           if bstock.fecha >= vdesde  then do:
           /* -------------- */
           vlotedestino = "".
           IF bstock.id_tipo_movimiento = 16 THEN /* REPROCESOS */
           RUN CONTROL_reprocesos.


           find tipostambor of bstock no-lock no-error.
           create repstock.
           assign repstock.id_articulo    = bstock.id_articulo
                  repstock.nomart         = productos_terminados.descripcion
                  repstock.id_serial      = bstock.id_serial
                  repstock.id_tipotambor  = bstock.id_tipotambor
                  repstock.nomtambor = tipostambor.descripcion
                  repstock.id_lote        = bstock.id_lote
                  repstock.anio           = bstock.anio
                  repstock.fecha          = bstock.fecha
                  repstock.nommov         = tipos_movimientos.alta
                  repstock.origen         = vori
                  repstock.nomorigen      = vnomori
                  repstock.destino        = vdes
                  repstock.nomdestino     = vnomdes
                  repstock.debe           = vdebe
                  repstock.haber          = vhaber
                  repstock.saldo          = vsaldo_t
                  repstock.tambor_desde   = bstock.tambor_desde
                  repstock.tambor_hasta   = bstock.tambor_hasta
                  repstock.cantidad       = v_cantidad * (IF bstock.signo = "+"  THEN 1 ELSE -1)
                  repstock.cantidad_400   = v_cantidad_400 * (IF bstock.signo = "+"  THEN 1 ELSE -1)
                  repstock.nomcalidad     = vnomcalidad
                  repstock.usuario        = bstock.c_usuario
                  repstock.datos_adicional = vlotedestino.
/*                display bstock.id_serial 
                   bstock.id_tipotambor
                   tipostambor.descripcion
                   bstock.id_lote
                   bstock.anio
                   bstock.fecha
                   tipos_movimientos.alta 
                   vori 
                   vnomori 
                   vdes 
                   vnomdes 
                   vdebe   when vdebe <> 0 format ">>>9"
                   vhaber  when vhaber <> 0 format ">>>9"
                   vsaldo_t when vsaldo_t <> 0
                   bstock.tambor_desde column-label "Desde"
                   bstock.tambor_hasta column-label "Hasta"
                   v_cantidad 
                   v_cantidad_400 
                   vnomcalidad
                   auxi.c_usuario  
                   with frame fdetalle.
            down with frame fdetalle.                   */
            end.
                                   
           if last-of(bstock.id_lote) then do:
/*                down with frame fdetalle.
                display "----" @ tipos_movimientos.alta 
/*                        vsaldo  */
                with frame fdetalle.
                down with frame fdetalle. */
                assign vsaldo_t = 0.
           end.
                                                    
        end.            
       end.        /* de verror. */
        
 end.
hide frame fproc.
for each auxi where if not vstock0 then auxi.cant_tmb <> 0 else true 
   break by auxi.id_articulo.
   assign v_cantidad = 0 v_cantidad_400 = 0 vestado = 99.
   assign verror = false.
/*   run determina_datos (input auxi.id_empresa,
                         input auxi.id_sucursal,
                         input auxi.id_tipotambor,
                         input auxi.nromov,
                         input auxi.desde,
                         input auxi.hasta,
                         input auxi.cant_tmb,
                         output verror).
   */                      
   if not verror then do:
   
    assign auxi.estado = vestado
           xestado     = "".                         
    find estados_lotes where estados_lotes.id_estado_lote = vestado
    no-lock no-error.    
    if available estados_lote then xestado = estados_lote.descripcion.
    /* CONTROL DE STOCK EN tambores_industria */
    vcontrol = 0.
    run auditoria       (input auxi.id_empresa,
                         input auxi.id_sucursal,
                         input auxi.id_tipotambor,
                         input auxi.nromov,
                         input auxi.id_lote,
                         input auxi.anio,
                         input auxi.id_articulo,
                         input auxi.desde,
                         input auxi.hasta,
                         input auxi.cant_tmb,
                         output vcontrol,
                         output verror).
    accum auxi.cant_tmb (total by auxi.id_articulo).
    find tipostambor of auxi no-lock no-error.
    vlotedestino = "".
    IF bstock.id_tipo_movimiento = 16 THEN /* REPROCESOS */
           RUN CONTROL_reprocesos.
    create repstockr.
    assign repstockr.id_articulo = auxi.id_articulo
           repstockr.nomart      = productos_terminados.descripcion
           repstockr.id_tipotambor = auxi.id_tipotambor
           repstockr.nomtambor     = tipostambor.descripcion
           repstockr.id_lote       = auxi.id_lote
           repstockr.anio          = auxi.anio
           repstockr.fecha         = auxi.fecha
           repstockr.cant_tmb      = auxi.cant_tmb 
           repstockr.vcontrol       = vcontrol
           repstockr.estado        = xestado
           repstockr.cantidad      = auxi.cantidad
           repstockr.cantidad_400  = auxi.cantidad_400
           repstockr.nomcalidad    = auxi.nomcalidad
           repstockr.datos_adicional = vlotedestino.
/*    display auxi.id_articulo
            productos_terminados.descripcion
            auxi.id_tipotambor column-label "Tipo"
            tipostambor.abreviatura column-label "Tipo de Tambor"
            auxi.id_lote format ">>>>>9"
            auxi.anio
            auxi.fecha
            auxi.cant_tmb column-label "Tamb."  format ">>>9"
                   (total by auxi.id_articulo)  
            vcontrol column-label "Control" format ">>>9" 
                    (total by auxi.id_articulo)
            xestado  column-label "Estado"
            auxi.cantidad column-label "Kg.Nom."
                    (total by auxi.id_articulo) format ">>>>,>>9"
            auxi.cantidad_400 column-label "Kg. 400 gpl"
                    (total by auxi.id_articulo) format ">>>>,>>9"
            auxi.nomcalidad  column-label "Calidad" format "x(20)"
            with frame fresumen down scrollable centered overlay
            color black/white .             */
   end. /* del verror */            
/*   if last-of(auxi.id_articulo) then do:
/*       down with frame fresumen.
    display accum total by auxi.id_articulo auxi.cant_tmb @
        auxi.cant_tmb with frame fresumen. */
   end.        
   */
end.
end.

/* {s_editar.i &titulo = 'Listado'}    
   */


procedure determina_datos.
define input parameter vempresa    like bstock.id_empresa.
define input parameter vsucursal   like bstock.id_sucursal.
define input parameter vtipotambor like bstock.id_tipotambor.
define input parameter vnromov     like bstock.nromov.
define input parameter vdesde      as integer.
define input parameter vhasta      as integer.
define input parameter vcant       as integer.
define output parameter verror     as logical.

find movimientos_tambores where 
        movimientos_tambores.id_empresa = vempresa and
        movimientos_tambores.id_sucursal = vsucursal and
        movimientos_tambores.id_tipotambor = vtipotambor and
        movimientos_tambores.nromov_mov = vnromov
        no-lock no-error.
if available movimientos_tambores 
    then assign vnromov = movimientos_tambores.nromov.        
for each tambores_industria where
                tambores_industria.id_empresa    = vempresa and
                tambores_industria.id_sucursal   = vsucursal and
                tambores_industria.id_tipotambor = vtipotambor and
                tambores_industria.nromov        = vnromov and
                tambores_industria.id_tambor    >= vdesde and
                tambores_industria.id_tambor    <= vhasta  
                no-lock.
                accum tambores_industria.kilos_tambor (total).
                
/*              display tambores_industria.id_lote tambores_industria.id_tambor
                kilos_tambor tambores_industria.nromov
                accum total tambores_industria.kilos_tambor.  */
                
end.                
    
find first tambores_industria where
            tambores_industria.id_empresa    = vempresa and
            tambores_industria.id_sucursal   = vsucursal and
            tambores_industria.id_tipotambor = vtipotambor and
            tambores_industria.nromov        = vnromov
            no-lock no-error.
        if not available tambores_industria then do:
/*            message "Atencion Inconsistencia" view-as alert-box.
            next. */
        end.
        else do:
        vestado = 99.                             
        if tambores_industria.id_tipotambor = 3 then do: /* tambores de jugo */
           find lotes_jugo of tambores_industria no-lock no-error.
           if not available lotes_jugo then do:
            message "Atencion Inconsistencia en Lote Jugo para " skip
                    "Articulo: " tambores_industria.id_articulo
                    "Lote:" tambores_industria.id_lote 
                    " A¤o:" tambores_industria.anio
                    " Tipo Tbor: " tambores_industria.id_tipotambor
                    view-as alert-box error.
            assign verror = true.
            return.
           end.
           assign vestado = lotes_jugo.estado_lote when available lotes_jugo.
        end.
        if tambores_industria.id_tipotambor = 6 then do: /* tamb de aceite */
           find lotes_aceite of tambores_industria no-lock no-error.
           if not available lotes_aceite then do:
           message "Atencion Inconsistencia en Lote Aceite para " skip
                    "Articulo: " tambores_industria.id_articulo
                   "Lote:" tambores_industria.id_lote 
                   " A¤o:" tambores_industria.anio
                   " Tipo Tbor: " tambores_industria.id_tipotambor
                   view-as alert-box error.
           assign verror = true.
            return.
           end.
           assign vestado = lotes_aceite.estado_lote 
           when available lotes_aceite.
        end.                  
        if tambores_industria.id_tipotambor = 4 then do: /* sobrante */
           find sobrante where
           tambores_industria.id_empresa    = sobrante.id_empresa and          ~            tambores_industria.id_sucursal   = sobrante.id_sucursal and        ~              tambores_industria.id_tipotambor = sobrante.id_tipotambor_sobrante
           and tambores_industria.nromov  = sobrante.nromov_sobrante
           no-lock no-error.
           if not available sobrante then do:
           message "Atencion Inconsistencia en Sobrante para " skip
                    "Articulo: " tambores_industria.id_articulo
                   "Lote:" tambores_industria.id_lote 
                   " A¤o:" tambores_industria.anio
                   " Tipo Tbor: " tambores_industria.id_tipotambor
                   view-as alert-box error.
           assign verror = true.
            return.
           end.
           assign vestado = lotes_aceite.estado_lote 
           when available lotes_aceite.
        end.
        
            find calidades where 
                 calidades.id_calidad = tambores_industria.id_calidad
                 no-lock no-error.
            assign vnomcalidad = calidades.descripcion 
                                    when available calidades. 

            
            run datos_extra (input tambores_industria.id_calidad,
                             input tambores_industria.id_envase,
                             input tambores_industria.id_articulo,
                             input vcant,
                       input (accum total tambores_industria.kilos_tambor)).
        end.                             
end.

procedure datos_extra.
define input parameter vcalidad  like tambores_industria.id_calidad.
define input parameter venvase   like tambores_industria.id_envase.
define input parameter varticulo like tambores_industria.id_articulo.
define input parameter vcant as integer.
define input parameter vkgtmb    like tambores_industria.kilos_tambor.


define vari v_kilos_envase      like r_productos_calidad_envase.kilos.
define vari v_coef              as decimal.
define vari v_pesoref           as decimal.

FIND r_productos_calidad_envase where
        r_productos_calidad_envase.id_articulo = varticulo and
        r_productos_calidad_envase.id_calidad = vcalidad and
        r_productos_calidad_envase.id_envase = venvase
        NO-LOCK NO-ERROR.
        
IF AVAILABLE r_productos_calidad_envase 
THEN v_kilos_envase = r_productos_calidad_envase.kilos.
ELSE v_kilos_envase = 250.

if vkgtmb = 0 then assign v_cantidad = vcant * v_kilos_envase.
              else assign v_cantidad = vkgtmb.
IF varticulo = 52 OR
   varticulo = 53 THEN DO:
   FIND r_productos_calidad WHERE 
    r_productos_calidad.id_articulo = varticulo  AND        
    r_productos_calidad.id_calidad  = vcalidad 
   NO-LOCK NO-ERROR.  
   IF AVAILABLE r_productos_calidad 
    THEN v_coef = r_productos_calidad.coeficiente.
    ELSE v_coef = 1.

END.
ELSE v_coef = 1.
              
IF v_coef >= 1.25 THEN v_pesoref = 260.
                  ELSE v_pesoref = 250.

if vkgtmb = 0 then assign v_cantidad_400 = vcant * v_kilos_envase * v_coef.
              else assign v_cantidad_400 = vkgtmb * v_coef.


end.


procedure x_selsuco.
define input-output parameter vsucursal as integer.
DEFINE QUERY q1 FOR sucursales SCROLLING. 
DEFINE BROWSE B1 QUERY q1
 DISPLAY  sucursales.id_sucursal
         sucursales.nombre
         WITH 5 DOWN  CENTERED color black/white
         TITLE " Sucursales Origen ". 
         
DEFINE FRAME TRABAJO1
     B1 HELP "Elija su Opcion"
     WITH no-box centered row 5 color black/white.

on return of browse b1 do:
    vsucursal = sucursales.id_sucursal.
    apply "window-close" to b1 in frame trabajo1.
end.
         

OPEN QUERY q1 for each sucursales no-lock.
ENABLE B1 WITH FRAME TRABAJO1 OVERLAY.
APPLY "ENTRY" TO B1.
APPLY "VALUE-CHANGED" TO B1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
end.

procedure x_seltipo.
define input-output parameter vtipo as integer.
DEFINE QUERY q1 FOR tipos_movimientos SCROLLING. 
DEFINE BROWSE B1 QUERY q1
 DISPLAY  tipos_movimientos.id_tipo_movimiento
         tipos_movimientos.descripcion
         WITH 5 DOWN  CENTERED color black/white
         TITLE " Tipos de Movimiento ". 
         
DEFINE FRAME TRABAJO1
     B1 HELP "Elija su Opcion"
     WITH no-box centered row 5 color black/white.

on return of browse b1 do:
    vtipo = tipos_movimientos.id_tipo_movimiento.
    apply "window-close" to b1 in frame trabajo1.
end.
         

OPEN QUERY q1 for each tipos_movimientos no-lock.
ENABLE B1 WITH FRAME TRABAJO1 OVERLAY.
APPLY "ENTRY" TO B1.
APPLY "VALUE-CHANGED" TO B1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
end.



procedure auditoria.
define input parameter vempresa    like bstock.id_empresa.
define input parameter vsucursal   like bstock.id_sucursal.
define input parameter vtipotambor like bstock.id_tipotambor.
define input parameter vnromov     like bstock.nromov.
define input parameter vlote       like bstock.id_lote.
define input parameter vanio       like bstock.anio.
define input parameter varticulo   like bstock.id_articulo.
define input parameter vdesde      as integer.
define input parameter vhasta      as integer.
define input parameter vcant       as integer.
define output parameter vcontrol   as integer.
define output parameter verror     as logical.
/*
find movimientos_tambores where 
        movimientos_tambores.id_empresa = vempresa and
        movimientos_tambores.id_sucursal = vsucursal and
        movimientos_tambores.id_tipotambor = vtipotambor and
        movimientos_tambores.nromov_mov = vnromov
        no-lock no-error.
if available movimientos_tambores 
    then assign vnromov = movimientos_tambores.nromov.        
    */
    /* message vsucursal view-as alert-box. */
for each tambores_industria where
                tambores_industria.id_sucursal_ubicacion   = vsucursal and
                tambores_industria.id_articulo   = varticulo and
                tambores_industria.id_tipotambor = vtipotambor and
                tambores_industria.id_lote       = vlote and
                tambores_industria.anio          = vanio 
             /* tambores_industria.id_tambor    >= vdesde and
                tambores_industria.id_tambor    <= vhasta  */
                no-lock.
                if tambores_industria.id_locacion_ubicacion = 4 then
                vcontrol = vcontrol + 1.
                
/*              display tambores_industria.id_lote tambores_industria.id_tambor
                kilos_tambor tambores_industria.nromov
                accum total tambores_industria.kilos_tambor. 
  */              
end.
    
find first tambores_industria where
            tambores_industria.id_empresa    = vempresa and
            tambores_industria.id_sucursal   = vsucursal and
            tambores_industria.id_tipotambor = vtipotambor and
            tambores_industria.nromov        = vnromov
            no-lock no-error.
        if not available tambores_industria then do:
/*            message "Atencion Inconsistencia" view-as alert-box.
            next. */
        end.
        else do:
        vestado = 99.                             
        if tambores_industria.id_tipotambor = 3 then do: /* tambores de jugo */
           find lotes_jugo of tambores_industria no-lock no-error.
           if not available lotes_jugo then do:
            message "Atencion Inconsistencia en Lote Jugo para " skip
                    "Articulo: " tambores_industria.id_articulo
                    "Lote:" tambores_industria.id_lote 
                    " A¤o:" tambores_industria.anio
                    " Tipo Tbor: " tambores_industria.id_tipotambor
                    view-as alert-box error.
            assign verror = true.
            return.
           end.
           assign vestado = lotes_jugo.estado_lote when available lotes_jugo.
        end.
        if tambores_industria.id_tipotambor = 6 then do: /* tamb de aceite */
           find lotes_aceite of tambores_industria no-lock no-error.
           if not available lotes_aceite then do:
           message "Atencion Inconsistencia en Lote Aceite para " skip
                    "Articulo: " tambores_industria.id_articulo
                   "Lote:" tambores_industria.id_lote 
                   " A¤o:" tambores_industria.anio
                   " Tipo Tbor: " tambores_industria.id_tipotambor
                   view-as alert-box error.
           assign verror = true.
            return.
           end.
           assign vestado = lotes_aceite.estado_lote 
           when available lotes_aceite.
        end.                  
        if tambores_industria.id_tipotambor = 4 then do: /* sobrante */
           find sobrante where
           tambores_industria.id_empresa    = sobrante.id_empresa and          ~           tambores_industria.id_sucursal   = sobrante.id_sucursal and         ~             tambores_industria.id_tipotambor = sobrante.id_tipotambor_sobrante
           and tambores_industria.nromov  = sobrante.nromov_sobrante
           no-lock no-error.
           if not available sobrante then do:
           message "Atencion Inconsistencia en Sobrante para " skip
                    "Articulo: " tambores_industria.id_articulo
                   "Lote:" tambores_industria.id_lote 
                   " A¤o:" tambores_industria.anio
                   " Tipo Tbor: " tambores_industria.id_tipotambor
                   view-as alert-box error.
           assign verror = true.
            return.
           end.
           assign vestado = lotes_aceite.estado_lote 
           when available lotes_aceite.
        end.
       end.
end.


PROCEDURE CONTROL_reprocesos.
define buffer y for tambores_industria.

find first tambores_industria where
                tambores_industria.id_sucursal = bstock.id_sucursal and
                tambores_industria.id_empresa  = bstock.id_empresa and
                tambores_industria.id_tipotambor = bstock.id_tipotambor and
                tambores_industria.nromov = bstock.nromov
                no-lock no-error.
IF available tambores_industria and
   tambores_industria.nromov_destino <> 0 then do:
   find first y  where
              y.id_sucursal     = tambores_industria.id_sucursal_destino and
              y.id_empresa      = tambores_industria.id_empresa_destino and
              y.id_tipotambor   = tambores_industria.id_tipotambor_destino and
              y.nromov          = tambores_industria.nromov_destino                
              no-lock no-error.
    IF AVAILABLE Y THEN DO:
            vlotedestino = string(Y.id_lote) + "/" + STRING(Y.anio,"9999") + "-" + STRING(Y.id_articulo).
    END.
END.
END.
