/********************************************/
/* STOCK A FECHA DE ARTICULOS               */
/* AUTOR: DANIEL REYNA                      */
/********************************************/


define buffer bstock for stock_historico_tambores.
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
    field c_usuario   like bstock.c_usuario
    field cantidad    as decimal format ">>>>,>>>,>>9"
    field cantidad_400 as decimal
    field nomcalidad     like calidades.descripcion
    index ind is unique id_articulo id_lote anio.
    
define vari varticulo like bstock.id_articulo.
define vari vid_lote  like bstock.id_lote.
define vari vanio     like bstock.anio.
define vari vfecha    as date.
define vari vsucursal like bstock.id_sucursal.
define vari vlote like bstock.id_lote.
define vari vano like bstock.anio.
define vari vnomsuco like sucursales.nombre.
define vari vdetalle  as logical format "Si/No".


form               bstock.id_serial format ">>>>>>>9"
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
                   vsaldo  column-label "Acumul" format "->>>9"
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
    vsucursal label "Sucursal"  help "Ingrese Cod.Sucursal <F3> Ayuda"
    vnomsuco no-label skip(1)
    varticulo label "Cod.Articulo"  help "Ingrese Cod.Articulo <F3> Ayuda"
    productos_terminados.descripcion no-label  skip(1)
    vlote   label "Lote" help "Ingrese Nro.Lote para seguimiento"
    vano   label "A¤o"  help "Ingrese el A¤o del Lote"
    vfecha    label "Stock a " help "Ingrese Fecha Tope"
    vdetalle  label "Detallado?" help "Detallado? <S> o <N>"
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

vfecha = today.

update vsucursal 
       varticulo
       vlote
       vano
       vfecha
       vdetalle
       with frame fing.
find productos_terminados where
     productos_terminados.id_articulo = varticulo
     no-lock no-error.
find sucursales where sucursales.id_sucursal = vsucursal 
     no-lock no-error. 
assign vnomsuco = sucursales.nombre.

{s_device1.i}

{s_priini.i
   &id_formulario = "STKFEC"
   &heading       = "y_stock_fecha.hea"
   &footing       = "s_nada.foo"
}

for each productos_terminados where 
    productos_terminados.id_tipo_articulo = 2 and
    if varticulo = 0 then true 
                     else productos_terminados.id_articulo = varticulo
    no-lock.
 assign varticulo = productos_terminados.id_articulo.    
 assign vsaldo = 0 v_cantidad_t = 0 v_cantidad_400_t = 0.
 for each auxi. delete auxi. end.

 for each bstock use-index serial_lote
        where bstock.id_articulo    = varticulo and
              bstock.id_suc_origen  = vsucursal and
              bstock.fecha          <= vfecha and
              if vlote <> 0 then bstock.id_lote = vlote and
                                 bstock.anio    = vano
                            else true
              no-lock
              break by bstock.id_articulo
                    by bstock.id_lote
                    by bstock.anio:
/*        displa 
        bstock.id_articulo 
        bstock.fecha
        bstock.id_lote 
        bstock.anio 
        bstock.id_serial 
/*        bstock.datos_adicionales   */
        bstock.id_suc_origen 
        bstock.id_suc_des         
        bstock.signo column-label "S"  
        bstock.tambor_desde column-label "Desde"
        bstock.tambor_hasta column-label "Hasta"
        bstock.id_tipo_movimiento  column-label "Tipo"
        bstock.id_empresa 
        bstock.id_sucursal 
        bstock.id_tipotambor  
        bstock.nromov
        with frame fproc 1 down scrollable row 10 overlay.
        pause 0.
        */
        assign cnt_tmb = bstock.tambor_hasta - bstock.tambor_desde + 1.
        assign v_cantidad = 0 v_cantidad_400 = 0 vnomcalidad = "" .

        run determina_datos (input bstock.id_empresa,
                             input bstock.id_sucursal,
                             input bstock.id_tipotambor,
                             input bstock.nromov,
                             input bstock.tambor_desde,
                             input bstock.tambor_hasta,
                             input cnt_tmb).
                
        find auxi where auxi.id_articulo = bstock.id_articulo and
                        auxi.id_lote     = bstock.id_lote and
                        auxi.anio        = bstock.anio
                        no-error.
        if not available auxi then create auxi.
        assign 
               auxi.id_empresa   = bstock.id_empresa
               auxi.id_sucursal  = bstock.id_sucursal
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
                       v_cantidad_t      = v_cantidad_t + v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t + v_cantidad_400.
        if bstock.signo = "-" then 
                assign auxi.cant_tmb     = auxi.cant_tmb - cnt_tmb
                       v_cantidad_t      = v_cantidad_t - v_cantidad
                       v_cantidad_400_t  = v_cantidad_400_t - v_cantidad_400.
        if bstock.signo = "B" then 
                assign auxi.cant_tmb     = cnt_tmb
                       v_cantidad_t      = v_cantidad
                       v_cantidad_400_t  = v_cantidad_400.
                
                
                
        if vdetalle then do:               
           assign vdebe = 0 vhaber = 0.
           if bstock.signo = "+" then assign vdebe  = cnt_tmb
                                             vsaldo = vsaldo + cnt_tmb
                                             vsaldo_t = vsaldo_t + cnt_tmb.
           if bstock.signo = "-" then assign vhaber  = cnt_tmb
                                             vsaldo = vsaldo - cnt_tmb
                                             vsaldo_t = vsaldo_t - cnt_tmb.
           if bstock.signo = "B" then assign vdebe  = cnt_tmb
                                             vsaldo = cnt_tmb
                                             vsaldo_t = cnt_tmb.

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
                                             
           /* -------------- */                                              
           display bstock.id_serial 
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
                   v_cantidad 
                   v_cantidad_400 
                   vnomcalidad
                   auxi.c_usuario 
                   with frame fdetalle.
            down with frame fdetalle.                   
                                   
           if last-of(bstock.anio) then do:
                down with frame fdetalle.
                display "---->" @ tipos_movimientos.alta 
                        vsaldo 
                with frame fdetalle.
                down with frame fdetalle.
                assign vsaldo_t = 0.
           end.
                                                    
        end.            
                
        
 end.
hide frame fproc.

for each auxi /* where auxi.cant_tmb <> 0 */ .
    assign v_cantidad = 0 v_cantidad_400 = 0 .
    run determina_datos (input auxi.id_empresa,
                         input auxi.id_sucursal,
                         input auxi.id_tipotambor,
                         input auxi.nromov,
                         input 1,
                         input 9999999,
                         input auxi.cant_tmb).
    assign auxi.cantidad = v_cantidad
           auxi.cantidad_400 = v_cantidad_400.                                
    
    display auxi.id_articulo
            auxi.id_lote
            auxi.anio
            auxi.fecha
            auxi.cant_tmb column-label "Cant.Tamb." (total)
            auxi.cantidad column-label "Kg.Nominales"
            auxi.cantidad_400 column-label "Kg. 400 gpl"
            auxi.nomcalidad  column-label "Calidad"
            with scrollable centered overlay
            color black/white title             
            string(auxi.id_articulo) + " - " +
            upper(productos_terminados.descripcion) + " -----> Suc: " +
            vnomsuco.
end.
end.
{s_editar.i &titulo = 'Listado'}    




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


procedure determina_datos.
define input parameter vempresa    like bstock.id_empresa.
define input parameter vsucursal   like bstock.id_sucursal.
define input parameter vtipotambor like bstock.id_tipotambor.
define input parameter vnromov     like bstock.nromov.
define input parameter vdesde      as integer.
define input parameter vhasta      as integer.
define input parameter vcant       as integer.

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
                
/*                display tambores_industria.id_lote tambores_industria.id_tambor
                kilos_tambor
                accum total tambores_industria.kilos_tambor. */
                
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
define input parameter  vcant as integer.
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
    r_productos_calidad.id_articulo = varticulo  AND        r_productos_calidad.id_calidad  = vcalidad 
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
