/*******************************************************/
/* MOVIMIENTOS DE STOCK DE TAMBORES                    */
/* DANIEL REYNA                                        */
/*******************************************************/

define vari xarticulo as character.
define vari xlote as character.
define vari xanio as character.
define vari vnomsuco like sucursales.nombre.
define vari vnomsucd like sucursales.nombre.
define vari vtipo    like tipos_movi.id_tipo_movimiento.
define vari vtitulo  like tipos_movi.descripcion.
define vari vlsucd   like tipos_movi.suc_des.
define vari vlsuco   like tipos_movi.suc_ori.
define vari i as integer.
define temp-table xsuco
    field id_sucursal like sucursales.id_sucursal
    field nombre like sucursales.nombre.
define temp-table xsucd
    field id_sucursal like sucursales.id_sucursal
    field nombre like sucursales.nombre.
    
    
define vari vopcion as logical 
view-as  radio-set radio-buttons "Si", true, "No",False horizontal.


form
    "Ud. ha elegido producir un Egreso de" skip(1)
    vnomsuco label "Suc.Origen " skip
    vnomsucd label "Suc.Destino" SKIP
    "y su ingreso correspondiente en la Suc.Destino"
    skip(1)
    vopcion label "Elija su Opcion"
       help "<Flechas> Desplaza - <Barra-Enter> Selecciona - <F1> Confirma"
    with frame fopcion side-labels centered row 14 overlay color white/black
    title color black/white " A T E N C I O N ".


define new shared temp-table auxi
    field id_sucursal           like tambores_industria.id_sucursal 
    field id_empresa            like tambores_industria.id_empresa 
    field id_tipotambor         like tambores_industria.id_tipotambor 
    field nromov                like tambores_industria.nromov 
    field id_lote like tambores_industria.id_lote
    field anio like tambores_industria.anio
    field id_articulo like tambores_industria.id_articulo
    field tambores  as integer column-label "Tambores"
    field desde     as integer column-label "Desde"
    field hasta     as integer column-label "Hasta"
    field id_sucursal_ubicacion like tambores_industria.id_sucursal_ubicacion
    field id_sucursal_destino like tambores_industria.id_sucursal_ubicacion
    column-label "Suc.Dest."
    field c_usuario like articulos.c_usuario
    field c_hora like articulos.c_hora
    field c_fecha like articulos.c_fecha
    field marca as logical format "*/ "
    index ind id_articulo anio id_lote.

    
    

    
define variable vsucursal  like sucursales.id_sucursal.
define variable vsucursald like sucursales.id_sucursal.

define variable varticulo like productos_terminado.id_articulo.
define variable vlote     like lotes_jugo.id_lote.
define variable vanio     like lotes_jugo.anio.

form skip(1)   vsucursal 
               label "Sucursal Origen "
               validate(input vsucursal <> 0 and
               can-find(sucursales where sucursales.id_sucursal = vsucursal),
               "Sucursal Inexistente")
               help  "Ingrese sucursal Origen - <F3> Ayuda"
               vnomsuco no-label
               skip(1)   
               vsucursald 
               label "Sucursal Destino"
               validate(input vsucursald <> 0 and
               can-find(sucursales where sucursales.id_sucursal = vsucursald),
               "Sucursal Inexistente")
               help  "Ingrese sucursal Destino - <F3> Ayuda"
               vnomsucd no-label
               
               skip(1)
               varticulo label "Articulo"
               help "Ingrese Codigo de Articulo - 0 Todos - <F3> Ayuda"
               productos_terminados.descripcion no-label                         
               skip(1)
               vlote  label "Lote"
                      help "Ingrese el Lote - 0 Todos"
               skip(1)
               vanio  label "A¤o"
                      help "Ingrese el A¤o - 0 Todos"

                         
     with frame fing side-labels centered 
     overlay row 5 color white/black title color black/white 
     " " + vtitulo + " ".

on f3 of vsucursal do:
    assign vsucursal.
    run x_selsuco (input-output vsucursal).
    find xsuco where xsuco.id_sucursal = vsucursal no-lock no-error.
    if available xsuco then do:
    assign vnomsuco = xsuco.nombre.
    display vsucursal xsuco.nombre @ vnomsuco with frame fing.
    end.    
end.
on return of vsucursal do:
    assign vsucursal.                                                        
    find xsuco where xsuco.id_sucursal = vsucursal no-lock no-error. 
    if available xsuco then do:
        assign vnomsuco = xsuco.nombre.
        display vsucursal xsuco.nombre @ vnomsuco with frame fing.
    end.
    else do:
        message "Sucursal No HABILITADA para el tipo de movimiento"
        view-as alert-box error.
        return no-apply.
    end.                
end.

on f3 of vsucursald do:
    assign vsucursald.
    run x_selsucd (input-output vsucursald).
    find xsucd where xsucd.id_sucursal = vsucursald no-lock no-error.
    if available xsucd then do:
    assign vnomsucd = xsucd.nombre.
    display vsucursald xsucd.nombre @ vnomsucd with frame fing.
    end.
end.

on return of vsucursald do:
    assign vsucursald.                                                        
    find xsucd where xsucd.id_sucursal = vsucursald no-lock no-error.
    if available xsucd then do:
    assign vnomsucd = xsucd.nombre.
    display vsucursald xsucd.nombre @ vnomsucd with frame fing.
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

DEFINE QUERY q FOR tipos_movi SCROLLING. 
DEFINE BROWSE BBROWSE QUERY q
DISPLAY  tipos_movi.id_tipo_movimiento column-label "Tipo"
         tipos_movi.descripcion        column-label "Movimiento"
         WITH 5 DOWN  CENTERED color white/black
         TITLE " Movimientos de Stock ". 
         
DEFINE FRAME TRABAJO
     BBROWSE HELP "Elija su Opcion"
     WITH no-box centered row 5 color white/black.

on return of browse bbrowse do:
    for each xsuco. delete xsuco. end. for each xsucd. delete xsucd. end.
    assign vtipo   = tipos_movi.id_tipo_movimiento
           vtitulo = tipos_movi.descripcion
           vlsuco  = tipos_movi.suc_ori
           vlsucd  = tipos_movi.suc_des.
    do i = 1 to num-entries(vlsuco):
        find sucursales where sucursales.id_sucursal = integer(entry(i,vlsuco))
        no-lock no-error.
        if available sucursales then do:
        create xsuco.
        assign xsuco.id_sucursal = integer(entry(i,vlsuco))
               xsuco.nombre      = sucursales.nombre.
        end.
    end.               
    if num-entries(vlsuco) = 1 then vsucursal = integer(entry(1,vlsuco)).
    
    do i = 1 to num-entries(vlsucd):
        find sucursales where sucursales.id_sucursal = integer(entry(i,vlsucd))
        no-lock no-error.
        if available sucursales then do:
        create xsucd.
        assign xsucd.id_sucursal = integer(entry(i,vlsucd))
               xsucd.nombre      = sucursales.nombre.
        end.               
    end.    
    if num-entries(vlsucd) = 1 then vsucursald = integer(entry(1,vlsucd)).
    
    apply "window-close" to bbrowse in frame trabajo.
end.
         

OPEN QUERY q for each tipos_movi where tipos_movi.ajustes no-lock.
ENABLE BBROWSE WITH FRAME TRABAJO OVERLAY.
APPLY "ENTRY" TO BBROWSE.
APPLY "VALUE-CHANGED" TO BBROWSE.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


update vsucursal 
       vsucursald
       varticulo
       vlote
       vanio with frame fing.     
if varticulo = 0  then xarticulo = "*". else xarticulo = string(varticulo).
if vlote = 0      then xlote = "*". else xlote = string(vlote).
if vanio = 0      then xanio = "*". else xanio = string(vanio).


for each lotes_jugo where
  can-do(xarticulo,string(lotes_jugo.id_articulo)) and
  can-do(xlote,string(lotes_jugo.id_lote)) and
  can-do(xanio,string(lotes_jugo.anio)) no-lock.
  find first tambores_industria of lotes_jugo where
          tambores_industria.id_articulo           = lotes_jugo.id_articulo and
          tambores_industria.anio                  = lotes_jugo.anio and
          tambores_industria.id_sucursal_ubicacion = vsucursal
          no-lock no-error.
  if available tambores_industria then do:          
    find sucursales where 
        sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion
        no-lock no-error.
    create auxi.
    assign
    auxi.id_sucursal           = tambores_industria.id_sucursal 
    auxi.id_empresa            = tambores_industria.id_empresa 
    auxi.id_tipotambor         = tambores_industria.id_tipotambor 
    auxi.nromov                = tambores_industria.nromov 
    auxi.id_lote               = tambores_industria.id_lote
    auxi.anio                  = tambores_industria.anio
    auxi.id_articulo           = tambores_industria.id_articulo
    auxi.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
    auxi.id_sucursal_destino   = 0.

    display tambores_industria.id_lote
            tambores_industria.anio
            tambores_industria.id_articulo skip
            tambores_industria.id_sucursal_ubicacion
            sucursales.nombre format "x(20)"
            with frame fproc 1 down side-labels centered row 10 overlay
            color black/white.
    pause 0.            
  end.    
end.
hide frame fproc.
for each auxi.
   for each tambores_industria  where
          tambores_industria.id_sucursal           = auxi.id_sucursal and
          tambores_industria.id_empresa            = auxi.id_empresa and
          tambores_industria.id_tipotambor         = auxi.id_tipotambor and
          tambores_industria.nromov                = auxi.nromov and
          tambores_industria.anio                  = auxi.anio and
          tambores_industria.id_sucursal_ubicacion = vsucursal
          no-lock
          break by tambores_industria.id_lote:
        display tambores_industria.id_lote 
                tambores_industria.anio
                tambores_industria.id_tambor
                with frame fproc1 side-labels centered overlay row 10 1 down
                color black/white.
        pause 0.
                  
        if first-of(tambores_industria.id_lote) then 
            assign auxi.desde = tambores_industria.id_tambor.
        if last-of(tambores_industria.id_lote) then 
            assign auxi.hasta = tambores_industria.id_tambor.
          
        accum tambores_industria.id_tambor (count).
    end.          
    assign auxi.tambores = auxi.hasta - auxi.desde  + 1.
end.
hide frame fproc1.

/*----- SELECCION DE LOTES PARA SU MOVIMIENTO --------*/
repeat:
    run y_movstk.p.
    find first auxi where auxi.marca no-error.
    if not available auxi then do:
        message " No se ha seleccionado ningun Lote " view-as alert-box error.
        leave.
    end.

    for each auxi where auxi.marca.
        display auxi.id_lote
                auxi.anio
                auxi.id_sucursal_ubicacion
                auxi.desde 
                auxi.hasta
                auxi.tambores
        with frame fdetalle 5 down scrollable  color black/white   
        centered row 5 
        title color white/black "Detalle de Lotes Seleccionados".
    end.
    display vnomsuco vnomsucd with frame fopcion. pause 0.
    update vopcion with frame fopcion.
    leave.
end.    
if vopcion then do:
    for each auxi where auxi.marca:
        /*---- Creacion de Movimientos de Stock -----*/
        create movimientos_tambores.
        assign
        movimientos_tambores.anio               = auxi.anio
        movimientos_tambores.c_fecha            = today
        movimientos_tambores.c_hora             = string(time,"hh:mm:ss")
        movimientos_tambores.c_usuario          = userid("userdb")
        movimientos_tambores.datos_adicionales  = "Mov.Stock"
        movimientos_tambores.fecha              = today
        movimientos_tambores.id_articulo        = auxi.id_articulo
        movimientos_tambores.id_empresa         = auxi.id_empresa
        movimientos_tambores.id_lote            = auxi.id_lote
        movimientos_tambores.id_sucursal        = auxi.id_sucursal
        movimientos_tambores.id_suc_origen      = vsucursal
        movimientos_tambores.id_suc_des         = vsucursald
        movimientos_tambores.id_tipotambor      = auxi.id_tipotambor                     movimientos_tambores.id_tipo_movimiento = vtipo
        movimientos_tambores.nromov             = auxi.nromov
        movimientos_tambores.tambor_desde       = auxi.desde
        movimientos_tambores.tambor_hasta       = auxi.hasta.
        run s_nromov.p (output movimientos_tambores.nromov_mov).
        
        
        /*------ rutina para generacion de Movimiento de Stock --------*/
        /*------ baja stock de sucursal origen                 --------*/               run y_gstkmov.p (input movimientos_tambores.id_empresa,
                          input movimientos_tambores.id_sucursal,
                          input movimientos_tambores.id_tipotambor,
                          input movimientos_tambores.nromov_mov,
                          input vtipo)
                          "movimientos_tambores".
        if return-value <> "" then do:
            message "Error en la grabacion de Stock Historico" 
            view-as alert-box.
            undo, return.
        end.
        
    /* comentado hasta que se implemente
        for each tambores_industria where 
                tambores_industria.id_sucursal = auxi.id_sucursal and
                tambores_industria.id_empresa = auxi.id_empresa and
                tambores_industria.id_tipotambor = auxi.id_tipotambor and
                tambores_industria.nromov = auxi.nromov and
                tambores_industria.desde  >= auxi.desde and
                tambores_industria.hasta  <= auxi.hasta:
            assign tambores_industria.id_sucursal_ubicacion = vsucursald.                end.                
     */           
    end.
end.


procedure x_selsuco.
define input-output parameter vsucursal as integer.
DEFINE QUERY q1 FOR xsuco SCROLLING. 
DEFINE BROWSE B1 QUERY q1
DISPLAY  xsuco.id_sucursal
         xsuco.nombre
         WITH 5 DOWN  CENTERED color black/white
         TITLE " Sucursales Origen ". 
         
DEFINE FRAME TRABAJO1
     B1 HELP "Elija su Opcion"
     WITH no-box centered row 5 color black/white.

on return of browse b1 do:
    vsucursal = xsuco.id_sucursal.
    apply "window-close" to b1 in frame trabajo1.
end.
         

OPEN QUERY q1 for each xsuco no-lock.
ENABLE B1 WITH FRAME TRABAJO1 OVERLAY.
APPLY "ENTRY" TO B1.
APPLY "VALUE-CHANGED" TO B1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
end.

procedure x_selsucd.
define input-output parameter vsucursald as integer.
DEFINE QUERY q1 FOR xsucd SCROLLING. 
DEFINE BROWSE B1 QUERY q1
DISPLAY  xsucd.id_sucursal
         xsucd.nombre
         WITH 5 DOWN  CENTERED color black/white
         TITLE " Sucursales Destino ". 
         
DEFINE FRAME TRABAJO1
     B1 HELP "Elija su Opcion"
     WITH no-box centered row 5 color black/white.

on return of browse b1 do:
    vsucursald = xsucd.id_sucursal.
    apply "window-close" to b1 in frame trabajo1.
end.
         

OPEN QUERY q1 for each xsucd no-lock.
ENABLE B1 WITH FRAME TRABAJO1 OVERLAY.
APPLY "ENTRY" TO B1.
APPLY "VALUE-CHANGED" TO B1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
end.