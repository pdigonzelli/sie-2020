/************************************************************/
/* PROGRAMA DEMO DE UN  ABM CON BROWSE EN FORMA GRAFICA CON */
/* LIBRERIA NUEVA                                           */
/************************************************************/
define shared temp-table auxi
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

{s_abmbrw.i                             
    &tablas-query = "auxi"  
    &tabla        = "auxi"
    &baja-logica = "no"
    &defvari     = "s_nada.for"
    &condeach    = "y_movstk.ech"

    &dispbrow    = "y_movstk.dib"
    &posibrow    = "no-box centered row 5 color white/black"
    &cposform    = "y_movstk.for"
    &with-form   = "centered overlay color black/white 
                    title color white/black 
                   ""Destino de Tambores"" row 15  side-labels"
    &dispform    = "y_movstk.dis"
    &cposupda    = "y_movstk.upa"       
    &cposupdm    = "y_movstk.upm"   
    &ampliainf   = "s_nada.for"
    &precreate   = "y_movstk.ppd" 
    &poscreate   = "s_nada.for"
    &preupdate   = "y_movstk.pru"
    &posupdate   = "s_nada.for"
    &prepredel   = "y_movstk.ppd"
    &predelete   = "s_nada.for"
    &posdelete   = "s_nada.for"       
    &helpbrow    = "y_movstk.hlp"
    &mensaje     = "y_movstk.mes"
    &eventos     = "y_movstk.eve"
}
