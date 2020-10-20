/*********************************************/
/* CARGA DE AJUSTES DE TAMBORES INDUSTRIALES */
/* AUTOR: DANIEL REYNA                       */
/*********************************************/

define vari xserial as DECIMAL FORMAT ">>>>>>>>>>9".

form
    xserial label "Serial"
    with frame fing side-labels centered.
    
define variable v_ok_lote    as logical initial true.
define variable v_ok_deslote as logical initial true.
define variable v_ok_haslote as logical initial true.
define variable v_suctambor  as integer.
define variable v_kilos_tambor like tambores_industria.kilos_tambor.
define variable v_nromov       like tambores_industria.nromov initial 0.
define variable v_id_calidad      like items_factura.id_calidad.
define variable v_id_envase       like items_factura.id_envase.
define variable v_id_ordenentrega like remitos.id_orden_entrega.
define vari i as integer.

{ s_abmc3.i
 &programa           = "y_movtam.p"
 &archivo            = "movimientos_tambores"
 &use_index          = "use-index principal"
 &campo_choose       = "id_lote"
 &maxdown            = "9"
 &with_form_trabajo  = "row 5 
  title color blue/yellow "" MOVIMIENTOS DE STOCK DE TAMBORES INDUSTRIALES """
 &color_trabajo      = "yellow/blue"
 &color_choose       = "message"
 &puede_modificar    = "no"
 &temp_table         = "no"

 &condicion_where    = "y_movtam.whe"
 &variables_busqueda = "s_nada.vbu"
 &form_trabajo       = "y_movtam.for"
 &form_relacionadas  = "s_nada.fre"
 &proceso_display    = "y_movtam.dpy"
 &campos_update      = "y_movtam.upd"
 &busqueda           = "s_nada.bus"
 &relaciones         = "y_movtam.rel"
 &borrado            = "y_movtam.del"
 &creacion           = "s_nada.alt"
 &modificacion       = "y_movtam.mod"
 &campos_create      = "y_movtam.for"
 &mensaje            = "s_nada.mes"
 &teclas_validas     = "s_nada.tec"
 &funciones          = "s_nada.fun"
}
