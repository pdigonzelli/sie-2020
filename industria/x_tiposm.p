/****************************************************************************/
/*  NOMBRE PROGRAMA......:   x_proter.p                                     */
/****************************************************************************/
/*  PROGRAMADOR..........:   Grupo Sƒuken - Juan Carlos R¡os                */
/*  FECHA CREACION.......:   14/03/95                                       */
/*  REVISION.............:   1.00                                           */
/****************************************************************************/

{ s_cons2.i
  &archivo            = "tipos_movimientos"
  &use_index          = "use-index tipos_movimientos"
  &campo_choose       = "id_tipo_movimiento"
  &maxdown            = "10"
  &with_form_trabajo  = "row 7 title color black/yelow 
  "" Tipos de Movimiento """
  &color_trabajo      = "white/black"
  &color_choose       = "black/white"

  &condicion_where    = "s_nada.whe"
  &variables_busqueda = "s_nada.vbu"
  &busqueda           = "s_nada.bus"
  &parametros         = "x_tiposm.par"
  &captura            = "x_tiposm.cap"
  &form_trabajo       = "x_tiposm.for"
  &proceso_display    = "x_tiposm.dpy"
  &proceso_amplia     = "s_nada.pam"
}
 