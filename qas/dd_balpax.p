/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_balpax.p                                    */
/****************************************************************************/
/*  Permite agregar y modificar registros completos (abmc tipo-5)           */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

session:time-source = "produccion". 

{ s_balpax.i
 &archivo             = "balanza_tickets"
 &use_index           = "use-index balanza_pesada_ticket"
 &otros_archivos1     = ", proveedores, origenes, lote "
 &archivos_relacion1  = ", FIRST proveedores OF balanza_tickets no-lock, ~
            FIRST origenes where ~
            balanza_tickets.id_proveedor = origenes.id_proveedor and ~
            balanza_tickets.id_origen = origenes.id_origen no-lock, ~
            FIRST lote where ~
            balanza_tickets.id_proveedor = lote.id_proveedor and ~
            balanza_tickets.id_origen = lote.id_origen and ~
            balanza_tickets.id_lote = lote.id_lote no-lock "
 &otros_archivos2     = ", productos_terminados, variedades, colores, tipo_cosecha, ~
            tipos_servicios, envases_prod "
 &archivos_relacion2  = ", FIRST productos_terminados where ~
            productos_terminados.id_articulo = balanza_tickets.id_materia_prima and ~
            productos_terminados.id_tipo_articulo = 1 no-lock, ~
            FIRST variedades OF balanza_tickets no-lock, ~
            FIRST colores OF balanza_tickets no-lock, ~
            FIRST tipo_cosecha OF balanza_tickets no-lock, ~
            FIRST tipos_servicios OF balanza_tickets no-lock, ~
            FIRST envases_prod of balanza_tickets where ~
            envases_prod.produccion = false no-lock "
 &campo_choose        = "id_proveedor"
 &maxdown             = "3"
 &with_form_trabajo   = "row 4 side-labels title color white/red "" Ingreso de Fruta a Packing Lavalle (Cabina) """
 &color_trabajo       = "white/green"
 &color_choose        = "white/red"
 &puede_modificar     = "yes"
 &archivo_cabecera    = "balanza_pesadas"
 &use_index_cabecera  = "use-index balanza_pesada"
 &condicion_where_cab = "where balanza_pesadas.id_balanza = x_balanza and ~
            balanza_pesadas.id_pesada < 9000000"

 &condicion_where     = "dd_baldex.whe"
 &variables_busqueda  = "dd_baldex.vbu"
 &proceso_display     = "dd_baldex.dpy"
 &campos_display      = "dd_baldex.dis"
 &campos_display1     = "dd_baldx1.dis"
 &campos_display2     = "dd_baldx2.dis"
 &relaciones          = "dd_baldex.rel"
 &borrado             = "dd_baldex.del"
 &borrado_posterior   = "dd_baldx1.del"
 &creacion            = "dd_baldex.alt"
 &campos_create       = "dd_baldex.cre"
 &modificacion        = "dd_baldex.mod"
 &proceso_display_cab = "dd_balcax.dpy"
 &campos_display_cab  = "dd_balcax.dis"  
 &campos_update_cab   = "dd_balcax.upd"
 &busqueda_cabecera   = "dd_balcax.bus"
 &relaciones_cabecera = "dd_balcax.rel"
 &borrado_cabecera    = "dd_balcax.del"
 &creacion_cabecera   = "dd_balcax.alt"
 &campos_create_cab   = "dd_balcax.cre"
 &modificacion_cab    = "dd_balcax.mod"
 &funciones           = "dd_balcax.fun"
}
