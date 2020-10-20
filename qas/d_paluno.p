/****************************************************************************/
/*  NOMBRE PROGRAMA......:   d_paluno.p                                    */
/****************************************************************************/
/*  Permite consultar registros completos                                   */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

DEFINE VAR hlib AS HANDLE NO-UNDO.

{persistentProcedure.i libinterfaces.p hLib}

{ d_paluno.i
 &archivo1            = "pallets"
 &archivo2            = "items_pallets"
 &use_index1          = "use-index suc_orden_item"
 &use_index2          = "use-index items_pallets"
 &otros_archivos1     = ", turnos_packing, colores "
 &archivos_relacion1  = ", first turnos_packing OF pallets no-lock, ~
            FIRST colores OF pallets no-lock "
 &otros_archivos2     = ", proveedores, origenes, lote, aux_colores "
 &archivos_relacion2  = ", FIRST proveedores OF items_pallets no-lock, ~
            FIRST origenes of items_pallets no-lock, ~
            FIRST lote of items_pallets no-lock, ~
            FIRST aux_colores of items_pallets no-lock "
 &with_form_trabajo   = "row 4 side-labels title color white/red "" Consulta de un Determinado Pallet """
 &temp_table          = "yes"

 &variables_busqueda  = "d_paluno.vbu"
 &busqueda            = "d_paluno.bus"
 &campos_display      = "d_palun0.dis"
 &campos_display1     = "d_palun1.dis"
 &campos_display2     = "d_palun2.dis"
 &campos_display_cab  = "d_paluno.dis"  
 &funciones           = "d_paluno.fun"
}

{declaraPalletn.i}
