/****************************************************************************/
/*  NOMBRE PROGRAMA......:   infocliente.p                                  */
/****************************************************************************/

define variable x_fecha_des     as date format "99/99/99".
define variable x_fecha_has     as date format "99/99/99".
define variable x_cod_vap       as integer.

/*
set
    x_fecha_des
    x_fecha_has
    x_cod_vap.

message
    x_fecha_des
    x_fecha_has
    x_cod_vap
    view-as alert-box.
  */
define temp-table x_puerto
    field id_cliente                as integer    format  ">>>,>>9"
    field razonsocialCliente        as character  format  "X(18)"
    field id_vapor                  as integer    format  ">,>>9"
    field id_sucursal               as integer    format  ">>9"
    field id_pallet                 as integer    format  ">>,>>>,>>9"
    field descripcionVapor          as character  format  "X(18)"
    field id_punto_emisor           as integer    format  ">9"
    field descripcionPtoEmisor      as character  format  "X(18)"
    field abreviaturaPtoEmisor      as character  format  "X(18)"
    field id_aduana                 as integer    format  ">>9"
    field descripcionAduana         as character  format  "X(18)"
    field id_destino                as integer    format  ">>>9"
    field id_destino_grupo          as integer    format  ">>9"
    field id_destino_packing        as integer    format  ">>9"
    field descripcionDestino        as character  format  "X(18)"
    field abreviaturaDestino        as character  format  "X(18)"
    field id_tipo_esquinero         as integer    format  ">>9"
    field descripcionTipoEsquinero  as character  format  "X(18)"
    field abreviaturaTipoEsquinero  as character  format  "X(18)"
    field id_variedad               as integer    format  ">>9"
    field descripcionVariedad       as character  format  "X(18)"
    field abreviaturaVariedad       as character  format  "X(18)"
    field descripcionInglesVariedad as character  format  "X(18)"
    field abreviaturaInglesVariedad as character  format  "X(18)"
    field id_marca                  as integer    format  ">>9"
    field descripcionMarca          as character  format  "X(18)"
    field abreviaturaMarca          as character  format  "X(18)"
    field descripcionInglesMarca    as character  format  "X(18)"
    field abreviaturaInglesMarca    as character  format  "X(18)"
    field id_envase                 as integer    format  ">>9"
    field descripcionEnvase         as character  format  "X(18)"
    field abreviaturaEnvase         as character  format  "X(18)"
    field descripcionInglesEnvase   as character  format  "X(18)"
    field abreviaturaInglesEnvase   as character  format  "X(18)"
    field id_caract                 as integer    format  ">>9"
    field descripcionCaract         as character  format  "X(18)"
    field abreviaturaCarac          as character  format  "X(18)"
    field id_articulo               as integer    format  ">>>>>>>>9"
    field descripcionProducto       as character  format  "X(18)"
    field descripcionInglesProducto as character  format  "X(18)"
    field id_sucursalDestino        as integer    format  ">>9"
    field id_orden                  as integer    format  ">>,>>>,>>9"
    field id_programa_despacho      as integer    format  ">>,>>>,>>9"
    field id_permiso_embarque       as character  format  "X(13)"
    field item_remito               as integer    format  ">>9"
    field id_empresa                as integer    format  ">>9"
    field bultos                    as integer    format  ">>,>>9"
    field bodega                    as character  format  "X(6)"
    field id_orden_embarque         as character  format  "X(13)"
    field contenedor                as character  format  "X(14)"
    field precinto                  as integer    format  ">,>>>,>>9"
    field termografo                as character  format  "X(8)"
    field item                      as integer    format  ">>9"
    field nro_comprobante           as character  format  "XXXX-XXXXXXXX"
    field fecha_emb                 as date       format  "99/99/99"
    field anulado                   as logical    initial false
    field pallet_senasa             as character  format  "X(18)"
    field precinto_embarque         as character  format  "X(15)"
    field anio                      as integer    format  "9999"
    field calibre                   as character  format  "XXX/X"
    field Tara                      as decimal    format  ">>>9.99"
    field pallets                   as integer    format  ">>9"
    field produccion                as logical    initial false
    field envase_packing            as logical    initial false
    field cant_bolsas               as integer    format  "->,>>>,>>9"
    field volume                    as character  format  "X(30)"
    field estado                    as logical    initial true
    field costo                     as decimal    format  ">,>>>,>>9.999"
    field kilos                     as decimal    format  ">>>9.99"
    field fecha_baja                as date       format  "99/99/99"
    field unidad_distribucion       as integer    format  ">,>>>,>>9"
    field unidad_almacen            as integer    format  ">,>>>,>>9"
    field nombre_bmp                as character  format  "X(30)"
    field estadoProdTerm            as logical    initial true
    field mercado                   as integer    format  "9"
    field sailing                   as integer    format  "->,>>>,>>9"
    field nombre                    as character  format  "X(30)".


/** Carga Archivo Temporario **/

x_fecha_des = DATE('01/01/03').
X_fecha_has  = TODAY.


for each pallets_puerto no-lock where
    pallets_puerto.fecha_emb >= x_fecha_des and
    pallets_puerto.fecha_emb <= x_fecha_has /*and
    pallets_puerto.id_vapor = x_cod_vap*/:

    find first pallets of pallets_puerto no-lock no-error.

    if available pallets then do:

        find first pedidos_packing of pallets no-lock no-error.
        find first clientes of pallets_puerto no-lock no-error.
        find first vapores of pallets_puerto no-lock no-error.
        find first puntos_emisores of pallets no-lock no-error.
        find first aduanas of pallets_puerto no-lock no-error.
        find first destinos of pallets_puerto no-lock no-error.
        find first tipo_esquineros of pallets no-lock no-error.
        find first variedades of pallets no-lock no-error.
        find first marcas_prod of pallets no-lock no-error.
        find first envases_prod of pallets no-lock no-error.
        find first caracteristicas of pallets no-lock no-error.
        find first productos_terminados of pallets no-lock no-error.

        create x_puerto.
        assign
            x_puerto.id_cliente                 = pallets_puerto.id_cliente
            x_puerto.razonsocialCliente         = if available clientes then
                                                  clientes.razon_social else ""
            x_puerto.id_vapor                   = pallets_puerto.id_vapor
            x_puerto.id_sucursal                = pallets_puerto.id_sucursal
            x_puerto.id_pallet                  = pallets_puerto.id_pallet
            x_puerto.descripcionVapor           = if available vapores then
                                                  vapores.descripcion else ""
            x_puerto.id_punto_emisor            = pallets.id_punto_emisor
            x_puerto.descripcionPtoEmisor       = if available puntos_emisores then
                                                  puntos_emisores.descripcion else ""
            x_puerto.abreviaturaPtoEmisor       = if available puntos_emisores then
                                                  puntos_emisores.abreviatura else ""
            x_puerto.id_aduana                  = pallets_puerto.id_aduana
            x_puerto.descripcionAduana          = if available aduanas and
                                                  pallets_puerto.id_aduana <> 0 then
                                                  aduanas.descripcion else ""
            x_puerto.id_destino                 = pallets_puerto.id_destino
            x_puerto.id_destino_packing         = pallets_puerto.id_destino_packing
            x_puerto.descripcionDestino         = if available destinos then
                                                  destinos.descripcion else ""
            x_puerto.abreviaturaDestino         = if available destinos then
                                                  destinos.abreviatura else ""
            x_puerto.id_tipo_esquinero          = pallets.id_tipo_esquinero
            x_puerto.descripcionTipoEsquinero   = if available tipo_esquineros then
                                                  tipo_esquineros.descripcion else ""
            x_puerto.abreviaturaTipoEsquinero   = if available tipo_esquineros then
                                                  tipo_esquineros.abreviatura else ""
            x_puerto.id_variedad                = pallets.id_variedad
            x_puerto.descripcionVariedad        = if available variedades then
                                                  variedades.descripcion else ""
            x_puerto.abreviaturaVariedad        = if available variedades then
                                                  variedades.abreviatura else ""
            x_puerto.descripcionInglesVariedad  = if available variedades then
                                                  variedades.descripcion_ingles else ""
            x_puerto.abreviaturaInglesVariedad  = if available variedades then
                                                  variedades.abreviatura_ingles else ""
            x_puerto.id_marca                   = pallets.id_marca
            x_puerto.descripcionMarca           = if available marcas_prod then
                                                  marcas_prod.descripcion else ""
            x_puerto.abreviaturaMarca           = if available marcas_prod then
                                                  marcas_prod.abreviatura else ""
            x_puerto.descripcionInglesMarca     = if available marcas_prod then
                                                  marcas_prod.descripcion_ingles else ""
            x_puerto.abreviaturaInglesMarca     = if available marcas_prod then
                                                  marcas_prod.abreviatura_ingles else ""
            x_puerto.id_envase                  = pallets.id_envase
            x_puerto.descripcionEnvase          = if available envases_prod then
                                                  envases_prod.descripcion else ""
            x_puerto.abreviaturaEnvase          = if available envases_prod then
                                                  envases_prod.abreviatura else ""
            x_puerto.descripcionInglesEnvase    = if available envases_prod then
                                                  envases_prod.descripcion_ingles else ""
            x_puerto.abreviaturaInglesEnvase    = if available envases_prod then
                                                  envases_prod.abreviatura_ingles else ""
            x_puerto.id_caract                  = pallets.id_caract
            x_puerto.descripcionCaract          = if available caracteristicas then
                                                  caracteristicas.descripcion else ""
            x_puerto.abreviaturaCarac           = if available caracteristicas then
                                                  caracteristicas.abreviatura else ""
            x_puerto.id_articulo                = pallets.id_articulo
            x_puerto.descripcionProducto        = if available productos_terminados then
                                                  productos_terminados.descripcion else ""
            x_puerto.descripcionInglesProducto  = if available productos_terminados then
                                                  productos_terminados.descripcion_ingles else ""
            x_puerto.id_sucursalDestino         = destinos.id_sucursal
            x_puerto.id_orden                   = pallets.id_orden
            x_puerto.id_programa_despacho       = pedidos_packing.id_programa_despacho
            x_puerto.id_permiso_embarque        = pallets_puerto.id_permiso_embarque
            x_puerto.item_remito                = pallets.item_remito
            x_puerto.id_empresa                 = pallets.id_empresa
            x_puerto.bultos                     = pallets.bultos
            x_puerto.bodega                     = pallets_puerto.bodega
            x_puerto.id_orden_embarque          = pallets_puerto.id_orden_embarque
            x_puerto.contenedor                 = pallets_puerto.contenedor
            x_puerto.precinto                   = pallets_puerto.precinto
            x_puerto.termografo                 = pallets_puerto.termografo
            x_puerto.item                       = pallets.item
            x_puerto.nro_comprobante            = substr(pallets_puerto.nro_comprobante,1,4) + "-" +
                                                  substr(pallets_puerto.nro_comprobante,5,8)
            x_puerto.fecha_emb                  = pallets_puerto.fecha_emb
            x_puerto.anulado                    = pallets_puerto.anulado
            x_puerto.pallet_senasa              = pallets.pallet_senasa
            x_puerto.precinto_embarque          = pallets_puerto.precinto_embarque
            x_puerto.anio                       = pallets_puerto.anio
            x_puerto.calibre                    = substr(pallets.calibre,1,3) + "/" +
                                                  substr(pallets.calibre,4,1)
            x_puerto.mercado                    = pedidos_packing.id_mercado
            x_puerto.sailing                    = destinos.sailing.
    end.
    else do:
        create x_puerto.
        assign
            x_puerto.id_sucursal                = pallets_puerto.id_sucursal
            x_puerto.id_pallet                  = pallets_puerto.id_pallet
            x_puerto.descripcionVapor           = "INEXISTENTE".
    end.
end.


/***********************************************************************************/
/***********************************************************************************/
/************************************************************************************
CAMPOS SIN USO O QUE NO TIENEN INFORMACION

            x_puerto.id_destino_grupo
            x_puerto.descripcionInglesPuerto
            x_puerto.Tara
            x_puerto.pallets
            x_puerto.produccion
            x_puerto.envase_packing
            x_puerto.cant_bolsas
            x_puerto.volume
            x_puerto.estado
            x_puerto.costo
            x_puerto.kilos
            x_puerto.fecha_baja
            x_puerto.unidad_distribucion
            x_puerto.unidad_almacen
            x_puerto.nombre_bmp
            x_puerto.estadoProdTerm
            x_puerto.nombre

AÑADI EL CAMPO
            razonsocialCliente
NO LO VI Y ME PARECIO QUE TENIA QUE IR (A LO MEJOR NO)
************************************************************************************/
/***********************************************************************************/
/***********************************************************************************/

output to "\\samiweb\public\infocliente.txt". 

put "Cod.Cli;".
put "Cliente;".
put "Cod.Vap;".
put "Suc;".
put "Pallet;".
put "Vapor;".
put "P.E.;".
put "Descripcion PE;".
put "Abreviatura PE;".
put "Cod.Ad;".
put "Aduana;".
put "Cod.Dest.;".
put "Cod.Dest.Pack;".
put "Descripcion Destino;".
put "Abrev.Destino;".
put "Cod.T.Esq.;".
put "Descripcion T.Esquin.;".
put "Abrev.T.Esquin;".
put "Cod.Variedad;".
put "Descripcion Variedad;".
put "Abrev.Variedad;".
put "Descripcion Variedad Ingles;".
put "Abrev.Variedad Ingles;".
put "Cod.Marca;".
put "Descripcion Marca;".
put "Abrev.Marca;".
put "Descripcion Marca Ingles;".
put "Abrev.Marca Ingles;".
put "Cod.Envase;".
put "Descripcion Envase;".
put "Abrev.Envase;".
put "Descripcion Envase Ingles;".
put "Abrev.Envase Ingles;".
put "Cod.Caract.;".
put "Descripcion Caracteristica;".
put "Abrev.Caract;".
put "Cod.Producto;".
put "Descripcion Producto;".
put "Descripcion Producto Ingles;".
put "Cod.Suc.Dest.;".
put "Pedido;".
put "Prog.Desp.;".
put "Permiso Emb.;".
put "Item Remito;".
put "Cod.Empresa;".
put "Bultos;".
put "Bodega;".
put "Orden Emb.;".
put "Contenedor;".
put "Precinto (Viejo);".
put "Termografo;".
put "Item Pedido;".
put "Remito;".
put "Fecha Emb.;".
put "Anulado;".
put "Pallet Senasa;".
put "Precinto Embarque;".
put "Año;".
put "Calibre;".
put "Mercado;".
put "Sailing;".

put skip.

for each x_puerto:
    export delimiter ";" x_puerto
    except
    x_puerto.id_destino_grupo
    x_puerto.Tara
    x_puerto.pallets
    x_puerto.produccion
    x_puerto.envase_packing
    x_puerto.cant_bolsas
    x_puerto.volume
    x_puerto.estado
    x_puerto.costo
    x_puerto.kilos
    x_puerto.fecha_baja
    x_puerto.unidad_distribucion
    x_puerto.unidad_almacen
    x_puerto.nombre_bmp
    x_puerto.estadoProdTerm
    x_puerto.nombre.
end.

output close.



