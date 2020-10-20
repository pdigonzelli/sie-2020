find first agencias where agencias.id_agencia = integer(orden_entrega.id_agencia:screen-value in frame F-Main)  no-lock no-error .
if available agencias then 
fi-agencias-descripcion:screen-value in frame F-Main = string(agencias.descripcion).
else
fi-agencias-descripcion:screen-value in frame F-Main = ''.

find first instrumentos_pagos where instrumentos_pagos.id_instrumento_pago = integer(orden_entrega.id_instrumento_pago:screen-value in frame F-Main)  no-lock no-error .
if available instrumentos_pagos then 
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = string(instrumentos_pagos.descripcion).
else
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = ''.

find first tipos_plazo where tipos_plazo.id_tipo_plazo = integer(orden_entrega.id_tipo_plazo:screen-value in frame F-Main)  no-lock no-error .
if available tipos_plazo then 
fi-tipos_plazo-descripcion:screen-value in frame F-Main = string(tipos_plazo.descripcion).
else
fi-tipos_plazo-descripcion:screen-value in frame F-Main = ''.

find first despachantes where despachantes.id_despachante = integer(orden_entrega.id_despachante:screen-value in frame F-Main)  no-lock no-error .
if available despachantes then 
fi-despachantes-descripcion:screen-value in frame F-Main = string(despachantes.descripcion).
else
fi-despachantes-descripcion:screen-value in frame F-Main = ''.

find first tipo_contenedor where tipo_contenedor.id_tipo_contenedor = integer(orden_entrega.id_tipo_contenedor:screen-value in frame F-Main)  no-lock no-error .
if available tipo_contenedor then 
fi-tipo_contenedor-descripcion:screen-value in frame F-Main = string(tipo_contenedor.descripcion).
else
fi-tipo_contenedor-descripcion:screen-value in frame F-Main = ''.

find first lugar_descarga where lugar_descarga.id_lugdes = integer(orden_entrega.id_lugdes:screen-value in frame F-Main)  no-lock no-error .
if available lugar_descarga then 
fi-lugar_descarga-descripcion:screen-value in frame F-Main = string(lugar_descarga.descripcion).
else
fi-lugar_descarga-descripcion:screen-value in frame F-Main = ''.

find first estados_oe where estados_oe.id_estado = integer(orden_entrega.id_estado:screen-value in frame F-Main)  no-lock no-error .
if available estados_oe then 
fi-estados_oe-descripcion:screen-value in frame F-Main = string(estados_oe.descripcion).
else
fi-estados_oe-descripcion:screen-value in frame F-Main = ''.

find first destinos where destinos.id_destino = integer(orden_entrega.id_destino:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion:screen-value in frame F-Main = ''.

find first clientes where clientes.id_cliente = integer(orden_entrega.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
fi-clientes-nombre:screen-value in frame F-Main = string(clientes.nombre).
else
fi-clientes-nombre:screen-value in frame F-Main = ''.

find first calidades where calidades.id_calidad = integer(orden_entrega.id_calidad:screen-value in frame F-Main)  no-lock no-error .
if available calidades then 
fi-calidades-descripcion:screen-value in frame F-Main = string(calidades.descripcion).
else
fi-calidades-descripcion:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(orden_entrega.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first clausulas where clausulas.id_clausula = integer(orden_entrega.id_condicion_venta:screen-value in frame F-Main)  no-lock no-error .
if available clausulas then 
fi-clausulas-descripcion:screen-value in frame F-Main = string(clausulas.descripcion).
else
fi-clausulas-descripcion:screen-value in frame F-Main = ''.

find first vapores where vapores.id_vapor = integer(orden_entrega.id_vapor:screen-value in frame F-Main)  no-lock no-error .
if available vapores then 
fi-vapores-descripcion:screen-value in frame F-Main = string(vapores.descripcion).
else
fi-vapores-descripcion:screen-value in frame F-Main = ''.
