DEFINE VAR v_desde_fecha AS DATE.
DEFINE VAR v_hasta_fecha AS DATE.
    
v_desde_fecha = DATE("01/01/2004").
v_hasta_fecha = DATE("10/02/2005").

OUTPUT TO VALUE("c:\temp\ver-oe.txt").
PUT "Orden;Parte;Fecha;Año;Sem;Cod;Producto".
PUT SKIP.
FOR EACH orden_entrega WHERE fecha >= v_desde_fecha AND fecha <= v_hasta_fecha NO-LOCK,
    EACH items_orden_entrega OF orden_entrega NO-LOCK:
    FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
    EXPORT DELIMITER ";"
        orden_entrega.id_orden_entrega
        items_orden_entrega.ITEM_oe
        orden_entrega.fecha
        orden_entrega.anio 
        items_orden_entrega.semana
        items_orden_entrega.id_articulo
        productos_terminados.descripcion WHEN AVAILABLE productos_terminados.

END.
OUTPUT CLOSE.
