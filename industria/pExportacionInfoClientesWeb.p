 /*-----------------------------------------------------------
   Exportacion de Datos de los Clientes
 -------------------------------------------------------------*/
 
 OUTPUT TO \\samiweb\public\mos_clients.txt.
     FOR EACH clientes WHERE clientes.id_tipo_cliente = 1 OR
                             clientes.id_tipo_cliente = 2 OR
                             clientes.id_tipo_cliente = 3 NO-LOCK: 
         EXPORT DELIMITER ";" id_cliente razon_social RECID(clientes).
     END.
 OUTPUT CLOSE.


