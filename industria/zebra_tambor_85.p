for each tambores_productos_terceros where id_tambor = 85.
        output to c:\lpt1. 
        put control "^XA".
        put control "^LH0,0".        
        put control "^PQ" 1 "^FS".
        
        put control "^FO30,20^A0N,^BY3^B3N,N,180,N,N^FD" "09" + string(tambores_productos_terceros.id_empresa,"9") + 
                                                                string(tambores_productos_terceros.id_sucursal,"999") + 
                                                                string(tambores_productos_terceros.indice_tambor,"9999999") "^FS".
        
        find proveedores where proveedores.id_proveedor = tambores_productos_terceros.id_proveedor no-lock no-error.
            if available proveedores then
                do:
                    put control "^FO600,210^A0R,120,70^FD" string(proveedores.nombre) "^FS".
                end.
        put control "^FO140,280^A0R,300,320^FD" string(tambores_productos_terceros.id_lote) "^FS".
        put control "^FO10,300^A0R,130,130^FD" tambores_productos_terceros.fecha "^FS".
        
        put control "^FO725,985^A0R,72,42^FD" substr(string(tambores_productos_terceros.fecha),7,2) + "-"  + 
        string(tambores_productos_terceros.id_etiqueta,"9999999") "^FS".

        put control "^FO80,850^A0R,60,60^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = tambores_productos_terceros.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO500,210^A0R,110,90^FD" productos_terminados.descripcion "^FS".
                    put control "^FO10,1000^A0R,72,52^FD" tambores_productos_terceros.id_tambor "^FS".
                end.
                
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
       
     end.
