define input parameter r as rowid.
find produccion_jugo where rowid(produccion_jugo) = r no-lock no-error.
if available produccion_jugo then
do:
    for each tambores_produccion_jugo of produccion_jugo by id_tambor.
      /*  message id_tambor. */
      pause 0.5.
       /* output to \\gabriel\zebra. */
        output to c:\lpt1.
        put control "^XA".
        put control "^LH0,0".        
        put control "^PQ" 1 "^FS".
        
        put control "^FO30,40^A0N,^BY3^B3N,N,230,N,N^FD" "01" + string(produccion_jugo.id_empresa,"9") + string(produccion_jugo.id_sucursal,"999") + string(produccion_jugo.id_produccion,"99999") + string(tambores_produccion_jugo.id_tambor,"999") "^FS".
        put control "^FO550,288^A0R,150,140^FD" "PRODUCCION" "^FS".
        put control "^FO190,292^A0R,300,320^FD" string(produccion_jugo.id_produccion) "^FS".
        put control "^FO24,393^A0R,130,140^FD" produccion_jugo.fecha "^FS".
        put control "^FO725,850^A0R,72,42^FD" substr(string(produccion_jugo.fecha),7,2) + "-"  + string(tambores_produccion_jugo.id_etiqueta,"9999999") "^FS".

        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = produccion_jugo.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(productos_terminados.id_articulo) "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_produccion_jugo.id_tambor "^FS".
                end.
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS".         
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
     end.
end.



