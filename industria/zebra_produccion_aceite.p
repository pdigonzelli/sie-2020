
define input parameter r as rowid.
find tambores_industria where rowid(tambores_industria) = r no-lock no-error.
if available tambores_industria then
do:
            if tambores_industria.id_sucursal = 96 then
                output to c:\lpt1.
            else
                output to \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
    
            put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "02" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
            put control "^FO24,393^A0R,130,140^FD" tambores_industria.fecha "^FS".
            put control "^FO725,835^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".

            put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
                
            find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
                if available productos_terminados then
                    do:
                        if productos_terminados.id_articulo = 61 or 
                         productos_terminados.id_articulo = 73 or 
                         productos_terminados.id_articulo = 41 or
                         productos_terminados.id_articulo = 513 or
                         productos_terminados.id_articulo = 514 or 
                         productos_terminados.id_articulo = 76 or
                         productos_terminados.id_articulo = 516 or
                         productos_terminados.id_articulo = 518 then
                       do: 
                        put control "^FO550,288^A0R,150,140^FD" productos_terminados.abreviatura "^FS".
                        put control "^FO190,292^A0R,300,320^FD" "C." + string(productos_terminados.id_articulo) "^FS".
                        put control "^FO672,1095^A0N,36,42^FDPROD.^FS".            
                        put control "^FO702,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                       end.
                      else 
                       do: 
                        put control "^FO550,288^A0R,150,140^FD" "PRODUCCION" "^FS".
                        put control "^FO190,292^A0R,300,320^FD" string(tambores_industria.id_tambor) "^FS".
                       end.
                        put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(productos_terminados.id_articulo) "^FS".
                     end.
            put control "^FO10,275^GB780,0,3^FS".
            put control "^FO10,1072^GB780,0,3^FS".
            put control "^FO10,1200^GB780,0,3^FS". 
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
        
end.
else
    message "Existe un error, no sale la etiqueta. Comunicarse con Centro de Computos" view-as alert-box. 



