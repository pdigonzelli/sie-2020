define input parameter r as rowid.
define var j as integer.
DEFINE VARIABLE cZebraPrinter AS CHARACTER  NO-UNDO.


find produccion_jugo where rowid(produccion_jugo) = r no-lock no-error.
if available produccion_jugo then
do:
    for each tambores_industria where tambores_industria.id_empresa = produccion_jugo.id_empresa
                                  and tambores_industria.id_sucursal = produccion_jugo.id_sucursal
                                  and tambores_industria.id_tipotambor = produccion_jugo.id_tipotambor
                                  and tambores_industria.nromov = produccion_jugo.nromov  
                                   by id_tambor.
        if tambores_industria.id_sucursal = 96 then
                cZebraPrinter =  "c:\lpt1". 
            else
                cZebraPrinter = "\\192.168.2.11\zebra".

        /*zebra facundoj*/
        /*cZebraPrinter = "\\192.168.1.104\ibm4400".*/

        OUTPUT TO VALUE(cZebraPrinter). 
                
        put control "^XA".
        put control "^LH0,0".        
        put control "^PQ" 1 "^FS".
        
        put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "01" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        put control "^FO24,393^A0R,130,140^FD" produccion_jugo.fecha "^FS".
        put control "^FO725,835^A0R,72,42^FD" substr(string(produccion_jugo.anio),3,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = produccion_jugo.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    if productos_terminados.id_articulo = 531 or
                       productos_terminados.id_articulo = 88  then 
                        do:
                            put control "^FO550,288^A0R,150,140^FD" productos_terminados.abreviatura "^FS".
                            put control "^FO190,292^A0R,300,320^FD" "C." + string(productos_terminados.id_articulo) "^FS".
                        end.
                    else 
                        do:
                            put control "^FO550,288^A0R,150,140^FD" "PRODUCCION" "^FS".
                            put control "^FO190,292^A0R,300,320^FD" string(produccion_jugo.id_produccion) "^FS".                
                        end.
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(productos_terminados.id_articulo) "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                end.
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS".         
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
        if tambores_industria.id_sucursal = 96 then
            do:
                    j = 1.
                    do j = 1 to 30000.
                    
                    end.

            end.
            else
            do:
                    j = 1.
                    do j = 1 to 5000.
                    
                    end.

            end.

     end.
end.



