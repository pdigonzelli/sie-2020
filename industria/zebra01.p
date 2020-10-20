define input parameter r as rowid.
define input parameter tipo_etiqueta as integer.
find produccion_jugo where rowid(produccion_jugo) = r no-lock no-error.
if available produccion_jugo then
do:
    for each tambores_produccion_jugo of produccion_jugo by id_tambor.

    output to \\gabriel\zebra.

    put control "^XA".
    put control "^LH0,0".
    put control "^PQ" 1 "^FS".

    case tipo_etiqueta:
     when 1 then /*PRODUCCION JUGO*/
        do:
            put control "^FO80,40^A0N,^BY3^B3N,N,230,N,N^FD" produccion_jugo.id_empresa + produccion_jugo.id_sucursal + produccion_jugo.id_produccion + produccion_jugo.fecha "^FS".
            put control "^FO550,288^A0R,150,140^FD" "PRODUCCION" "^FS".
            put control "^FO190,292^A0R,300,320^FD" string(produccion_jugo.id_produccion) "^FS".
            put control "^FO24,393^A0R,130,140^FD" produccion_jugo.fecha "^FS".
        end.

    end.  /* END DEL CASE*/
 
     put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
     put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

    case tipo_etiqueta:
     when 1 then /*PRODUCCION JUGO*/
        do:
            find productos_terminados where productos_terminados.id_articulo = produccion_jugo.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_produccion_jugo.id_tambor "^FS".
                    
                end.
        end.  /*1*/
    end. /*END DEL CASE*/
     put control "^FO10,275^GB780,0,3^FS".
     put control "^FO10,1072^GB780,0,3^FS".
     put control "^FO10,1200^GB780,0,3^FS". 
     put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
     put control "^XZ".
     output close.
     end.
end.



