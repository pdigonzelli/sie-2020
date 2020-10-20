define input parameter r as rowid.

DEFINE VARIABLE cZebra AS CHARACTER  NO-UNDO.

find sobrante_lotes_aceite where rowid(sobrante_lotes_aceite) = r no-lock no-error.
if available sobrante_lotes_aceite then

do:
   
   find lotes_aceite of sobrante_lotes_aceite no-lock no-error.
   if available lotes_aceite then                               
   do:                                 
   
    for each tambores_industria where tambores_industria.id_empresa = sobrante_lotes_aceite.id_empresa
                                  and tambores_industria.id_sucursal = sobrante_lotes_aceite.id_sucursal
                                  and tambores_industria.id_tipotambor = sobrante_lotes_aceite.id_tipotambor_sobrante
                                  and tambores_industria.nromov = sobrante_lotes_aceite.nromov_sobrante
                                   by tambores_industria.id_tambor.

        
        if tambores_industria.id_sucursal = 96 then
            cZebra = "c:\lpt1".
        else
            cZebra = "\\192.168.2.11\zebra".
        
        /*zebra facundoj*/
        /*cZebra = "\\192.168.1.104\ibm4400".*/

        OUTPUT TO VALUE(cZebra) .
             
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".

        put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "08" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        IF tambores_industria.id_articulo <> 519 AND
           tambores_industria.id_articulo <> 520 THEN DO:
            put control "^FO580,288^A0R,150,140^FD" "SOBRANTE" "^FS".
        END.
        put control "^FO390,292^A0R,130,140^FD" "LOTE " + string(lotes_aceite.id_lote) + "/" + SUBSTRING(STRING(lotes_aceite.anio),3,2,"CHARACTER") "^FS".
        
        put control "^FO24,393^A0R,130,140^FD" tambores_industria.fecha "^FS".
        put control "^FO725,835^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".

        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO200,293^A0R,140,125^FD" productos_terminados.abreviatura "^FS".                    
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.abreviatura + "  -  " + string(tambores_industria.id_articulo) "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                end.
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS". 
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
     end.
    end.
end.



