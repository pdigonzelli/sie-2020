define input parameter r as rowid.
define var i as integer.
define var total as integer.
define buffer comp for composicion_lote.

find composicion_lote where rowid(composicion_lote) = r no-lock no-error.


            
if available composicion_lote then
 do:
    total = 0.
    for each comp where comp.id_empresa = composicion_lote.id_empresa and
                                comp.id_sucursal = composicion_lote.id_sucursal and
                                comp.id_lote = composicion_lote.id_lote no-lock.
                total = total + comp.cantidad_tambores.
            end.
/*    do i = composicion_lote.numeracion_desde to composicion_lote.numeracion_hasta:*/
for each tambores_lote where tambores_lote.id_empresa = composicion_lote.id_empresa and
                             tambores_lote.id_sucursal = composicion_lote.id_sucursal and
                             tambores_lote.id_lote = composicion_lote.id_lote by tambores_lote.id_tambor.
         
               
/*        output to \\gabriel\zebra. */
        output to c:\lpt1.
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ1^FS".
                
        put control "^FO400,50^A0R,300,280^FD" string(composicion_lote.id_lote,">999")  + "/" + SUBSTRING(STRING(composicion_lote.fecha),7,2,"CHARACTER") "^FS".
            
        
/*        find tambores_lote where tambores_lote.id_empresa = composicion_lote.id_empresa and
                                 tambores_lote.id_sucursal = composicion_lote.id_sucursal and
                                 tambores_lote.id_lote = composicion_lote.id_lote and
                                 tambores_lote.id_tambor = i no-lock no-error.
        
        if available tambores_lote then
         do: */
            put control "^FO725,750^A0R,72,42^FD" substr(string(composicion_lote.fecha),7,2) + "-"  + string(tambores_lote.id_etiqueta,"999999") "^FS".
        /* end.
        else
         do:
            put control "^FO725,850^A0R,72,42^FD" "ERROR" "^FS".
         end. */
        
        put control "^FO24,100^A0R,300,320^FD" string(tambores_lote.id_tambor,"99") + "/" + string(total,"99") "^FS".
/*        put control "^FO24,100^A0R,300,320^FD" string(i,"99") + "/" + string(total,"99") "^FS". */

/*        put control "^FO10,970^A0N,^BY3^B3N,N,230,N,N^FD" "03" + string(composicion_lote.id_empresa,"9") + string(composicion_lote.id_sucursal,"999") + string(composicion_lote.id_lote,"99999") + string(i,"999") "^FS". */
        put control "^FO10,970^A0N,^BY3^B3N,N,230,N,N^FD" "03" + string(composicion_lote.id_empresa,"9") + string(composicion_lote.id_sucursal,"999") + string(composicion_lote.id_lote,"99999") + string(tambores_lote.id_tambor,"999") "^FS". 
        

        put control "^FO10,965^GB780,0,3^FS".
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.

        pause 1. 
           
   end.
end.



