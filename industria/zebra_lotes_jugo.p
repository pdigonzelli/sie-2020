define input parameter r as rowid.
define var i as integer.
define var total as integer.
define var f as integer.
define buffer comp for composicion_lote.

DEFINE VARIABLE cZebra AS CHARACTER  NO-UNDO.



FOR EACH composicion_lote where rowid(composicion_lote) = r no-lock .
/*if available composicion_lote then do: */
    do i = composicion_lote.numeracion_desde to  composicion_lote.numeracion_hasta:
        total = 0.        
        
        if composicion_lote.id_sucursal = 96 then 
            cZebra = "c:\lpt1". 
        else
            cZebra = "\\192.168.2.11\zebra". 

        cZebra = "\\192.168.1.105\ibm4400".
        
        OUTPUT TO VALUE(cZebra).
        
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
                
        put control "^FO400,50^A0R,300,280^FD" string(composicion_lote.id_lote,">999")  + "/" + SUBSTRING(STRING(composicion_lote.fecha),7,2,"CHARACTER") "^FS".
            for each comp where comp.id_empresa = composicion_lote.id_empresa and
                                comp.id_sucursal = composicion_lote.id_sucursal and
                                comp.id_tipotambor = composicion_lote.id_tipotambor AND
                                comp.nromov = composicion_lote.nromov no-lock.
                total = total + comp.cantidad_tambores.
            end.
        
        find tambores_industria where tambores_industria.id_empresa = composicion_lote.id_empresa and
                                      tambores_industria.id_sucursal = composicion_lote.id_sucursal and
                                      tambores_industria.id_tipotambor = composicion_lote.id_tipotambor and
                                      tambores_industria.nromov = composicion_lote.nromov and
                                      tambores_industria.id_tambor = i no-lock no-error.
        
        if available tambores_industria then
         do:
            put control "^FO725,735^A0R,72,42^FD" substr(string(composicion_lote.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
         end.
        else
         do:
            put control "^FO725,850^A0R,72,42^FD" "ERROR" "^FS".
         end.
        
        if total > 99 then
            put control "^FO24,100^A0R,300,250^FD" string(i,"999") + "/" + string(total,"999") "^FS".
        else
            put control "^FO24,100^A0R,300,320^FD" string(i,"99") + "/" + string(total,"99") "^FS".

        put control "^FO150,970^A0N,^BY3^B3N,N,230,N,N^FD" "03" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        put control "^FO10,965^GB780,0,3^FS".
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
        
        if composicion_lote.id_sucursal = 96 then
          do:  
            f = 1.
            do f = 1 to 30000.
            
            end.    
               
          end.
        else
          do:
            f = 1.
            do f = 1 to 10000.
            
            end.               
            
           end.
    
   end.

   LEAVE.
end.




