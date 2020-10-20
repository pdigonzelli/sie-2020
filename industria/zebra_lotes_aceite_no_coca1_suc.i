if tambores_industria.id_articulo = 58 then
 do:
    
    if p_suc = 96 then
        output to c:\lpt1. 
    else
        output to \\192.168.2.11\zebra.
                
    put control "^XA".
    put control "^LH0,0".
    put control "^PQ" 1 "^FS".
    put control "^FO400,50^A0R,300,280^FD" string(tambores_industria.id_lote,">999")  + "/" + SUBSTRING(STRING(tambores_industria.anio),3,2,"CHARACTER") "^FS".
    put control "^FO725,735^A0R,72,42^FD" substr(string(tambores_industria.anio),3,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
               
    put control "^FO24,100^A0R,300,320^FD" string(tambores_industria.id_tambor,"99") + "/" + string(total,"99") "^FS".

    put control "^FO150,970^A0N,^BY3^B3N,N,230,N,N^FD" "07" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
    put control "^FO10,965^GB780,0,3^FS".
    put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
    put control "^XZ".
    output close.
    f = 1.
    do f = 1 to 25000.
                   
    end.
 end.
