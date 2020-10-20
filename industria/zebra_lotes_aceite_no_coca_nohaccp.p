define input parameter r as rowid.
define input parameter desde as integer.
define input parameter hasta as integer.
define var i as integer.
define var ii as integer.
define var f as integer.
define var v_rnpa as char.
define var kilos as integer.
define var total as integer.
define buffer tam for tambores_industria.
kilos = 180.
find lotes_aceite where rowid(lotes_aceite) = r.

if available lotes_aceite then
 do:
    total = 0.
    for last tam where tam.id_empresa = lotes_aceite.id_empresa
                   and tam.id_sucursal = lotes_aceite.id_sucursal
                   and tam.id_tipotambor = lotes_aceite.id_tipotambor
                   and tam.nromov = lotes_aceite.nromov
                   by tam.id_tambor.
        total = tam.id_tambor.
    end.
    
    if desde = 0 and hasta = 0 then
    do:
     for each tambores_industria where tambores_industria.id_empresa = lotes_aceite.id_empresa
                                  and tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                                  and tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                                  and tambores_industria.nromov = lotes_aceite.nromov
                                   by tambores_industria.id_tambor.
        if tambores_industria.id_articulo = 58 then
            do:
                kilos = 200.
            end.
        if tambores_industria.id_articulo = 51 then
            do:
                kilos = 180.                
            end.
        if tambores_industria.id_tambor = 1 then
            do:
            
            if tambores_industria.id_sucursal = 96 then
                output to c:\lpt1. 
            else
                output to \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
        
            put control "^FO720,20^GB3,1200,3^FS".                
            put control "^FO725,520^A0R,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO680,20^GB3,1200,3^FS".
            put control "^FO688,296^A0R,25,30^FD" "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria,  y Financiera" "^FS".
            find sucursales where sucursales.id_sucursal = tambores_industria.id_sucursal no-lock no-error.
            if available sucursales then 
             do:
                find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
                if available productos_terminados then
                 do: 
                   if tambores_industria.id_sucursal = 96 then 
                    do: 
                        put control "^FO648,672^A0R,25,30^FD" "Administracion y Planta Industrial 1" "^FS".
                        put control "^FO616,336^A0R,25,30^FD" "Lavalle 4001- C.C. 240(4000) S.M. de TUCUMAN - ARGENTINA." "^FS".
                        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A." + string(productos_terminados.id_rnpa) + " - R.N.E.23000573^FS".
                    end.
                   else
                    do:
                        put control "^FO648,672^A0R,25,30^FD" "                        Planta Industrial 2" "^FS".
                        put control "^FO616,336^A0R,25,30^FD" "Ruta Provincial 38 - CP 4132 PADILLA FAMAILLA - ARGENTINA." "^FS".
                        put control "^FO584,184^A0R,25,30^FD" "                     Tel: (0381)4512650 - R.N.P.A." + string(productos_terminados.id_rnpa) + " - R.N.E.23001973^FS".            
                   end.
                 end.
              end.
            put control "^FO578,20^GB3,1200,3^FS".
        
            put control "^FO510,32^A0R,50,60^FD" "BATCH N." "^FS".
            put control "^FO510,710^A0R,50,50^FD" string(lotes_aceite.id_lote,">>99")  + "/" + SUBSTRING(STRING(tambores_industria.anio),3,2,"CHARACTER") "^FS".
        
            put control "^FO410,32^A0R,50,60^FD" "DRUM N." "^FS".
            put control "^FO410,710^A0R,50,50^FD" "00/" + string(total,"99") "^FS".
        
            put control "^FO310,32^A0R,50,60^FD" "GROSS WEIGHT" "^FS".
            put control "^FO310,710^A0R,50,50^FD" string((round(tambores_industria.tara,2) + kilos),"999.99") "^FS".
            put control "^FO310,890^A0R,50,60^FD" "KGS" "^FS".
        
            put control "^FO210,32^A0R,50,60^FD" "TARE WEIGHT" "^FS".
            put control "^FO210,735^A0R,50,50^FD" string(round(tambores_industria.tara,2),"99.99") "^FS".
            put control "^FO210,890^A0R,50,60^FD" "KGS" "^FS".
        
            put control "^FO120,32^A0R,50,60^FD" "NET WEIGHT" "^FS".
            put control "^FO120,710^A0R,50,50^FD" string(kilos,"999.99") "^FS".
            put control "^FO120,890^A0R,50,60^FD" "KGS" "^FS".
  
            put control "^FO720,50^A0R,72,42^FD" substr(string(tambores_industria.anio),3,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
                
            put control "^FO110,20^GB3,620,3^FS".
          /*  put control "^FO80,270^A0R,25,30^FD" "Important" "^FS".
            put control "^FO20,60^A0R,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS". */
            put control "^FO10,650^A0R,^BY3^B3R,N,100,N,N^FD" "07" + string(tambores_industria.id_etiqueta,"9999999") "^FS". 

            
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            f = 1.
            do f = 1 to 25000.
                   
            end. 
        
            /*  NO VA LA ETIQUETA DE LOTE PORQUE ES EL TAMBOR NUMERO CERO */
        
           
        end.
      
        {zebra_lotes_aceite_no_coca2_nohaccp.i}
        
        {zebra_lotes_aceite_no_coca1.i}
        
       end. /* END del FOR EACH */

      end. /* END del IF */
      else
        do:
          
          do ii = desde to hasta:
            find tambores_industria of lotes_aceite where tambores_industria.id_tambor = ii no-lock no-error.
          
            if available tambores_industria then
              do:  
                 if tambores_industria.id_articulo = 58 then
                    kilos = 200.
                 if tambores_industria.id_articulo = 51 then
                    kilos = 180.
                 if tambores_industria.id_tambor = 1 then
                    do:
                        {zebra_lotes_aceite_no_coca2_nohaccp.i}
                
                        {zebra_lotes_aceite_no_coca1.i}
                    end.
                    {zebra_lotes_aceite_no_coca2_nohaccp.i} 
        
        
                    {zebra_lotes_aceite_no_coca1.i}        
        
             end. /* END del If */
            end. /* END del do */
    
        end.          
                    

end. 


