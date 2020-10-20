define input parameter r as rowid.
define input parameter manu as date.
define var i as integer.
define var f as integer.
define var total as integer.
define var v_tara as char.
define var v_tara_k as char.
define var v_gross as char.
define var v_gross_k as char.
define buffer tam for tambores_industria.
find lotes_aceite where rowid(lotes_aceite) = r no-error.
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

    for each tambores_industria where tambores_industria.id_empresa = lotes_aceite.id_empresa
                                  and tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                                  and tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                                  and tambores_industria.nromov = lotes_aceite.nromov 
                                   by tambores_industria.id_tambor.
                                   
        if tambores_industria.id_tambor = 1 then
            do:
                output to c:\lpt1. 
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
        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A." + productos_terminados.id_rnpa + " - R.N.E.23000573^FS".
                  end.
                 else
                  do:
        put control "^FO648,672^A0R,25,30^FD" "                        Planta Industrial 2" "^FS".
        put control "^FO616,336^A0R,25,30^FD" "Ruta Provincial 38 - CP 4132 PADILLA FAMAILLA - ARGENTINA." "^FS".
        put control "^FO584,184^A0R,25,30^FD" "                     Tel: (0381)4512650 - R.N.P.A." + productos_terminados.id_rnpa + " - R.N.E.23001973^FS".            
                  end.
             end.
         end.
        put control "^FO578,20^GB3,1200,3^FS".
        
        put control "^FO510,32^A0R,50,60^FD" "BATCH N." "^FS".
        put control "^FO510,810^A0R,50,50^FD" string(lotes_aceite.id_lote,">>99")  + "/" + 
                                              SUBSTRING(STRING(tambores_industria.anio),3,2,"CHARACTER") "^FS".
        
        put control "^FO440,32^A0R,50,60^FD" "DATE OF MANUFACTURE" "^FS".
        put control "^FO440,810^A0R,50,50^FD" manu "^FS".
        
        put control "^FO360,32^A0R,50,60^FD" "DRUM N." "^FS".
        put control "^FO360,810^A0R,50,50^FD" "00/" + string(total,"99") "^FS".
        
        put control "^FO280,32^A0R,50,60^FD" "GROSS WEIGHT" "^FS".
        
        v_gross = string((tambores_industria.tara + 174.64),"999.99").
        substring(v_gross,4,1) = ",".
        put control "^FO280,520^A0R,50,50^FD" v_gross "^FS".
        put control "^FO280,700^A0R,50,60^FD" "KGS" "^FS".
        
        v_gross_k = string(((tambores_industria.tara + 174.64) * 2.2046),"999.99").
        substring(v_gross_k,4,1) = ",".
        put control "^FO280,848^A0R,50,50^FD" v_gross_k  "^FS".
        put control "^FO280,1000^A0R,50,60^FD" "LBS" "^FS".
        
        put control "^FO200,32^A0R,50,60^FD" "TARE WEIGHT" "^FS".
        
        v_tara = string(tambores_industria.tara,"99.99").
        substring(v_tara,3,1) = ",".
        
        put control "^FO200,545^A0R,50,50^FD" v_tara "^FS".
        put control "^FO200,700^A0R,50,60^FD" "KGS" "^FS".
        
        v_tara_k = string((((tambores_industria.tara + 174.64) * 2.2046) - 385),"99.99").
        substring(v_tara_k,3,1) = ",".
        
        put control "^FO200,872^A0R,50,50^FD" v_tara_k "^FS".
        put control "^FO200,1000^A0R,50,60^FD" "LBS" "^FS".
        
        put control "^FO120,32^A0R,50,60^FD" "NET WEIGHT" "^FS".
        put control "^FO120,520^A0R,50,50^FD" "174,64" "^FS".
        put control "^FO120,700^A0R,50,60^FD" "KGS" "^FS".
        put control "^FO120,848^A0R,50,50^FD" "385,00" "^FS".
        put control "^FO120,1000^A0R,50,60^FD" "LBS" "^FS".

  
        put control "^FO720,50^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + 
                                             string(tambores_industria.id_etiqueta,"9999999") "^FS".
         
        put control "^FO10,200^A0R,^BY3^B3R,N,100,N,N^FD" "06" + string(tambores_industria.id_etiqueta,"9999999") "^FS". 

        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        f = 1.
        do f = 1 to 25000.                   
        
        end.

            end.
            
        output to c:\lpt1. 
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
        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A." + productos_terminados.id_rnpa + " - R.N.E.23000573^FS".
                  end.
                 else
                  do:
        put control "^FO648,672^A0R,25,30^FD" "                        Planta Industrial 2" "^FS".
        put control "^FO616,336^A0R,25,30^FD" "Ruta Provincial 38 - CP 4132 PADILLA FAMAILLA - ARGENTINA." "^FS".
        put control "^FO584,184^A0R,25,30^FD" "                     Tel: (0381)4512650 - R.N.P.A." + productos_terminados.id_rnpa + " - R.N.E.23001973^FS".            
                  end.
             end.
         end.
        put control "^FO578,20^GB3,1200,3^FS".
        
        put control "^FO510,32^A0R,50,60^FD" "BATCH N." "^FS".
        put control "^FO510,810^A0R,50,50^FD" string(lotes_aceite.id_lote,">>99")  + "/" + 
                                              SUBSTRING(STRING(tambores_industria.anio),3,2,"CHARACTER") "^FS".
        
        put control "^FO440,32^A0R,50,60^FD" "DATE OF MANUFACTURE" "^FS".
        put control "^FO440,810^A0R,50,50^FD" manu "^FS".
        
        put control "^FO360,32^A0R,50,60^FD" "DRUM N." "^FS".
        put control "^FO360,810^A0R,50,50^FD" string(tambores_industria.id_tambor,"99") + "/" + string(total,"99") "^FS".
        
        put control "^FO280,32^A0R,50,60^FD" "GROSS WEIGHT" "^FS".
        
        v_gross = string((tambores_industria.tara + 174.64),"999.99").
        substring(v_gross,4,1) = ",".
        put control "^FO280,520^A0R,50,50^FD" v_gross "^FS".
        put control "^FO280,700^A0R,50,60^FD" "KGS" "^FS".
        
        v_gross_k = string(((tambores_industria.tara + 174.64) * 2.2046),"999.99").
        substring(v_gross_k,4,1) = ",".
        put control "^FO280,848^A0R,50,50^FD" v_gross_k  "^FS".
        put control "^FO280,1000^A0R,50,60^FD" "LBS" "^FS".
        
        put control "^FO200,32^A0R,50,60^FD" "TARE WEIGHT" "^FS".
        
        v_tara = string(tambores_industria.tara,"99.99").
        substring(v_tara,3,1) = ",".
        
        put control "^FO200,545^A0R,50,50^FD" v_tara "^FS".
        put control "^FO200,700^A0R,50,60^FD" "KGS" "^FS".
        
        v_tara_k = string((((tambores_industria.tara + 174.64) * 2.2046) - 385),"99.99").
        substring(v_tara_k,3,1) = ",".
        
        put control "^FO200,872^A0R,50,50^FD" v_tara_k "^FS".
        put control "^FO200,1000^A0R,50,60^FD" "LBS" "^FS".
        
        put control "^FO120,32^A0R,50,60^FD" "NET WEIGHT" "^FS".
        put control "^FO120,520^A0R,50,50^FD" "174,64" "^FS".
        put control "^FO120,700^A0R,50,60^FD" "KGS" "^FS".
        put control "^FO120,848^A0R,50,50^FD" "385,00" "^FS".
        put control "^FO120,1000^A0R,50,60^FD" "LBS" "^FS".

  
        put control "^FO720,50^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + 
                                             string(tambores_industria.id_etiqueta,"9999999") "^FS".
         
        put control "^FO10,200^A0R,^BY3^B3R,N,100,N,N^FD" "06" + string(tambores_industria.id_etiqueta,"9999999") "^FS". 

        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        
        f = 1.
        do f = 1 to 25000.                   
        end.

           
   end. 
end. 
else message "Existe algun problema con el lote" view-as alert-box.




