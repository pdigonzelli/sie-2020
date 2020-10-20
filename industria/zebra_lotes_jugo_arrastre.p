DEFINE INPUT PARAMETER r AS ROWID.

DEFINE VAR f AS INTEGER.



FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
IF AVAILABLE lotes_jugo THEN DO:               
    FOR EACH arrastre_lote WHERE arrastre_lote.id_empresa = lotes_jugo.id_empresa
                             AND arrastre_lote.id_sucursal = lotes_jugo.id_sucursal
                             AND arrastre_lote.id_tipotambor = lotes_jugo.id_tipotambor
                             AND arrastre_lote.nromov = lotes_jugo.nromov NO-LOCK.
        
        FOR EACH tambores_industria WHERE tambores_industria.id_empresa = arrastre_lote.id_empresa
                                      AND tambores_industria.id_sucursal = arrastre_lote.id_sucursal
                                      AND tambores_industria.id_tipotambor = arrastre_lote.id_tipotambor_arrastre
                                      AND tambores_industria.nromov = arrastre_lote.nromov_arrastre
                                      NO-LOCK
                                      BY id_tambor.
                    
            IF tambores_industria.id_sucursal = 96 THEN
                OUTPUT TO c:\lpt1.
            ELSE
                OUTPUT TO \\192.168.2.11\zebra.
            

          /*zebra gabriel*/
          /*OUTPUT TO \\192.168.1.58\zebra105.*/

            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
    
            put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "05" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
            put control "^FO580,288^A0R,150,140^FD" "ARRASTRE" "^FS".
            put control "^FO390,292^A0R,130,140^FD" "LOTE " + string(lotes_jugo.id_lote) + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
            
            put control "^FO24,393^A0R,130,140^FD" tambores_industria.fecha "^FS".
            put control "^FO725,835^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + 
                                                  string(tambores_industria.id_etiqueta,"9999999") "^FS".
    
            
            put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
            put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".
    
            find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
                if available productos_terminados then
                    do:
                        put control "^FO180,293^A0R,130,140^FD" productos_terminados.abreviatura "^FS".                    
                        put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(tambores_industria.id_articulo) "^FS".
                        put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                    end.
            put control "^FO10,275^GB780,0,3^FS".
            put control "^FO10,1072^GB780,0,3^FS".
            put control "^FO10,1200^GB780,0,3^FS". 
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            f = 1.
            do f = 1 to 30000.
        
            end.
        END.
    END.
END.
