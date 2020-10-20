        output to c:\lpt1. 
        put control "^XA".
        put control "^LH0,0".        
        put control "^PQ" 1 "^FS".
        
        put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "01" + string(006,"9999999") "^FS".
        put control "^FO24,393^A0R,130,140^FD" today "^FS".
        put control "^FO725,50^A0R,72,42^FD" substr(string(today),7,2) + "-"  + string(066,"9999999") "^FS".
        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        put control "^FO650,288^A0R,150,140^FD" "PRODUCCION" "^FS".
        put control "^FO350,292^A0R,300,320^FD" string(1048) "^FS".
        put control "^FO190,380^A0R,180,170^FD" "c/sodio" "^FS".                
        put control "^FO24,1135^A0N,72,42^FDjugo turbio -  58^FS".
        put control "^FO630,1135^A0N,72,42^FD" 1 "^FS".
                
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS".         
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
