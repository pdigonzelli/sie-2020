define var f as integer.
define var i as integer.

        do i = 1 to 60.
                output to \\192.168.1.92\zebra.  
        
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO390,320^A0R,220,270^FD" "WILD"  "^FS".
                
                put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 436)" "^FS".
                
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
               output close.
               f = 1.
               do f = 1 to 30000.
    
               end.
        end. 
