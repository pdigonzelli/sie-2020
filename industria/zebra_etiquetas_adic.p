define var f as integer.
define var i as integer.

/**************UNA MAS******************************************************************/   
i = 1.
   do i = 1 to 59.
         output to c:\lpt1.  
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO100,120^A0R,520,270^FD" "TW0177"  "^FS".
                
                                
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
    output close.
   f = 1.
   do f = 1 to 30000.
   end.
end.
       
        
     /********FIN ETIQUETA WILD***********************/
