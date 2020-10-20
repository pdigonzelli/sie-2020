 define input parameter r as rowid.
define buffer tam for tambores_lote.
define var tambores as integer.
define var peso_neto as integer.
define var f as integer.
define var gall as decimal.
define var peso_bruto as integer.

find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
if available lotes_jugo then
do:

        for last tam of lotes_jugo by id_tambor.
         tambores = tam.id_tambor. 
        end.

                
        for each tambores_lote of lotes_jugo no-lock.
/**********************************************************************************************************/
/*****************GENERO LA ETIQUETA NUMERO CERO***********************************************************/
            if tambores_lote.id_tambor = 1 then
             do:
            output to c:\lpt1. 
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
            put control "^FO25,144^GB785,0,3^FS".
            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573" "^FS".
            put control "^FO25,200^GB785,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if lotes_jugo.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,750,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.fecha),7,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end.
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            put control "^FO560,434^A0N,45,60^FD" "00/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".
            put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".
            put control "^FO600,810^A0N,45,45^FD" "17" "^FS".
            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,898^A0N,34,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") "^FS".
            put control "^FO25,938^GB355,0,3^FS".
            put control "^FO85,948^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,988^A0N,38,45^FD" "24 MONTH" "^FS".
                end.
            else
                do:
            put control "^FO85,920^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,960^A0N,38,45^FD" "24 MONTH" "^FS".                
                end. 
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1030^GB785,0,3^FS".
            put control "^FO358,1040^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1072^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1100^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO25,1150^GB785,0,3^FS".
            put control "^FO25,1155^GB785,0,3^FS". 
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
                        
            end.
            
/***************TERMINA LA ETIQUETA NUMERO CERO***********************************************************/            
/*********************************************************************************************************/            
            /*            output to \\gabriel\zebra.  */
            output to c:\lpt1. 
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 2 "^FS".
            
            /* put control "^ILR:logo.grf^FS". 
             put control "^DGR:logo.grf,00800,010^FS". */
    
         
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
            put control "^FO25,144^GB785,0,3^FS".
            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573" "^FS".
            put control "^FO25,200^GB785,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if lotes_jugo.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,750,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.fecha),7,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end.
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            put control "^FO560,434^A0N,45,60^FD" string(tambores_lote.id_tambor,">99") + "/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".
            put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".
            put control "^FO600,810^A0N,45,45^FD" "17" "^FS".
            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,898^A0N,34,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") "^FS".
            put control "^FO25,938^GB355,0,3^FS".
            put control "^FO85,948^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,988^A0N,38,45^FD" "24 MONTH" "^FS".
                end.
            else
                do:
            put control "^FO85,920^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,960^A0N,38,45^FD" "24 MONTH" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1030^GB785,0,3^FS".
            put control "^FO358,1040^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1072^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1100^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO25,1150^GB785,0,3^FS".
            put control "^FO25,1155^GB785,0,3^FS". 
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            f = 1.
            do f = 1 to 30000.
    
            end.
        end.
   /***************ETIQUETA WILD******************************************************/     
       for each tambores_lote of lotes_jugo no-lock. 
        
                output to c:\lpt1.  
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO390,320^A0R,220,270^FD" "WILD"  "^FS".
                if lotes_jugo.id_articulo = 52 then
                    do:
                put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 401)" "^FS".                    
                    end.
                else
                    do:
                put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 268)" "^FS".
                    end.
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
               output close.
               f = 1.
               do f = 1 to 30000.
    
               end.
        end.
     /**************UNA MAS******************************************************************/   
        output to c:\lpt1.  
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO390,320^A0R,220,270^FD" "WILD"  "^FS".
                if lotes_jugo.id_articulo = 52 then
                    do:
                put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 401)" "^FS".                    
                    end.
                else
                    do:
                put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 268)" "^FS".
                    end.
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
    output close.
   f = 1.
   do f = 1 to 30000.
   end.
        
        
     /********FIN ETIQUETA WILD***********************/
end.



