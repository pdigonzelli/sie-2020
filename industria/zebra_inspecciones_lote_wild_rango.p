define input parameter r as rowid.
define input parameter p_tam_desde as integer.
define input parameter p_tam_hasta as integer.
define input parameter p_wn as char.
DEFINE INPUT PARAMETER p_suc AS INTEGER.

define buffer tam for tambores_industria.
define var tambores as integer.
define var peso_neto as decimal format ">>9.99".
define var peso_neto_balde as decimal format ">>9.99".

define var f as integer.
define var gall as decimal.
define var peso_bruto as decimal format ">>9.99".
define var peso_bruto_balde as decimal format ">>9.99".

find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
find envases_prod of lotes_jugo no-lock no-error.

if available lotes_jugo then
do:

        for last tam of lotes_jugo by id_tambor.
         tambores = tam.id_tambor. 
        end.

                
        for each tambores_industria where tambores_industria.id_empresa = lotes_jugo.id_empresa 
                                      and tambores_industria.id_sucursal = lotes_jugo.id_sucursal
                                      and tambores_industria.id_tipotambor = lotes_jugo.id_tipotambor
                                      and tambores_industria.nromov = lotes_jugo.nromov 
                                      and tambores_industria.id_tambor >= p_tam_desde
                                      and tambores_industria.id_tambor <= p_tam_hasta no-lock
                                      by id_tambor desc.
/**********************************************************************************************************/
/*****************GENERO LA ETIQUETA NUMERO CERO***********************************************************/
            if tambores_industria.id_tambor = 1 then
             do:
                if p_suc = 96 then 
                    output to c:\lpt1.  
                else
                    output to \\192.168.2.11\zebra.
                   
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            if lotes_jugo.id_sucursal = 96 then
            do:  
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                           and r_productos_calidad.id_articulo = lotes_jugo.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice AS LOGICAL.
                    
                    CASE choice:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
            end.
            else
            do:
            
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 2: Ruta Provincial 38 -Tel: (0381) 4512650" "^FS".            
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                           and r_productos_calidad.id_articulo = lotes_jugo.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice95 AS LOGICAL.
                    
                    CASE choice95:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
                            
            end.
            
            
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
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.anio),3,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            /* if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end. */
                
            if lotes_jugo.id_envase = 527 then
                do:
                    peso_neto_balde = lotes_jugo.peso_neto.
                    peso_bruto_balde = lotes_jugo.peso_neto + 1.05.                    
                end.
            else
                do:
                    peso_neto = lotes_jugo.peso_neto.
                    peso_bruto = lotes_jugo.peso_neto + envases_prod.tara.    
                end.
               
                
            /* message "Peso Neto:" peso_neto "    Pero Bruto:" peso_bruto view-as alert-box.    */
                
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            if tambores > 99 then
                put control "^FO560,434^A0N,45,60^FD" "000/" + string(tambores,">99") "^FS".
            else 
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

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
                end.

            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,810^A0N,45,45^FD" "1.05" "^FS".
                end.
            else
                do:
                    put control "^FO600,810^A0N,45,45^FD" envases_prod.tara "^FS".
                end.

            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,898^A0N,34,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") "^FS".
            put control "^FO25,938^GB355,0,3^FS".
            put control "^FO85,948^A0N,28,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,975^A0N,28,45^FD" "24 MONTH" "^FS".
                end.
            else
                do:
            put control "^FO85,920^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,960^A0N,38,45^FD" "24 MONTH" "^FS".                
                end.    
                            
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            
            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
                end.
            
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
                        
            end.
           
            
/***************TERMINA LA ETIQUETA NUMERO CERO***********************************************************/            
/*********************************************************************************************************/            
                if p_suc = 96 then 
                    output to c:\lpt1.  
                else
                    output to \\192.168.2.11\zebra.

                 
            put control "^XA".
            put control "^LH0,0".
            
            if lotes_jugo.id_envase = 527 then    /* BIDONES PLASTICOS DE 22 LITROS */
                put control "^PQ" 1 "^FS".
            else 
                put control "^PQ" 2 "^FS".
            
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            if lotes_jugo.id_sucursal = 96 then
            do:  
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                           and r_productos_calidad.id_articulo = lotes_jugo.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice1 AS LOGICAL.
                    
                    CASE choice:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
            end.
            else
            do:
            
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 2: Ruta Provincial 38 -Tel: (0381) 4512650" "^FS".            
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                           and r_productos_calidad.id_articulo = lotes_jugo.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice195 AS LOGICAL.
                    
                    CASE choice195:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
                            
            end.            
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
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.anio),3,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
                            
            if lotes_jugo.id_envase = 527 then
                do:
                    peso_neto_balde = lotes_jugo.peso_neto.
                    peso_bruto_balde = lotes_jugo.peso_neto + 1.05.                    
                end.
            else
                do:
                    peso_neto = lotes_jugo.peso_neto.
                    peso_bruto = lotes_jugo.peso_neto + envases_prod.tara.    
                end.
            
                       
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            if tambores > 99 then
                put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,">99") + "/" + string(tambores,">99") "^FS".
            else
                put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,"999") + "/" + string(tambores,">99") "^FS".
            
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

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
                end.

            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,810^A0N,45,45^FD" "1.05" "^FS".
                end.
            else
                do:
                    put control "^FO600,810^A0N,45,45^FD" envases_prod.tara "^FS".
                end.

            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,898^A0N,34,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") "^FS".
            put control "^FO25,938^GB355,0,3^FS".
            put control "^FO85,948^A0N,28,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,975^A0N,28,45^FD" "24 MONTH" "^FS".
                end.
            else
                do:
            put control "^FO85,920^A0N,38,45^FD" "SHELF LIFE" "^FS".
            put control "^FO95,960^A0N,38,45^FD" "24 MONTH" "^FS".                
                end. 
            
            
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            
            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
                end.
            
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            if p_suc = 96 then
                do: 
                    f = 1.
                    do f = 1 to 30000.
    
                    end.
                end.
            else
                do:
                    f = 1.
                    do f = 1 to 5000.
    
                    end.
                
                end.
                
        end.
        
        
        /***************ETIQUETA WILD******************************************************/     
       for each tambores_industria where tambores_industria.id_empresa = lotes_jugo.id_empresa 
                                      and tambores_industria.id_sucursal = lotes_jugo.id_sucursal
                                      and tambores_industria.id_tipotambor = lotes_jugo.id_tipotambor
                                      and tambores_industria.nromov = lotes_jugo.nromov 
                                      and tambores_industria.id_tambor >= p_tam_desde
                                      and tambores_industria.id_tambor <= p_tam_hasta no-lock. 
        
               if p_suc = 96 then 
                    output to c:\lpt1.  
                else
                    output to \\192.168.2.11\zebra.

                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO390,320^A0R,220,270^FD" "WILD"  "^FS".
                if lotes_jugo.id_articulo = 52 then
                    do:
                        if p_wn = "" then
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 401)" "^FS".                    
                        else
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / " + p_wn + ")" "^FS".
                    end.
                else
                    do:
                        if p_wn = "" then
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 268)" "^FS".
                        else
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / " + p_wn + ")" "^FS".                    
                    end.
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
               output close.
               f = 1.
               do f = 1 to 30000.
    
               end.
        end. 
        
/*
     /**************UNA MAS******************************************************************/   
        output to c:\lpt1.  
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO390,320^A0R,220,270^FD" "WILD"  "^FS".
                if lotes_jugo.id_articulo = 52 then
                    do:
                        if p_wn = "" then
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 401)" "^FS".                    
                        else
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / " + p_wn + ")" "^FS".
                    end.
                else
                    do:
                        if p_wn = "" then
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / 268)" "^FS".
                        else
                            put control "^FO110,136^A0R,100,70^FD" "ARTICLE - NUMBER(3003 / " + p_wn + ")" "^FS".

                    end.
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
    output close.
   f = 1.
   do f = 1 to 30000.
   end.
        
  */      
     /********FIN ETIQUETA WILD***********************/

end.



