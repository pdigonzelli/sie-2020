define buffer mt for movimientos_tambores.
define buffer ti for tambores_industria.
define vari cant as integer.
define vari ncant as integer.

for each stock where  id_tipo_movimiento = 100  /* and  id_serial = 19078 */ .
    find mt where mt.id_empresa = stock.id_empresa and
                  mt.id_sucursal = stock.id_sucursal and
                  mt.id_tipotambor = stock.id_tipotambor and
                  mt.nromov_mov = stock.nromov
                      no-error.
    if available mt then do:
        find first ti where ti.id_empresa = mt.id_empresa and
                      ti.id_sucursal = mt.id_sucursal and
                      ti.id_tipotambor = mt.id_tipotambor and
                      ti.nromov        = mt.nromov
                      no-error.
        if not available ti then do:
            if mt.id_sucursal = 96 then do:
                find first ti where ti.id_empresa = mt.id_empresa and
                          ti.id_sucursal = 95 and
                          ti.id_tipotambor = mt.id_tipotambor and
                          ti.nromov        = mt.nromov
                          no-error.
                if available ti then do:
                    cant = cant  + 1.
                    display ti with frame sss title "95".
                    pause 0.
                 assign mt.id_sucursal    = 95
                        stock.id_sucursal = 95.   

                end.                    
/*                                else ncant = ncant + 1.     */
            end.                
            else
            if mt.id_sucursal = 95 then do:
                find first ti where ti.id_empresa = mt.id_empresa and
                          ti.id_sucursal = 96 and
                          ti.id_tipotambor = mt.id_tipotambor and
                          ti.nromov        = mt.nromov
                          no-error.
                if available ti then do:
                 cant = cant  + 1.

                 displa  ti with frame lsls 1 columns title "96".
                 assign mt.id_sucursal    = 96
                        stock.id_sucursal = 96.   
                END.                 
/*                                else ncant = ncant + 1. */
            end.
            else display mt.id_sucursal.                            
            if not available ti then do:
                find first ti where 
                          ti.id_empresa = mt.id_empresa and
/*                          ti.id_sucursal = 96 and  */
                          ti.id_tipotambor = mt.id_tipotambor and
                          ti.nromov        = mt.nromov
                          no-error. 
                if available ti then  cant = cant  + 1.
                                else ncant = ncant + 1. 
                              
               display mt.id_articulo mt.id_sucursal ti.id_sucursal when                     available ti with frame slsls title "nose".
                    
            
            end.                
        end.
        else do:
            cant = cant + 1.
/*            display ti with frame esta title "esta" 1 columns.
            wait-for return of current-window. */
        end.            
    end.           
                  
end.
display cant ncant.