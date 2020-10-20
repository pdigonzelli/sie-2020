define input parameter p_lista like listas_correo.id_lista.
define input parameter p_rowid_tabla as rowid.
define input parameter p_mensaje as char.

define var okay as logical.
define var v_subject as char.
define var v_body as char.
define var v_usuarios as char.
define var v_lotes as char.
define var v_orden as char.
define var i as integer.
DEFINE VAR vItemsContratos AS CHAR.

if p_lista = 1 or p_lista = 4  then /* MANEJO DE CONTRATOS */
  do:  
    find contratos where rowid(contratos) = p_rowid_tabla no-lock no-error.
    if available contratos then
        do:
            for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
                v_usuarios = v_usuarios + "," + usuarios_listas.email.            
            end.
            
            if v_usuarios <> "" then v_usuarios = substring(v_usuarios,2,length(v_usuarios) - 1).
            
            FOR EACH items_contratos OF contratos.
                vItemsContratos = vItemsContratos + "Parte " + STRING(items_contratos.ITEM) + 
                                  " en la semana " + STRING(items_contratos.semana_entrega) + 
                                  " a " + STRING(items_contratos.semana_entrega_hasta) + 
                                  " del año " + STRING(items_contratos.anio_semana_entrega) + CHR(10).
            END.

            vItemsContratos = "Las partes del contrato tienen las siguientes semanas de entrega: " + 
                              chr(10) + vItemsContratos.

            v_subject = "Contrato " + contratos.id_contrato.
            v_body = "Se ha " + p_mensaje + " el Contrato " + contratos.id_contrato + CHR(10) + CHR(10).
            v_body = v_body + vItemsContratos.
                    
            if v_usuarios <> "" then
                do:
                  RUN ..\industria\sendMail.p("",                               
                              2,                                
                              v_subject,
                              v_body, 
                              v_usuarios, 
                              ""). 
                       
                end.
        end. 
  end.
  
if p_lista = 2 then /* CREACION Y MANEJO DE ORDENES DE ENTREGA */
  do: 
    find orden_entrega where rowid(orden_entrega) = p_rowid_tabla no-lock no-error.
    if available orden_entrega then
        do:
            for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
                v_usuarios = v_usuarios + "," + usuarios_listas.email.            
            end.
            
            if v_usuarios <> "" then v_usuarios = substring(v_usuarios,2,length(v_usuarios) - 1).
    
            v_subject = "Orden de Entrega " + string(orden_entrega.id_orden_entrega)
                      + " de la semana " + string(orden_entrega.semana_entrega).
            
            find first items_contratos of orden_entrega no-lock no-error.
            if available items_contratos then
                do:
                    v_body = "Se ha " + p_mensaje + " la OE " + string(orden_entrega.id_orden_entrega) + chr(10) +
                             "Y esta vinculada a la parte " + string(items_contratos.item) + 
                             " del contrato " + items_contratos.id_contrato + 
                             " de la semana " + string(items_contratos.semana_entrega).
                end.
            else v_body = "Se ha " + p_mensaje + " la OE " + string(orden_entrega.id_orden_entrega) + "." +
                          chr(10) +
                          "Esta vinculada a la parte " + string(orden_entrega.item) + 
                          " del contrato " + orden_entrega.id_contrato + 
                          " de la semana " + string(orden_entrega.semana_entrega) + "." +
                          chr(10) +
                          "Pero revisar bien porque puede haber problemas....".
                    
            if v_usuarios <> "" then
                do:
                  RUN ..\industria\sendMail.p("",                               
                              2,                                
                              v_subject,
                              v_body, 
                              v_usuarios, 
                              "").                        
                end.
        end.  
  end.


if p_lista = 3 or p_lista = 10 then /* CUANDO SE REALACIONAN LOS LOTES CON LAS OE */
  do:
    find orden_entrega where rowid(orden_entrega) = p_rowid_tabla no-lock no-error.
    if available orden_entrega then
        do:
            for each tambores_industria of orden_entrega no-lock
                                        break by id_sucursal
                                              by id_lote
                                              by year(tambores_industria.fecha).
                i = i + 1.
                if last-of(year(tambores_industria.fecha)) then do:
                    if v_lotes = "" then do:
                        v_lotes = string(tambores_industria.id_lote) + "/" + 
                                  string(year(tambores_industria.fecha)) + "(" + 
                                  string (i) + " tambores)".
                    end.
                    else do:
                        v_lotes = v_lotes + chr(10) + string(tambores_industria.id_lote) + "/" + 
                                  string(year(tambores_industria.fecha)) + "(" + 
                                  string (i) + " tambores)".
                    end.
                    i = 0.
                end.
                
            end.
            for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
                v_usuarios = v_usuarios + "," + usuarios_listas.email.            
            end.
            
            if v_usuarios <> "" then v_usuarios = substring(v_usuarios,2,length(v_usuarios) - 1).
    
            v_subject = "Lotes de la OE " + string(orden_entrega.id_orden_entrega) 
                      + " de la semana " + string(orden_entrega.semana_entrega).
                      
            v_body = p_mensaje + " de los lote/s:" + chr(10) + 
                     v_lotes + chr(10) + " de la Orden Entrega " + string(orden_entrega.id_orden_entrega).
                    
            if v_usuarios <> "" then
                do:
                  RUN ..\industria\sendMail.p("S.A. San Miguel A.G.I.C y F",                               
                              2,                                
                              v_subject,
                              v_body, 
                              v_usuarios, 
                              "").                
                       
                end.
        end.
  end.
