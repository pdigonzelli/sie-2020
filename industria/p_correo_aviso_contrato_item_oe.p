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

if p_lista = 1 or p_lista = 4  then /* MANEJO DE CONTRATOS */
  do:  
    find contratos where rowid(contratos) = p_rowid_tabla no-lock no-error.
    if available contratos then
        do:
            for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
                v_usuarios = v_usuarios + "," + usuarios_listas.email.            
            end.
            
            if v_usuarios <> "" then v_usuarios = substring(v_usuarios,2,length(v_usuarios) - 1).
    
            v_subject = "Contrato " + contratos.id_contrato.
            v_body = "Se ha " + p_mensaje + " el Contrato " + contratos.id_contrato.
                    
            if v_usuarios <> "" then
                do:
                RUN ..\industria\sendMail.p(INPUT "",                       /* SIEMPRE TIENE QUE IR */
                                            INPUT 2,                                                   /* PRIORIDAD */
                                            INPUT v_subject,                                           /* SUBJECT */
                                            INPUT v_body,                                              /* BODY     */
                                            INPUT v_usuarios,                                          /* DEST. SEP COMAS */
                                            INPUT ""                                      /* ARCHIVOS ATTACHED SEP POR COMAS */
                                            ).
                       
                end.
        end. 
  end.
  
if p_lista = 2 then /* CREACION Y MANEJO DE ORDENES DE ENTREGA */
  do: 
    find items_orden_entrega where rowid(items_orden_entrega) = p_rowid_tabla no-lock no-error.
    if available items_orden_entrega then
        do:
            for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
                v_usuarios = v_usuarios + "," + usuarios_listas.email.            
            end.
            
            if v_usuarios <> "" then v_usuarios = substring(v_usuarios,2,length(v_usuarios) - 1).
    
            v_subject = "Orden de Entrega " + string(items_orden_entrega.id_orden_entrega)
                      + " parte " + STRING(items_orden_entrega.ITEM_oe)
                      + " de la semana " + string(items_orden_entrega.semana_entrega).
            
            find first items_contratos of items_orden_entrega no-lock no-error.
            if available items_contratos then
                do:
                    v_body = "Se ha " + p_mensaje + " la OE " + string(items_orden_entrega.id_orden_entrega)
                             + " parte " + STRING(items_orden_entrega.ITEM_oe)
                             + chr(10) +
                             "Y esta vinculada a la parte " + string(items_contratos.item) + 
                             " del contrato " + items_contratos.id_contrato + 
                             " de la semana " + string(items_contratos.semana_entrega).
                end.
            else v_body = "Se ha " + p_mensaje + " la OE " + string(items_orden_entrega.id_orden_entrega) 
                        + " parte " + STRING(items_orden_entrega.ITEM_oe)
                            + "." +
                          chr(10) +
                          "Esta vinculada a la parte " + string(items_orden_entrega.item) + 
                          " del contrato " + items_orden_entrega.id_contrato + 
                          " de la semana " + string(items_orden_entrega.semana_entrega) + "." +
                          chr(10) +
                          "Pero revisar bien porque puede haber problemas....".
                    
            if v_usuarios <> "" then
                do:
                RUN SendMail.p(INPUT "",                       /* SIEMPRE TIENE QUE IR */
                               INPUT 2,                                                   /* PRIORIDAD */
                               INPUT v_subject,                                           /* SUBJECT */
                               INPUT v_body,                                              /* BODY     */
                               INPUT v_usuarios,                                          /* DEST. SEP COMAS */
                               INPUT ""                                      /* ARCHIVOS ATTACHED SEP POR COMAS */
                              ).
                       
                end.
        end.  
  end.


if p_lista = 3 or p_lista = 10 then /* CUANDO SE REALACIONAN LOS LOTES CON LAS OE */
  do:
    find items_orden_entrega where rowid(items_orden_entrega) = p_rowid_tabla no-lock no-error.
    if available items_orden_entrega then
        do:
            for each tambores_industria of items_orden_entrega no-lock
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
    
            v_subject = "Lotes de la parte " + STRING(items_orden_entrega.ITEM_oe) + " de la OE " 
                      + string(items_orden_entrega.id_orden_entrega) 
                      + " de la semana " + string(items_orden_entrega.semana_entrega).
                      
            v_body = p_mensaje + " de los lote/s:" + chr(10) + 
                     v_lotes + chr(10) + " de la parte " + STRING(items_orden_entrega.ITEM_oe) + 
                     " de la Orden Entrega " + string(items_orden_entrega.id_orden_entrega).
                    
            if v_usuarios <> "" then
                do:
                RUN SendMail.p(INPUT "S.A. San Miguel A.G.I.C y F",                       /* SIEMPRE TIENE QUE IR */
                               INPUT 2,                                                   /* PRIORIDAD */
                               INPUT v_subject,                                           /* SUBJECT */
                               INPUT v_body,                                              /* BODY     */
                               INPUT v_usuarios,                                          /* DEST. SEP COMAS */
                               INPUT ""                                      /* ARCHIVOS ATTACHED SEP POR COMAS */
                              ).
                       
                end.
        end.
  end.