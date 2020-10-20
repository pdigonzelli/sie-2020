do:
  if not transaction then
    do:
      assign prgmen[1] = self:private-data
             prgmen[2] = self:private-data.
      if index(prgmen[1],".p") > 0 then
       do:
          prgmen[2] = replace(prgmen[1],".p",".r").
          assign l-es_persis = no.  /*-- EL PROGRAMA CORRE EN FORMA NO PERSISTENTE --*/
       end.
       
      if index(prgmen[1],".w") > 0 then
       do:
          prgmen[2] = replace(prgmen[1],".w",".r").
          assign l-es_persis = yes. /*-- EL PROGRAMA CORRE EN FORMA PERSISTENTE --*/
       end.
       
      if search(prgmen[1]) <> ? or search(prgmen[2]) <> ? then
       do:
          if l-es_persis then
           do:
              run s_mail.p.
              if self:name <> "" and self:name <> ? then
                message self:name.
              on return return.
              if not valid-handle(h_prog1) then
                do:
                  run value(prgmen[1]) persistent set h_prog1.
                  run initializeObject in h_prog1.
                end.
              else
                if not valid-handle(h_prog2) then
                  do:
                    run value(prgmen[1]) persistent set h_prog2.
                    run initializeObject in h_prog2.
                  end.
                else
                  if not valid-handle(h_prog3) then
                    do:
                      run value(prgmen[1]) persistent set h_prog3.
                      run initializeObject in h_prog3.
                    end.
                  else
                    message "Demasiadas Opciones de Men£ Instanaciadas !!!" skip 
                            "Cierre Algunas para Continuar!!!" view-as alert-box information.
              on return return.
              run actualiza_datos.
              /*
              ventana:window-state = 2.
              apply "ENTRY" to ventana.
              */
           end.
          else
           do:
              run s_mail.p.
              run disable_menu.
              run disable_buttons.
              if self:name <> "" and self:name <> ? then
                message self:name.
              on return return.
              run value(prgmen[1]).
              on return return.
              run enable_menu.
              run enable_buttons.
              run actualiza_datos.
              apply "ENTRY" to ventana.
           end.
       end.
      else
          message "El Programa" prgmen[1] "No Existe"
                  view-as alert-box information.
    end.
  else
      message "Para Continuar, Confirme o Cancele la Transacci¢n Activa" 
              view-as alert-box error title " Atenci¢n ".
end.
