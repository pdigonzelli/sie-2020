if userid("userdb") <> "computos" then
  do:
    find par_grupos where par_grupos.letra_inicial = substring(userid("userdb"),1,1) no-lock.
    find par_entorno of par_grupos no-lock.
    if session:window-system = "TTY" then
      do:
        if not (par_entorno.id_entorno = 1 or par_entorno.id_entorno = 3) then
          do:
            message "El m¢dulo de" par_grupos.nombre_grupo "no est  habilitado para este entorno de trabajo." view-as alert-box error.
            quit.
          end.
      end.
    else
      do:
        if not (par_entorno.id_entorno = 1 or par_entorno.id_entorno = 2) then
          do:
            message "El m¢dulo de" par_grupos.nombre_grupo "no est  habilitado para este entorno de trabajo." view-as alert-box error.
            quit.
          end.
      end.
    if par_grupos.estado = no then
      do:
         message "El m¢dulo de" par_grupos.nombre_grupo "ha sido deshabilitado por Gerencia de Sistemas." skip
                 "Si va a operar con ‚l, por favor, p¢ngase en contacto con el administrador del sistema."
                  view-as alert-box information.
      end.
  end.
