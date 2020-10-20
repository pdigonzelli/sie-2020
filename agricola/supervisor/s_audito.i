assign {1}.c_usuario = (if userid("userdb") <> "" then userid("userdb") else program-name(1))
       {1}.c_fecha   = today
       {1}.c_hora    = string(time,"HH:MM:SS").
