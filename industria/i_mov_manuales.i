assign tambores_industria.id_empresa_ubicacion = v_emp
       tambores_industria.id_sucursal_ubicacion = v_suc
       tambores_industria.id_locacion_ubicacion = v_loc
       tambores_industria.id_posicion_ubicacion = 1
       tambores_industria.c_usuario             = userid("userdb")
       tambores_industria.c_fecha               = today
       tambores_industria.c_hora                = string(time,"HH:MM:SS").
