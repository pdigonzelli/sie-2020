DEFINE VAR hp AS HANDLE NO-UNDO.
DEFINE BUFFER lotes_ubi FOR lotes_ubicacion.


RUN libLotesUbicacion.p PERSISTENT SET hp.
/*
FOR EACH lotes_aceite WHERE fecha >= DATE("01/01/03") NO-LOCK ,
     FIRST tambores_industria OF lotes_aceite NO-LOCK .


        RUN getLoteUbicacion IN hp 
            (lotes_aceite.id_empresa , 
             lotes_aceite.id_sucursal , 
             lotes_aceite.id_tipotambor , 
             lotes_aceite.nromov , 
             tambores_industria.id_sucursal_ubicacion,
             BUFFER lotes_ubi) NO-ERROR.
        
        
        IF RETURN-VALUE = "" THEN
            DISP lotes_ubi.id_sucursal_ubicacion.
        ELSE
            DO:
                RUN createFromLoteIndustria IN hp 
                    (ROWID(lotes_aceite)) NO-ERROR.
                DISPLAY RETURN-VALUE.
            END.
      PAUSE 0.
END.

FOR EACH lotes_jugo WHERE fecha >= DATE("01/01/03") NO-LOCK ,
     FIRST tambores_industria OF lotes_jugo NO-LOCK .


        RUN getLoteUbicacion IN hp 
            (lotes_jugo.id_empresa , 
             lotes_jugo.id_sucursal , 
             lotes_jugo.id_tipotambor , 
             lotes_jugo.nromov , 
             tambores_industria.id_sucursal_ubicacion,
             BUFFER lotes_ubi) NO-ERROR.
        
        
        IF RETURN-VALUE = "" THEN
            DISP lotes_ubi.id_sucursal_ubicacion.
        ELSE
            DO:
                RUN createFromLoteIndustria IN hp 
                    (ROWID(lotes_jugo)) NO-ERROR.
                DISPLAY RETURN-VALUE.
            END.
      PAUSE 0.
END.
*/
MESSAGE "TRANSFERENCIA" VIEW-AS ALERT-BOX.

FOR EACH lotes_ubicacion WHERE id_sucursal_ubicacion = 85 NO-LOCK.
    RUN transferenciaLoteUbicacion IN hp (1 , lotes_ubicacion.id_sucursal , lotes_ubicacion.id_tipotambor , 
                                          lotes_ubicacion.nromov , 85 , 88 , 
                                          TODAY , lotes_ubicacion.cantidad / 2) NO-ERROR.
    IF RETURN-VALUE <> "" THEN
        UNDO , LEAVE.
END.
