FOR EACH tambores_industria WHERE id_tipotambor = 6
                              AND id_articulo   = 51
                              AND anio          = 2002 
                              AND id_sucursal_ubicacion = 95
                              AND id_locacion_ubicacion = 4 
                              AND id_lote       = 547
                              BREAK BY nro_remito
                                    BY id_tambor.
    /*IF FIRST-OF (nro_remito) THEN  DO:
        FIND FIRST remitos WHERE remitos.nro = tambores_industria.nro_remito NO-LOCK NO-ERROR.
        IF AVAILABLE remitos THEN DISP remitos.nro_comp remitos.fecha remitos.c_usuario.
        FIND FIRST items_factura OF remitos NO-LOCK NO-ERROR.
        IF AVAILABLE items_factura THEN DO:
        DISP desde_lote hasta_lote.
        END.
        
    END.*/
    
    DISP id_tambor 
         id_sucursal_remito
         nro_remito
         id_sucursal_ubicacion
         id_locacion_ubicacion
         .
    /*
    UPDATE id_sucursal_remito.
      
    FIND FIRST lotes_aceite WHERE lotes_aceite.nromov = tambores_industria.nromov_destino.
    DISP lotes_aceite.id_lote lotes_aceite.c_fecha.
      */                                                   
    
    RUN ../industria/y_gstkrprod.p (input tambores_industria.id_empresa,
                                    input tambores_industria.id_sucursal,
                                    input tambores_industria.id_tipotambor,
                                    input tambores_industria.nromov,
                                    INPUT tambores_industria.id_tambor,
                                    input tambores_industria.id_tambor,
                                    INPUT 950,
                                    input 95,
                                    input 10).  

    ASSIGN id_articulo = 51
           c_usuario = "cacha"
           c_fecha = TODAY.  


/*
    RUN ../industria/y_gstkrprod.p (input tambores_industria.id_empresa,
                                                          input tambores_industria.id_sucursal,
                                                          input tambores_industria.id_tipotambor,
                                                          input tambores_industria.nromov,
                                                          input tambores_industria.id_tambor,
                                                          input tambores_industria.id_tambor,
                                                          input 950,
                                                          input 95,
                                                          input 9).  
                                                          */
      
END.
