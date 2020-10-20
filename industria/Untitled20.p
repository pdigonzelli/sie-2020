FOR EACH tambores_industria WHERE id_tipotambor = 4
                              AND (id_lote = 513 OR id_lote = 503 OR id_lote = 523)
                              AND anio = 2004
                              AND id_articulo = 524
                              BY id_lote
                              BY id_tambor.
    DISP id_tambor id_lote fecha c_usuario.
    /*ASSIGN tambores_industria.fecha = date(STRING(DAY(tambores_industria.fecha)) + "/" +
                                      STRING(MONTH(tambores_industria.fecha)) + "/2004").*/
END.
