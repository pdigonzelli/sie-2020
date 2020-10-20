DEFINE VAR v_articulo AS INTEGER.
DEFINE VAR v_cantidad AS INTEGER.
DEFINE VAR v_anio AS INTEGER.


v_anio = 2002.
v_cantidad = 0.

OUTPUT TO c:\temp\ver_total.txt.


FOR EACH tambores_industria WHERE (tambores_industria.id_articulo = 52 OR
                                   tambores_industria.id_articulo = 53 OR
                                   tambores_industria.id_articulo = 71)
                              AND tambores_industria.id_tipotambor = 3 
                              AND YEAR(tambores_industria.fecha) = v_anio 
                              NO-LOCK
                              BREAK BY id_lote:
    v_cantidad = v_cantidad + 1.
    IF LAST-OF(id_lote) THEN DO:
        EXPORT DELIMITER ";" tambores_industria.fecha id_tipotambor id_articulo id_lote kilos_tambor v_cantidad.
        v_cantidad = 0.
    END.
END.

FOR EACH tambores_industria WHERE (tambores_industria.id_articulo = 51 /* aceite */ OR 
                                   tambores_industria.id_articulo = 50 /* aceite grado 2 */ OR
                                   tambores_industria.id_articulo = 57 /* water phase */ OR 
                                   tambores_industria.id_articulo = 58 /* oil phase */ OR 
                                   tambores_industria.id_articulo = 74 /* terpeno */ OR 
                                   tambores_industria.id_articulo = 76 /* destilado */)
                              AND tambores_industria.id_tipotambor = 6 
                              AND year(tambores_industria.fecha) = v_anio 
                              NO-LOCK
                              BREAK BY id_lote:
    v_cantidad = v_cantidad + 1.
    IF LAST-OF(id_lote) THEN DO:
        EXPORT DELIMITER ";" tambores_industria.fecha id_tipotambor id_articulo id_lote kilos_tambor v_cantidad.
        v_cantidad = 0.
    END.
END.

OUTPUT CLOSE.
