DEFINE INPUT PARAMETER v_sucursal AS INTEGER.
DEFINE INPUT PARAMETER v_articulo AS INTEGER.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_lote AS INTEGER.
DEFINE VAR v_kilos_total_lote AS DECIMAL.

FOR EACH tambores_industria WHERE tambores_industria.id_articulo = v_articulo
                              AND tambores_industria.id_sucursal_ubicacion = v_sucursal
                              AND tambores_industria.id_locacion_ubicacion = 4
                            BREAK BY tambores_industria.nromov.
                            /* by facundo 30/04/07 */
                            /* 
                            NO-LOCK
                            BREAK BY tambores_industria.id_sucursal
                                  BY tambores_industria.id_articulo
                                  BY tambores_industria.id_envase
                                  BY tambores_industria.id_lote
                                  BY tambores_industria.anio. */
    
    
    IF FIRST-OF(tambores_industria.nromov)  THEN DO:
            v_tambores = 0.
            v_lote = 0.
    END.
    
    IF tambores_industria.id_lote = 0 THEN DO:
        v_lote      = tambores_industria.id_tambor.
        v_tambores  = 1.
        {i_prod_mat_prima_aceite_x_tambor.i}
    END.
    ELSE v_tambores = v_tambores + 1.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
        v_lote = tambores_industria.id_lote.
        {i_prod_mat_prima_aceite_x_tambor.i}
    END.
END.
