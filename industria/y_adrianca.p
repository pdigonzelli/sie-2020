DEFINE VAR v_suc AS INTEGER.

v_suc = 
FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion = v_suc
                              AND tambores_industria.id_locacion_ubicacion = 4
                            BREAK BY tambores_industria.id_tipotambor
                                  BY tambores_industria.nromov
                                  BY tambores_industria.id_tambor.


END.
