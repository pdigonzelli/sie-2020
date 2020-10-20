DISPLAY STRING(TIME,"hh:mm:ss").
FOR EACH contratos NO-LOCK.
    
    FOR EACH tambores_industria NO-LOCK WHERE
    tambores_industria.id_contrato_of      = contratos.id_contrato AND 
    tambores_industria.id_tipocontrato_of  = contratos.id_tipo_contrato AND
    tambores_industria.anio_of             = contratos.anio , 
    FIRST LOTES_JUGO OF TAMBORES_INDUSTRIA WHERE LOTES_JUGO.ESTADO_LOTE >= 2 NO-LOCK
    BREAK BY tambores_industria.nromov.

        ACCUM TAMBORES_INDUSTRIA.id_tipotambor ( COUNT ).
    END.

END.

DISPLAY COUNT STRING(TIME,"hh:mm:ss").

