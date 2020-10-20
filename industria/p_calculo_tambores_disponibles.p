DEFINE TEMP-TABLE tt_tambores 
    FIELD id_sucursal AS INTEGER COLUMN-LABEL "Suc" FORMAT ">>9"
    FIELD id_lote AS INTEGER COLUMN-LABEL "Lote" FORMAT ">>>9"
    FIELD anio AS INTEGER COLUMN-LABEL "Año" FORMAT ">>>9"
    FIELD id_articulo AS INTEGER COLUMN-LABEL "Art." FORMAT ">>9"
    FIELD articulo AS CHAR COLUMN-LABEL "Producto" FORMAT "x(25)"
    FIELD id_tambor AS INTEGER COLUMN-LABEL "Tambor" FORMAT ">>>9"
    FIELD kilos_tambor AS DECIMAL COLUMN-LABEL "Peso" FORMAT ">>>>9.99"
    FIELD fecha_cierre AS DATE COLUMN-LABEL "Fecha" FORMAT "99/99/99"
    FIELD id_row AS ROWID.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_tambores.
DEFINE INPUT PARAMETER articulo as integer.
DEFINE INPUT PARAMETER fecha as date.
DEFINE INPUT PARAMETER sucursal AS INTEGER.
DEFINE INPUT PARAMETER v_tambor_desde as integer.
DEFINE INPUT PARAMETER v_tambor_hasta as integer.
DEFINE INPUT PARAMETER v_art AS INTEGER.
DEFINE INPUT PARAMETER v_lote AS INTEGER.
DEFINE INPUT PARAMETER v_suc AS INTEGER.

define var i as integer no-undo.
define var nombre-item as character no-undo.




FOR EACH tt_tambores.
    DELETE tt_tambores.
END.

FOR EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_lote = articulo NO-LOCK.
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = 
                                          origenes_materia_prima.id_articulo_mp
                                    NO-LOCK NO-ERROR.
    FOR EACH tambores_industria WHERE tambores_industria.id_articulo = origenes_materia_prima.id_articulo_mp
                              AND tambores_industria.id_empresa_destino      = 0
                              AND tambores_industria.id_sucursal_destino     = 0
                              AND tambores_industria.id_tipotambor_destino   = 0
                              AND tambores_industria.nromov_destino          = 0
                              AND tambores_industria.Fecha_cierre           <= fecha
                              AND tambores_industria.id_locacion_ubicacion   = 4
                              AND tambores_industria.id_sucursal_ubicacion   = sucursal
                              AND (IF v_tambor_desde = 0 
                                    OR v_tambor_hasta = 0 THEN TRUE 
                                                          ELSE tambores_industria.id_tambor >= v_tambor_desde)
                              AND (IF v_tambor_desde = 0 
                                    OR v_tambor_hasta = 0 THEN TRUE 
                                                          ELSE tambores_industria.id_tambor <= v_tambor_hasta)
                              AND (IF v_art <> 0 THEN tambores_industria.id_articulo = v_art 
                                                 ELSE TRUE)
                              AND (IF v_lote <> 0 THEN tambores_industria.id_lote = v_lote 
                                                 ELSE TRUE) 
                              
                              AND (IF v_suc <> 0 THEN tambores_industria.id_sucursal = v_suc 
                                                 ELSE TRUE)
                             NO-LOCK.
        CREATE tt_tambores.
        ASSIGN tt_tambores.id_sucursal = tambores_industria.id_sucursal
               tt_tambores.id_lote = tambores_industria.id_lote
               tt_tambores.anio    = tambores_industria.anio
               tt_tambores.id_articulo = tambores_industria.id_articulo
               tt_tambores.articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
               tt_tambores.id_tambor = tambores_industria.id_tambor
               tt_tambores.kilos_tambor = tambores_industria.kilos_tambor
               tt_tambores.fecha_cierre = tambores_industria.fecha_cierre
               tt_tambores.id_row = ROWID(tambores_industria).
    END.
END.
