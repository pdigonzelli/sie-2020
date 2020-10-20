DEFINE INPUT PARAMETER p_suc AS INTEGER.
DEFINE INPUT PARAMETER p_articulo AS INTEGER.
DEFINE INPUT PARAMETER p_tipotambor AS INTEGER.
DEFINE INPUT PARAMETER p_calidad AS INTEGER.
DEFINE INPUT PARAMETER p_descripcion AS CHAR.

DEFINE VAR v_articulo AS INTEGER.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_anio AS INTEGER.

v_articulo = p_articulo.
v_tambores = 0.
v_kilos = 0.
v_anio = 0.

FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND (IF p_tipotambor <> 0 THEN tambores_industria.id_tipotambor = p_tipotambor ELSE TRUE)
                                      AND tambores_industria.id_articulo            = v_articulo.

    v_tambores  = v_tambores + 1.
    v_kilos     = v_kilos + tambores_industria.kilos_tambor.
    v_anio      = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)).
END.

/* MESSAGE "Tambores " v_tambores " articulo " v_articulo VIEW-AS ALERT-BOX. */

IF v_tambores >= 1 THEN DO:
    FIND FIRST sucursales WHERE sucursales.id_sucursal = p_suc no-lock no-error.
    FIND FIRST envases_prod WHERE envases_prod.id_envase = 501 no-lock no-error.
                                                                
    CREATE stock_tambores.
    ASSIGN stock_tambores.id_empresa            = 1
           stock_tambores.id_sucursal           = p_suc
           stock_tambores.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
           stock_tambores.id_sucursal_ubicacion = p_suc 
           stock_tambores.id_lote               = 0 
           stock_tambores.id_tipotambor         = 2
           stock_tambores.anio_lote             = v_anio
           stock_tambores.id_envase             = 501
           stock_tambores.envase                = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
           stock_tambores.id_articulo           = v_articulo
           stock_tambores.articulo              = p_descripcion
           stock_tambores.calidad               = p_descripcion
           stock_tambores.id_calidad            = p_calidad
           stock_tambores.tambores              = v_tambores
           stock_tambores.kilos                 = v_kilos
           stock_tambores.orden_reporte         = 1000.
END.
