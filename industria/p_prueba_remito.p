
DEFINE TEMP-TABLE tt_tambores
    FIELD id_empresa     AS INTEGER  COLUMN-LABEL "Empresa"
    FIELD id_sucursal     AS INTEGER  COLUMN-LABEL "Sucursal"
    FIELD id_tipotambor     AS INTEGER  COLUMN-LABEL "Tipotambor"
    FIELD nromov     AS INTEGER  COLUMN-LABEL "Nromov"
    FIELD id_lote     AS INTEGER  COLUMN-LABEL "Lote"
    FIELD id_tambor     AS INTEGER  COLUMN-LABEL "Tambor"
    FIELD id_articulo   AS INTEGER  COLUMN-LABEL "Articulo"
    FIELD id_calidad    AS INTEGER  COLUMN-LABEL "Calidad"
    FIELD id_envase     AS INTEGER  COLUMN-LABEL "Envase"
    FIELD kilos_tambor  AS DECIMAL  COLUMN-LABEL "Peso Neto"
    FIELD anio          AS INTEGER  COLUMN-LABEL "Año"
    FIELD elegido       AS LOGICAL  COLUMN-LABEL "Seleccionado" FORMAT "SI/" INITIAL FALSE
    FIELD id_reg        AS ROWID.

RUN wc_tambores_disponibles.w (INPUT 3, /*tipotambor */
                               INPUT 53, /* articulo */ 
                               INPUT 534, /* lote */
                               INPUT 2003, /* anio */
                               INPUT 95, /* sucursal */
                               input-OUTPUT TABLE tt_tambores).

FOR EACH tt_tambores WHERE elegido.
    DISP id_tambor.
END.
