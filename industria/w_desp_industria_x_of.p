define input parameter p_r_contratos AS ROWID.

define var v_lote as integer format ">>>9".
define var v_anio_lote as integer format "9999".
define var total_tambores as integer.
define var total_kilos as decimal.
DEFINE VAR v_con LIKE contratos.id_contrato.
DEFINE VAR v_tip LIKE contratos.id_tipo_contrato.
DEFINE VAR v_ani LIKE contratos.anio.
DEFINE BUFFER b_tambores FOR tambores_industria.

                
for each despachos_industria.
    delete despachos_industria.
end.

FIND FIRST contratos WHERE ROWID(contratos) = p_r_contratos NO-LOCK.
 {i_desp_industria_x_of.i}
