DEFINE INPUT PARAMETER p_cliente AS INTEGER.
DEFINE INPUT PARAMETER p_fecha_desde AS DATE.
DEFINE INPUT PARAMETER p_fecha_hasta AS DATE.
DEFINE INPUT PARAMETER p_suc_remito AS INTEGER.

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

FOR EACH contratos WHERE contratos.id_cliente = p_cliente NO-LOCK.
    {i_desp_industria_x_cliente.i}
END.
