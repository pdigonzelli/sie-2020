define input parameter v_orden_entrega as integer no-undo.
define var v_fob_ton as decimal no-undo.
define var v_fob_unitario as decimal no-undo.
DEFINE VAR v_fob_sin_comision AS DECIMAL NO-UNDO.

run p_calculo_fob_ton.p (input v_orden_entrega, output v_fob_ton, output v_fob_unitario).

v_fob_sin_comision = v_fob_ton - DECIMAL(orden_entrega.importe_comision:screen-value in frame F-Main).
orden_entrega.fob_ton:screen-value in frame F-Main = string(v_fob_sin_comision).
orden_entrega.fob_unitario:screen-value in frame F-Main = string(v_fob_unitario).
