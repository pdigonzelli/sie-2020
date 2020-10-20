define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_articulo as integer.
define var v_envase as integer.

for each stock_tambores.
    delete stock_tambores.
end.

for each info_contratos.
    delete info_contratos.
end.


FIND FIRST comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.

/**************** TAMBORES LOTES DE JUGO CLARO *****************************************************/
run p_calculo_lotes_jugo_opt.p (input p_suc). 
 

/******************************* TAMBORES PRODUCCION JUGO ******************************************/
run p_calculo_produccion_jugo.p (input p_suc).
/***************************************************************************************************/    


/******************************* TAMBORES DE TERCEROS **********************************************/
run p_calculo_stock_tercero.p (input p_suc).
/***************************************************************************************************/  

/******************************* TAMBORES MATERIA PRIMA ACEITE *************************************/
run p_calculo_stock_mat_prima_aceite.p (input p_suc).
/***************************************************************************************************/ 


/********  LOS QUE TIENEN ES CERO LOS MANDO AL FONDO ******************/

for each stock_tambores where stock_tambores.orden_reporte = 0.
    assign stock_tambores.orden_reporte = 1000.
end.


