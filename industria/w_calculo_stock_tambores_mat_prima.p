define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_articulo as integer.

for each stock_tambores.
    delete stock_tambores.
end.

/******************************* TAMBORES PRODUCCION JUGO ******************************************/
run p_calculo_produccion_jugo.p (input p_suc).
/***************************************************************************************************/ 

/*********************** CALCULO LOS TAMBORES DE PRODUCTOS DE TERCEROS    ***********************/
run p_calculo_stock_tercero.p (input p_suc).
/************************************************************************************************/


