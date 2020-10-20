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

/*  message "Estoy por cargar los tambores de stock" view-as alert-box.  */

find comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.

/**************** TAMBORES LOTES DE JUGO CLARO *****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 53, input 1).

/**************** TAMBORES LOTES DE JUGO TURBIO ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 52, input 10).

/**************** TAMBORES LOTES DE JUGO LIMA ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 66, input 15).

/**************** TAMBORES LOTES DE CELL PULP DE LIMON *********************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 71, input 20).
     
/**************** TAMBORES LOTES DE CELL PULP DE NARANJA *******************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 70, input 25).

/******************************* TAMBORES PRODUCCION JUGO ******************************************/
run p_calculo_produccion_jugo_cacha.p (input p_suc).
/***************************************************************************************************/    

/******************************* TAMBORES DE TERCEROS **********************************************/
run p_calculo_stock_tercero.p (input p_suc).
/***************************************************************************************************/  

/**************** TAMBORES LOTES DE ACEITE ESENCIAL DE LIMON GRADO 2 *******************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 50, input 73).
/***************************************************************************************************/
                            
/**************** TAMBORES LOTES DE ACEITE ESENCIAL DE LIMON ***************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 51, input 74).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO EN FRIO DE LIMON ******************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 512, input 75).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 513, input 80).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 514, input 81).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 517, input 90).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 518, input 91).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 519, input 92).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESCERADO TEM. AMB. DE LIMON ****************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 520, input 93).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE NARANJA *************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 41, input 100).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE NARANJA *************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 44, input 101).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE OIL PHASE ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 57, input 110).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE OIL PHASE 2 ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 90, input 111).
/***************************************************************************************************/ 


/**************** TAMBORES LOTES DE WATER PHASE ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 58, input 105).
/***************************************************************************************************/ 

/**************** TAMBORES LOTES DE ACEITE POMELO ****************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 61, input 130).
/***************************************************************************************************/ 

/**************** TAMBORES LOTES DE TERPENO ********************************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 74, input 170).
/***************************************************************************************************/

/**************** TAMBORES LOTES DE ACEITE DESTILADO ***************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 76, input 180).
/***************************************************************************************************/

/**************** TAMBORES PRODUCCION DE ACEITE DESTILADO ***************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 761, input 190).
/***************************************************************************************************/ 

/**************** TAMBORES PRODUCCION DE ACEITE DESTILADO ***************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 762, input 193).
/***************************************************************************************************/ 

/**************** TAMBORES PRODUCCION DE ACEITE DESTILADO ***************************************/
run p_calculo_lotes_jugo_cacha.p (input p_suc, input 763, input 195).
/***************************************************************************************************/ 

/**************** TAMBORES FOLDEADO DE ART 582 ***************************************/
run p_calculo_lotes_jugo_CACHA.p (input p_suc, input 582, input 196).
/***************************************************************************************************/ 

/******************************* TAMBORES MATERIA PRIMA ACEITE *************************************/
run p_calculo_stock_mat_prima_aceite.p (input p_suc).
/***************************************************************************************************/ 

/********  LOS QUE TIENEN ES CERO LOS MANDO AL FONDO ******************/

for each stock_tambores where stock_tambores.orden_reporte = 0.
    assign stock_tambores.orden_reporte = 1000.
end.
