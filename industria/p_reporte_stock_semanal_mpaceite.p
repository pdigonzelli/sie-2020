DEFINE VAR v_kilos AS INTEGER.

v_kilos = 0.

FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_locacion_ubicacion  = 4
                                      AND tambores_industria.id_tipotambor          = 2
                                      AND (tambores_industria.id_articulo           = 511
                                        OR tambores_industria.id_articulo           = 59
                                        OR tambores_industria.id_articulo           = 571
                                        OR tambores_industria.id_articulo           = 581)
                                      BREAK BY tambores_industria.id_sucursal_ubicacion
                                            BY tambores_industria.id_articulo
                                            BY tambores_industria.anio.
                                          
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
        
    IF LAST-OF(tambores_industria.anio) THEN DO:
        
        CREATE reporte_stock_semanal.
        ASSIGN reporte_stock_semanal.id_reporte     = 1
               reporte_stock_semanal.id_sucursal    = tambores_industria.id_sucursal_ubicacion
               reporte_stock_semanal.id_articulo    = tambores_industria.id_articulo
               reporte_stock_semanal.id_contrato    = ""
               reporte_stock_semanal.anio           = tambores_industria.anio
               reporte_stock_semanal.kilos          = v_kilos
               reporte_stock_semanal.kilos_400      = 0.
               
        v_kilos = 0.
    END.
END.





/*
/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE NARANJA Y MANDARINA (CODIGO 44)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 44, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 608 , /* CALIDAD */
                                            INPUT "Ac.Nar.Mand." /* DESCRIPCION */
                                               ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE WATER DE NARANJA (CODIGO 48)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 48, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 611 , /* CALIDAD */
                                            INPUT "Pro.WaterNar" /* DESCRIPCION */
                                               ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE GRADO 2 DE PRODUCCION (CODIGO 50)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 50, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 602 , /* CALIDAD */
                                            INPUT "AceiteGrado2" /* DESCRIPCION */
                                               ).

/*********************** CALCULO LOS TAMBORES DE POT DE FOLDEADO (CODIGO 59)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 59).

/*********************** CALCULO LOS TAMBORES DE ACEITE ESENCIAL DE LIMA (CODIGO 73)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 73, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 607 , /* CALIDAD */
                                            INPUT "Aceite Lima" /* DESCRIPCION */
                                               ).

/************************* CALCULO LOS TAMBORES DE ACEITE DESTILADO (CODIGO 76)  **************/
v_kilos = 0.
v_tambores = 0.
v_articulo = 76.
v_anio      = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    v_anio = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)).
        
end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = v_anio
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Aceite Dest."
                   stock_tambores.calidad        = "Aceite Dest."
                   stock_tambores.id_calidad     = 606
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = (v_tambores * 170.184)
                   stock_tambores.orden_reporte  = 180.
        end.  

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE NARANJA (CODIGO 411)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 411, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 620 , /* CALIDAD */
                                            INPUT "Prd.Ac.Nar." /* DESCRIPCION */
                                           ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE PROD GRADO 2 (CODIGO 501)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 501, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 622 , /* CALIDAD */
                                            INPUT "AcLimon Gr 2" /* DESCRIPCION */
                                               ).

/*********************** CALCULO LOS TAMBORES DE produccion de aceite de ralladora (CODIGO 509)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 509).
/***************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE produccion de aceite de ralladora (CODIGO 510)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 510).
/***************************************************************************************************/

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE (CODIGO 511)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc, /* SUCURSAL */
                                            INPUT 511, /* ARTICULO */
                                            INPUT 2 , /* TIPOTAMBOR */
                                            INPUT 602 , /* CALIDAD */
                                            INPUT "Prod.Aceite" /* DESCRIPCION */
                                           ).
/***************************************************************************************************/

/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA LIMPIA (CODIGO 513)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 513 ,
                                            INPUT 6 ,
                                            INPUT 616 ,
                                            INPUT "Borra Limpia"
                                               ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA P/LIMPIAR (CODIGO 514)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 514 ,
                                            INPUT 8 ,
                                            INPUT 617 ,
                                            INPUT "Bo.P/Limpiar"
                                               ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA FILTRADA (CODIGO 516)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 516 ,
                                            INPUT 2 ,
                                            INPUT 618 ,
                                            INPUT "Bo. Filtrada"
                                               ).

/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA SOBRANTE FILTRADA DE BORRA (CODIGO 518)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 518 ,
                                            INPUT 0 ,
                                            INPUT 619 ,
                                            INPUT "Sob.Bo.Filt."
                                               ).

/****** CALCULO LOS TAMBORES DE BORRA DESCERADO EN FRIO (CODIGO 519)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 519 ,
                                            INPUT 0 ,
                                            INPUT 627 ,
                                            INPUT "BorraDesFrio"
                                               ).

/****** CALCULO LOS TAMBORES DE BORRA DESCERADA A TEMPERATURA AMBIENTE (CODIGO 520)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 520 ,
                                            INPUT 0 ,
                                            INPUT 628 ,
                                            INPUT "BorraDesTemA"
                                               ).

/*********************** CALCULO LOS TAMBORES DE PRODUCCION DE OIL PHASE (CODIGO 571)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 571 ,
                                            INPUT 2 ,
                                            INPUT 600 ,
                                            INPUT "Prd.OilPhase"
                                               ).

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE WATER PHASE (CODIGO 581)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 581 ,
                                            INPUT 2 ,
                                            INPUT 601 ,
                                            INPUT "Prd.WaterPh"
                                               ).

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE WATER PHASE (CODIGO 582)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 582 ,
                                            INPUT 2 ,
                                            INPUT 623 ,
                                            INPUT "Water Conce"
                                               ).

/*********************** CALCULO LOS TAMBORES DE TERPENO DE FOLDEADO (CODIGO 741)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 741).
/***************************************************************************************************/

/*************** CALCULO LOS TAMBORES DE PRODUCCION DE TERPENO ALTO BETAMINENO (CODIGO 742)  ********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 742 ,
                                            INPUT 2 ,
                                            INPUT 624 ,
                                            INPUT "Terp.A.Beta"
                                               ).

/******************* CALCULO LOS TAMBORES DE PRODUCCION DE TERPENO BAJO CITRAL (CODIGO 743)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 743 ,
                                            INPUT 2 ,
                                            INPUT 625 ,
                                            INPUT "Terp.B.Citr"
                                               ).

/******************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DESTILADO (CODIGO 762)  **************/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 762 ,
                                            INPUT 2 ,
                                            INPUT 625 ,
                                            INPUT "AcDestilado"
                                               ).

/****** CALCULO LOS TAMBORES DE SOBRANTE DE ACEITE DESTILADO (CODIGO 763)  ***********/
RUN p_cal_tam_mat_prima_aceite_universal.p (INPUT p_suc ,
                                            INPUT 763 ,
                                            INPUT 6 ,
                                            INPUT 625 ,
                                            INPUT "BorraAcDest"
                                               ).
/*********************** CALCULO LOS TAMBORES DE BORRA (CODIGO 744)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 744).
/***************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE BORRA (CODIGO 578)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 578).
/***************************************************************************************************/
  */
