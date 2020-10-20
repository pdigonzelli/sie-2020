/* 
/*------ rutina para llamados de proceso de remitos --------*/
find remitos where nro_comprobante = "007000001826" no-lock.
display nro_comprobante.
run y_gstkrem.p (input remitos.id_sucursal,
                 input remitos.id_tipo_movsto,
                 input remitos.nro,
                 input 1,
                 input 3)
                 "items_factura".
if return-value <> "" then do:
    message "Error en el Procesamiento de Remitos" view-as alert-box.
end.

/*------ rutina para llamados de anulacion de remitos --------*/
find remitos where nro_comprobante = "007000001826" no-lock.
display nro_comprobante.
run y_gstkrem.p (input remitos.id_sucursal,
                 input remitos.id_tipo_movsto,
                 input remitos.nro,
                 input 1,
                 input 4)
                 "items_factura".
if return-value <> "" then do:
    message "sadlajd" view-as alert-box.
end.
*/

/*
/*------ rutina para llamados de creacion de lotes --------*/
find lotes_jugo where
        lotes_jugo.id_empresa    = 1 and 
        lotes_jugo.id_sucursal   = 95 and
        lotes_jugo.id_tipotambor = 3 and
        lotes_jugo.nromov        = 22936
        no-lock no-error.
run y_gstkcre.p (input lotes_jugo.id_empresa,
                  input lotes_jugo.id_sucursal,
                  input lotes_jugo.id_tipotambor,
                  input lotes_jugo.nromov,
                  input 1)
                  "lotes_jugo".
if return-value <> "" then do:
    message "sadlajd" view-as alert-box.
end.





/*------ rutina para llamados de anulacion de lotes --------*/
find lotes_jugo where
        lotes_jugo.id_empresa    = 1 and 
        lotes_jugo.id_sucursal   = 95 and
        lotes_jugo.id_tipotambor = 3 and
        lotes_jugo.nromov        = 22936
        no-lock no-error.
run y_gstkcre.p (input lotes_jugo.id_empresa,
                  input lotes_jugo.id_sucursal,
                  input lotes_jugo.id_tipotambor,
                  input lotes_jugo.nromov,
                  input 2)
                  "lotes_jugo".
if return-value <> "" then do:
    message "sadlajd" view-as alert-box.
end.


*/
