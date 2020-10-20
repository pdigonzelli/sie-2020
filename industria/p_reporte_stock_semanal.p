DEFINE VAR tiempo AS INTEGER.
DEFINE TEMP-TABLE rep_stock_semanal
    FIELD id_reporte AS INTEGER
    FIELD id_sucursal AS INTEGER
    FIELD id_articulo AS INTEGER
    FIELD id_contrato AS CHAR
    FIELD anio AS INTEGER
    FIELD kilos AS DECIMAL
    FIELD kilos_400 AS DECIMAL.



for each reporte_stock_semanal.
    delete reporte_stock_semanal.
end.



run p_reporte_stock_semanal_lotes.p. /****** TAMBORES LOTES JUGO Y ACEITE  *****/

run p_reporte_stock_semanal_mpjugo.p. /****** TAMBORES PRODUCCION JUGO *********/

/*run p_reporte_stock_semanal_tercero.p. /****** TAMBORES DE TERCEROS *******/*/

run p_reporte_stock_semanal_mpaceite.p. /****** TAMBORES MATERIA PRIMA ACEITE ******/

FOR EACH reporte_stock_semanal.
    CREATE rep_stock_semanal.
    BUFFER-COPY reporte_stock_semanal TO rep_stock_semanal.
END.

RUN generateExcel.p (INPUT TABLE rep_stock_semanal,
                     INPUT " Reporte Stock Semanal",
                     INPUT " Nada" ,
                     INPUT 7,
                     INPUT 8,
                     INPUT "Century Gothic",
                     INPUT 7). 
