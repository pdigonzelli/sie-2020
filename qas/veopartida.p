
/*------------------------------------------------------------------------
    File        : veopartida.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Mar 24 16:39:27 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE BUFFER BSUC FOR SUCURSALES.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
OUTPUT TO D:\TEMP\PARTIDA.TXT.

CURRENT-WINDOW:WIDTH = 300.
FOR EACH items_stock WHERE nro_partida = 432785.
    FIND TIPO_MOVSTO OF ITEMS_STOCK.
    FIND MOVSUCU OF ITEMS_STOCK.
    FIND BSUC WHERE BSUC.ID_SUCURSAL = MOVSUCU.ID_SUC_ENVIO.
    FIND SUCURSALES WHERE SUCURSALES.ID_SUCURSAL = MOVSUCU.ID_SUC_ORIGEN.
    EXPORT DELIMITER ";" 
        MOVSUCU.ID_SUC_ORIGEN
        SUCURSALES.NOMBRE
        movsucu.id_suc_envio
        movsucu.nro
        BSUC.NOMBRE
        items_stock.id_tipo_movsto
        TIPO_MOVSTO.ABREVIATURA 
        items_stock.codigo_stock
        items_stock.nro_partida
        items_stock.nro_partida_serial
        items_stock.FECHA 
        items_stock.C_FECHA
        items_stock.c_hora
        items_stock.documento_sap
        items_stock.orden_entrega_sap
        items_stock.cantidad
        ITEMS_STOCK.c_usuario 
        ITEMS_STOCK.c_hora.
END.
OUTPUT CLOSE.
