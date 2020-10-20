
/*------------------------------------------------------------------------
    File        : vermovimientospartida.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Apr 20 15:12:31 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE BUFFER SUC1 FOR SUCURSALES.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR EACH items_stock WHERE items_stock.NRO_PARTIDA = 432682 NO-LOCK,
    FIRST tipo_movsto OF items_stock NO-LOCK ,
    FIRST movsucu OF ITEMS_STOCK NO-LOCK,
    FIRST SUCURSALES WHERE SUCURSALES.ID_SUCURSAL = movsucu.id_suc_origen NO-LOCK,
    FIRST SUC1 WHERE SUC1.id_sucursal = movsucu.id_suc_envio NO-LOCK  .
    
    
    
    DISP    items_stock.id_tipo_movsto 
            tipo_movsto.descripcion 
            items_stock.nro_partida
            items_stock.nro_partida_serial
            sucursales.nombre 
            items_stock.documento_sap format 'x(20)'
            SUC1.NOMBRE. 
END.    