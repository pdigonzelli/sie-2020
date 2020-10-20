
/*------------------------------------------------------------------------
    File        : ttpallet.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Mon Mar 07 21:43:24 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE TTPALLET
   FIELD sucursal   AS CHARACTER
   FIELD pallet_id  AS CHARACTER
   FIELD material_embalaje AS CHARACTER
   FIELD tipo_pallet AS CHARACTER
   FIELD contramarca AS CHARACTER
   FIELD ppecb AS CHARACTER
   FIELD organizacion AS CHARACTER.


DEFINE TEMP-TABLE TTPOSICIONPALLET
   FIELD ID_EMPRESA AS INTEGER 
   FIELD ID_PUNTO_EMISOR AS INTEGER
   FIELD ID_ORDEN AS INTEGER
   FIELD ITEM AS INTEGER
   FIELD ID_PACKING AS INTEGER
   FIELD posicion AS INTEGER
   FIELD orden_mto AS CHARACTER
   FIELD material_sap AS CHARACTER
   FIELD cantidad AS CHARACTER
   FIELD UM AS CHARACTER
   FIELD lote_sap AS CHARACTER
   FIELD Almacen AS CHARACTER
   FIELD orden_mts AS CHARACTER
   FIELD AlmacenMTS AS CHARACTER
   FIELD tipo_mercado AS CHARACTER
   FIELD Ccalidad AS CHARACTER
   FIELD Ccalibre AS CHARACTER
   FIELD tipo_packaging AS CHARACTER
   FIELD Ccolor AS CHARACTER
   FIELD porcentaje_mrl AS CHARACTER
   FIELD Cmarca AS CHARACTER
   FIELD planta_empaque AS CHARACTER
   FIELD fecha_produccion AS CHARACTER
   FIELD turno_produccion AS CHARACTER
   FIELD nro_up_puc_spa AS CHARACTER
   FIELD nro_renspa AS CHARACTER
   FIELD ggn AS CHARACTER
   FIELD finca_quinta AS CHARACTER
   FIELD liote_cuadro AS CHARACTER
   FIELD trazabilidad AS CHARACTER
   FIELD esp_fitosanitaria AS CHARACTER
   FIELD var_fitosanitaria AS CHARACTER. 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
