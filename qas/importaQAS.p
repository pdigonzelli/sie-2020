
/*------------------------------------------------------------------------
    File        : importaQAS.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Mar 24 22:28:27 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR LINEA AS CHARACTER NO-UNDO.     
DEFINE VAR I AS INTEGER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

CURRENT-WINDOW:WIDTH = 120.
INPUT FROM 'QAS2.CSV'.

REPEAT :
    SET LINEA FORMAT 'X(100)'.
    FIND FIRST items_stock WHERE NRO_PARTIDA = INTEGER(ENTRY(4,LINEA)) AND 
        NRO_PARTIDA_SERIAL = INTEGER(ENTRY(5,LINEA)) AND 
        SUBSTRING(items_stock.DOCUMENTO_SAP,3) = ENTRY(6,LINEA)  NO-ERROR.
    IF AVAILABLE items_stock THEN 
    DO:
        ORDEN_ENTREGA_SAP = ENTRY(7,LINEA).
        DISP ENTRY(7,LINEA) items_stock.id_tipo_movsto orden_entrega_sap WITH WIDTH 110.
    END.
END.

INPUT CLOSE.
   