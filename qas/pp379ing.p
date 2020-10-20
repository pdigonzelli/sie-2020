
/*------------------------------------------------------------------------
    File        : declaraPallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Mar 22 16:44:49 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER VSUC AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER VPALLET AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER VENTORNO AS CHARACTER NO-UNDO.

DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
DEFINE VAR RET AS LOGICAL NO-UNDO.

DEFINE VAR VBUDAT AS CHARACTER NO-UNDO.
DEFINE VAR VMATERIALSAP AS CHARACTER NO-UNDO.
DEFINE VAR VWERKS AS CHARACTER NO-UNDO.



/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */


FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = VSUC AND PALLETS.ID_PALLET = VPALLET NO-LOCK NO-ERROR.
IF NOT AVAILABLE PALLETS THEN RETURN ERROR 'PALLET INEXISTENTE'.

VBUDAT = STRING(DAY(pallets.fecha_operativa),'99') + STRING(MONTH(pallets.fecha_operativa),'99') + STRING(YEAR(pallets.fecha_operativa), '9999').

FIND pedidos_packing OF PALLETS NO-LOCK NO-ERROR.
IF NOT AVAILABLE PEDIDOS_PACKING THEN RETURN ERROR 'PEDIDO DE PRODUCCION INEXISTENTE'. 

FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
IF NOT AVAILABLE ITEMS_PEDIDOS_PACKING THEN RETURN ERROR 'ITEM DE PEDIDO DE PRODUCCION INEXISTENTE'. 
  
VMATERIALSAP = items_pedidos_packing.material_sap.
VWERKS = 'A100'.
  
    
CREATE SERVER hAppSrv.

IF VENTORNO = 'PRD' THEN
    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap2").
ELSE
    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap3").

RUN  CAMBIOPEDIDO.P ON hAppSrv  TRANSACTION DISTINCt (VSUC, VPALLET, VPEDIDO ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN  RETURN ERROR NEW PROGRESS.LANG.APPERROR(RETURN-VALUE , 550).

CATCH EX AS Progress.Lang.Error :
    RETURN ERROR NEW PROGRESS.LANG.APPERROR(EX:GETMESSAGE(1) , 550).
END CATCH. 
FINALLY.
    IF hAppSrv:CONNECTED () THEN
        ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.
END FINALLY.

