
/*------------------------------------------------------------------------
    File        : controlpallets.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Apr 06 15:55:40 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR I AS INTEGER NO-UNDO.
DEFINE VAR K AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH PALLETS WHERE PALLETS.ID_PALLET > 16000000 AND ESTADO NO-LOCK.
    I = 0.
    FOR EACH items_pallets OF PALLETS NO-LOCK.
        I = I + items_pallets.BULTOS.
    END.
    IF I <> pallets.bultos THEN
        DISP PALLETS.ID_PALLET I pallets.bultos.
    K = 0.    
    FOR EACH CAJAS OF PALLETS WHERE cajas.id_testigo = 0 NO-LOCK.
        K = K + 1.
    END.    
    
    IF K > pallets.bultos THEN
        DISP PALLETS.ID_PALLET K pallets.bultos.
end.