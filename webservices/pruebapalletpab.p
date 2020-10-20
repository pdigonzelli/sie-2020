 
/*------------------------------------------------------------------------
    File        : pruebapalletpab.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Apr 26 10:25:09 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND
                   PALLETS.ID_PALLET = 17003928 NO-LOCK NO-ERROR.
                   
FOR EACH items_pallets OF PALLETS NO-LOCK.
    FOR EACH CAJAS WHERE OF PALLETS NO-LOCK.
        DISP cajas.id_caja WITH 2 COLUMNS.
    END.
END.
 
