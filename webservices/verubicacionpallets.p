
/*------------------------------------------------------------------------
    File        : verubicacionpallets.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Apr 08 11:03:40 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR EACH PALLETS NO-LOCK WHERE pallets.fecha_operativa >= DATE("01/03/2017"), EACH items_pallets OF PALLETS NO-LOCK.
    DISP PALLETS.ID_PALLET items_pallets.bultos items_pallets.ubicacion FORMAT 'X(20)'.
END.