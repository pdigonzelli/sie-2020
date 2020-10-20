
/*------------------------------------------------------------------------
    File        : veoliqtarjas.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Aug 31 06:00:03 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR EACH liq_tarjas NO-LOCK WHERE liq_tarjas.fecha >= TODAY - 3,FIRST origenes OF liq_tarjas WHERE ID_ZONA = 2 NO-LOCK.
     
    DISP origenes.id_zona.
END.
