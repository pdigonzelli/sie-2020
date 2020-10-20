
/*------------------------------------------------------------------------
    File        : palletssinhu.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 05 10:45:45 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

OUTPUT TO D:\TEMP\PALLETSINHU.TXT.
FOR EACH PALLETS WHERE ID_PALLET >= 16000000 AND ESTADO.

    EXPORT DELIMITER ";" ID_PALLET ID_PALLET_SAP FECHA_OPERATIVA.
    
END.

OUTPUT CLOSE.