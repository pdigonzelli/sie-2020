
/*------------------------------------------------------------------------
    File        : assignposicion.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Dec 11 17:58:58 ACT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    POSICION-TEMPORAL.SERIE = CABECERA-TEMPORAL.SERIE
    POSICION-TEMPORAL.PORDER = CABECERA-TEMPORAL.SORDEN 
    POSICION-TEMPORAL.PDOCVENTAS = CABECERA-TEMPORAL.SDOCUMENTOVENTA 
    POSICION-TEMPORAL.PPOSICIONDOCVENTAS = CABECERA-TEMPORAL.SPOSICIONDOCUMENTOVENTA
    POSICION-TEMPORAL.PMATERIAL = CABECERA-TEMPORAL.SMATERIAL
    POSICION-TEMPORAL.PENVASE = POSICION.PENVASE 
    POSICION-TEMPORAL.PUNIDAD = POSICION.PUNIDAD 
    POSICION-TEMPORAL.PCALIBRESTD = CALIBRE-TEMPORAL.PCALIBRE.