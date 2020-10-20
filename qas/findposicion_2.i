
/*------------------------------------------------------------------------
    File        : findposicion.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Dec 11 17:57:03 ACT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST POSICION-TEMPORAL WHERE 
    POSICION-TEMPORAL.SERIE = CABECERA-TEMPORAL.SERIE AND
    POSICION-TEMPORAL.PORDER = CABECERA-TEMPORAL.SORDEN AND 
    POSICION-TEMPORAL.PDOCVENTAS = CABECERA-TEMPORAL.SDOCUMENTOVENTA AND 
    POSICION-TEMPORAL.PPOSICIONDOCVENTAS = CABECERA-TEMPORAL.SPOSICIONDOCUMENTOVENTA AND
    POSICION-TEMPORAL.PMATERIAL = CABECERA-TEMPORAL.SMATERIAL AND
    POSICION-TEMPORAL.PENVASE = POSICION.PENVASE AND 
    POSICION-TEMPORAL.PUNIDAD = POSICION.PUNIDAD AND 
    POSICION-TEMPORAL.PCALIBRESTD = CALIBRE-TEMPORAL.PCALIBRE NO-ERROR.