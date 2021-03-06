
/*------------------------------------------------------------------------
    File        : findcabecera.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Dec 10 10:44:53 ACT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*    FIELD SDOCUMENTOVENTA AS CHARACTER FORMAT 'X(10)'
    FIELD SPOSICIONDOCUMENTOVENTA AS CHARACTER FORMAT '999999'
    FIELD SSOLICITANTE AS CHARACTER FORMAT 'X(10)'
    FIELD SNOMBRE AS CHARACTER FORMAT 'X(30)'
    FIELD SFECHADOCUMENTO AS CHARACTER FORMAT '99999999'
    FIELD SMATERIAL AS CHARACTER FORMAT 'X(18)'
    FIELD SCANTIDAD AS CHARACTER FORMAT '99999999'
    FIELD SUNIDAD AS CHARACTER FORMAT 'X(3)'
    FIELD SPUERTOSALIDA AS CHARACTER FORMAT '99999'
    FIELD SDESCRIPCIONPUERTOSALIDA AS CHARACTER FORMAT 'X(40)'
    FIELD SDESTINO AS CHARACTER FORMAT '99999'
    FIELD SDESCRIPCIONDESTINO AS CHARACTER FORMAT 'X(40)'
    FIELD SPUERTOLLEGADA AS CHARACTER FORMAT '99999' 
    FIELD SDESCRIPCIONPUERTOLLEGADA AS CHARACTER FORMAT 'X(40)'.
*/
FIND FIRST CABECERA-TEMPORAL WHERE
    CABECERA-TEMPORAL.SORDEN = POSICION.PORDEN AND  
    CABECERA-TEMPORAL.SDOCUMENTOVENTA = CABECERA.SDOCUMENTOVENTA AND
    CABECERA-TEMPORAL.SPOSICIONDOCUMENTOVENTA = CABECERA.SPOSICIONDOCUMENTOVENTA AND
    CABECERA-TEMPORAL.SCALIDAD = POSICION.PCALIDAD AND
    CABECERA-TEMPORAL.SCANTIDAD = CABECERA.SCANTIDAD AND
    CABECERA-TEMPORAL.SCATEGORIA = POSICION.PCATEGORIA AND
    CABECERA-TEMPORAL.SCONTRAMARCA = POSICION.PCONTRAMARCA AND
    CABECERA-TEMPORAL.SDESCRIPCIONPUERTOLLEGADA = CABECERA.SDESCRIPCIONPUERTOLLEGADA AND
    CABECERA-TEMPORAL.SDESCRIPCIONPUERTOORIGEN = CABECERA.SDESCRIPCIONPUERTOSALIDA   AND
    CABECERA-TEMPORAL.SDESCRIPCIONDESTINO = CABECERA.SDESCRIPCIONDESTINO AND
    CABECERA-TEMPORAL.SDESTINO = CABECERA.SDESTINO AND
    CABECERA-TEMPORAL.SPUERTOLLEGADA = CABECERA.SPUERTOLLEGADA AND
    CABECERA-TEMPORAL.SPUERTOSALIDA = CABECERA.SPUERTOSALIDA AND
    CABECERA-TEMPORAL.SESPECIE = POSICION.PESPECIE AND
    CABECERA-TEMPORAL.SFECHADOCUMENTO = POSICION.PFECHASEMANA AND
    CABECERA-TEMPORAL.SLUGARDESCARGA = (IF AVAILABLE TAGENCIA THEN TAGENCIA.PLUGARDESCARGA ELSE ?)  AND
    CABECERA-TEMPORAL.SMATERIAL = CABECERA.SMATERIAL  AND
    CABECERA-TEMPORAL.SMERCADO = POSICION.PMERCADO AND
    CABECERA-TEMPORAL.SNOMBRE = CABECERA.SNOMBRE  AND
    CABECERA-TEMPORAL.SSOLICITANTE = CABECERA.SSOLICITANTE AND
    CABECERA-TEMPORAL.STIPOESQUINERO = POSICION.PTIPOESQUINERO AND
    CABECERA-TEMPORAL.STIPOPALLET =  POSICION.PTIPOPALLET AND
    CABECERA-TEMPORAL.SUNIDAD = CABECERA.SUNIDAD AND
    CABECERA-TEMPORAL.SAGENCIA = (IF AVAILABLE TAGENCIA THEN TAGENCIA.PAGENCIA ELSE ?) AND
    CABECERA-TEMPORAL.SDESCRIPCIONAGENCIA = (IF AVAILABLE TAGENCIA THEN TAGENCIA.PDESCRIPCIONAGENCIA ELSE ?) AND
    CABECERA-TEMPORAL.SVAPOR = (IF AVAILABLE TVAPOR THEN TVAPOR.PVAPOR ELSE ?) AND
    CABECERA-TEMPORAL.SDESCRIPCIONVAPOR = (IF AVAILABLE TVAPOR THEN TVAPOR.PDESCRIPCIONVAPOR ELSE ?) AND
    CABECERA-TEMPORAL.SVARIEDAD = POSICION.PVARIEDAD NO-ERROR.