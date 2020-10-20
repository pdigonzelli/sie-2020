
/*------------------------------------------------------------------------
    File        : veotipoproceso.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sun May 01 20:52:24 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH PALLETS WHERE pallets.fecha_operativa >= DATE('01/01/2016') AND  pallets.id_caract = 2 .
    FIND caracteristicas OF PALLETS NO-LOCK NO-ERROR.
    FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = pallets.tipo_proceso NO-LOCK NO-ERROR.
    DISP PALLETS.TIPO_PROCESO pallets.id_caract caracteristicas.descripcion tipos_procesos.DESCRIPCION.