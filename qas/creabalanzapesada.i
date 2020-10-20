
/*------------------------------------------------------------------------
    File        : creabalanzapesada.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Jan 13 15:16:53 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    
    CREATE balanza_pesadas.
    ASSIGN
        balanza_pesadas.fecha_entrada       = vfechaentrada
        balanza_pesadas.fecha_operativa     = vfechaoperativa
        balanza_pesadas.fecha_salida        = vfechaentrada
        balanza_pesadas.hora_entrada        = vhoraentrada
        balanza_pesadas.hora_salida         = vhoraentrada
        balanza_pesadas.id_balanza          = vbalanza
        balanza_pesadas.id_pesada           = vpesada
        balanza_pesadas.id_pesada_sap       = vpesadasap   
        balanza_pesadas.id_proveedor        = vproveedor
        balanza_pesadas.id_sucursal         = vsucursal
        balanza_pesadas.id_tipo_movsto      = vtipomov
        balanza_pesadas.id_tipo_transporte  = vtipotransporte
        balanza_pesadas.id_transporte       = INTEGER(vtransporte)
        balanza_pesadas.marca               = vmarca
        balanza_pesadas.modelo              = vmodelo
        balanza_pesadas.nromov              = vnromov
        balanza_pesadas.orden_carga_sap     = vocarga
        balanza_pesadas.patente             = vpatente
        balanza_pesadas.peso_descarte       = vpesodescarte
        balanza_pesadas.peso_entrada        = vpesoentrada
        balanza_pesadas.peso_neto           = vpesoneto
        balanza_pesadas.peso_salida         = vpesosalida
        balanza_pesadas.tara                = vtara
        balanza_pesadas.nombrePerfil        = 'SM'.