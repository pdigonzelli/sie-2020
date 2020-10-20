
/*------------------------------------------------------------------------
    File        : creamovsucu.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Jan 13 15:16:04 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    CREATE  movsucu.
    ASSIGN
         movsucu.id_sucursal     = vsucplaya
         movsucu.id_tipo_movsto  = vtipomov
         movsucu.nro             = x_numero
         movsucu.id_suc_origen   = vsucplaya
         movsucu.id_suc_envio    = vsucorigen
         movsucu.fecha_proceso   = ?
         movsucu.fecha_operativa = balanza_pesadas.fecha_operativa
         movsucu.fecha       = balanza_pesadas.fecha_salida
         movsucu.hora        = SUBSTRING(balanza_pesadas.hora_salida,1,2) + "00"
         movsucu.nombrePerfil = 'SM'.
