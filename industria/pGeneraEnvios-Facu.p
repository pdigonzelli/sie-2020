
DEFINE VAR viSemana    AS INTEGER NO-UNDO. 
DEFINE VAR i           AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio  AS INTEGER    NO-UNDO.

DEFINE VARIABLE vcFecha  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcMsg    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcProg   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcEnvios AS CHARACTER  NO-UNDO.

FIND LAST envios NO-LOCK NO-ERROR.
IF AVAILABLE envios THEN
  i = envios.id_envio.
ELSE
  i = 0.

/*recupero todas las oes de este año que hayan sido enviadas (asociacion con packing_list)*/
FOR EACH packing_list WHERE id_tipo_pack_List   = 1 
                        AND fecha_salida_vapor >= DATE('01/01/2005') 
                      NO-LOCK.
  FIND FIRST orden_entrega OF packing_list NO-LOCK NO-ERROR.
  /*
  IF orden_entrega.id_orden_entrega = 10745 THEN DO:
    /*interrupcion para debugger*/

    DEFINE VAR dbg AS LOGICAL.
    dbg = DEBUGGER:INITIATE().
    dbg = DEBUGGER:SET-BREAK().  
  END.
  */

  
  IF NOT AVAILABLE orden_entrega OR orden_entrega.id_tipo_orden_entrega <> 1 THEN NEXT.
  /*recupero la semana en la que se envio*/
  RUN semana_anio.p (packing_list.fecha_salida_vapor , OUTPUT viSemana, OUTPUT iAnio).
  i = i + 1.
  /*pregunto por el envio, si ya existe no lo creo*/
  FIND FIRST r_envio_oe OF orden_entrega NO-LOCK NO-ERROR. 
   /*si existe la relacion, pregunto por el envio porque puedo tener una oe con varios contenedores enviados en distintos packing_list y necesito un envio por oe*/
  IF AVAILABLE r_envio_oe THEN DO:
    FIND FIRST envios WHERE envios.semana     = viSemana                 
                        AND envios.id_vapor   = packing_list.id_vapor    
                        AND envios.id_origen  = orden_entrega.id_lugdes  
                        AND envios.id_destino = orden_entrega.id_destino 
                        AND envios.anio       = YEAR(packing_list.fecha_salida_vapor) 
                      NO-LOCK NO-ERROR.
    IF AVAILABLE envios THEN NEXT. /*ya existe el envio*/
  END.
  
  CREATE  envios.
  ASSIGN  envios.semana                       = viSemana
          envios.id_vapor                     = packing_list.id_vapor 
          envios.id_origen                    = orden_entrega.id_lugdes
          envios.id_envio                     = i + 1
          envios.id_destino                   = orden_entrega.id_destino
          envios.fecha_salida_origen          = packing_list.fecha_salida_vapor
          envios.fecha_llegada_destino        = envios.fecha_salida_origen + packing_list.dias_transito    
          envios.c_fecha                      = TODAY 
          envios.anio                         = YEAR(packing_list.fecha_salida_vapor).
  
  FIND FIRST r_envio_oe WHERE r_envio_oe.id_envio         = envios.id_envio 
                          AND r_envio_oe.id_orden_entrega = orden_entrega.id_orden_entrega 
                        NO-LOCK NO-ERROR.
  IF NOT AVAILABLE r_envio_oe THEN DO:
    CREATE  r_envio_oe.
    ASSIGN  r_envio_oe.id_envio         = envios.id_envio
            r_envio_oe.id_orden_entrega = orden_entrega.id_orden_entrega.
  END.
  vcEnvios = vcEnvios + ", " + STRING(i).
  
  /*creo los contenedores despachados en el packing_list*/
  FOR EACH items_packing_list OF packing_list NO-LOCK.
    /*agrego el where porque cuando ya existe un contenedor no me deja crear el mismo contenedor para distinto envio*/
    FIND FIRST contenedores OF items_packing_list WHERE contenedores.id_envio = envios.id_envio NO-LOCK NO-ERROR. /**/
    IF AVAILABLE contenedores THEN NEXT. /*ya existe el contenedor*/
    CREATE contenedores.
    ASSIGN contenedores.nro_contenedor = items_packing_list.nro_contenedor
           contenedores.id_envio       = envios.id_envio.
  END.
END.
