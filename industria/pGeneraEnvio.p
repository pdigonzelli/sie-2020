
DEFINE VAR viSemana AS INTEGER NO-UNDO. 
DEFINE VAR i        AS INTEGER NO-UNDO.

DEFINE VARIABLE vcFecha  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcMsg    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcProg   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcEnvios AS CHARACTER  NO-UNDO.

FIND LAST envios NO-LOCK NO-ERROR.
IF AVAILABLE envios THEN
  i = envios.id_envio.
ELSE
  i = 0.

FOR EACH packing_List WHERE id_tipo_pack_List = 1 
                        AND fecha_salida_vapor >= DATE('01/01/04') 
                      NO-LOCK.
  FIND FIRST orden_entrega OF packing_list NO-LOCK NO-ERROR.
  
  IF NOT AVAILABLE orden_entrega OR orden_entrega.id_tipo_orden_entrega <> 1 THEN NEXT.

  RUN s_seman1.p (packing_list.fecha_salida_vapor , OUTPUT viSemana).
  i = i + 1.
  
  FIND FIRST envios WHERE envios.semana     = viSemana                 AND
                          envios.id_vapor   = packing_list.id_vapor    AND
                          envios.id_origen  = orden_entrega.id_lugdes  AND
                          envios.id_destino = orden_entrega.id_destino AND
                          envios.anio       = YEAR(packing_list.fecha_salida_vapor) 
                    NO-LOCK NO-ERROR.
  
  IF AVAILABLE envios THEN DO:
    FIND FIRST r_envio_oe WHERE r_envio_oe.id_envio         = envios.id_envio 
                            AND r_envio_oe.id_orden_entrega = orden_entrega.id_orden_entrega 
                          NO-LOCK NO-ERROR.
    IF AVAILABLE r_envio_oe THEN 
      NEXT.  
    ELSE DO:
      /*existe el envio y no existe la relacion*/

    END.
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
  
  FIND FIRST r_envio_oe WHERE r_envio_oe.id_envio         = envios.id_envio AND
                              r_envio_oe.id_orden_entrega = orden_entrega.id_orden_entrega NO-LOCK NO-ERROR.
  IF NOT AVAILABLE r_envio_oe THEN DO:
    CREATE  r_envio_oe.
    ASSIGN  r_envio_oe.id_envio         = envios.id_envio
            r_envio_oe.id_orden_entrega = orden_entrega.id_orden_entrega.
  END.
  vcEnvios = vcEnvios + ", " + STRING(i).
  FOR EACH items_packing_list OF packing_list NO-LOCK.
    FIND contenedores OF items_packing_list NO-LOCK NO-ERROR.
    IF AVAILABLE contenedores  THEN NEXT.
    CREATE contenedores.
    ASSIGN contenedores.nro_contenedor = items_packing_list.nro_contenedor
           contenedores.id_envio       = envios.id_envio.
  END.

END.

IF vcEnvios <> "" THEN DO:
  vcFecha = STRING(TODAY, "99/99/9999") + " " + STRING(TIME, "HH:MM:SS").
  vcMsg   = "Se crearon los envios: " + vcEnvios.
  vcProg  = "pGeneraEnvio.p".
  /*
  RUN logIndustria.p (vcFecha, 
                      vcMsg, 
                      vcProg).                    
  */                    
END.                    

                        














