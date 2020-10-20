DEFINE INPUT PARAMETER pSuc  AS INTEGER.
DEFINE INPUT PARAMETER pArt  AS INTEGER.

DEFINE VAR vTambores         AS INTEGER.
DEFINE VAR vKilos            AS INTEGER.
DEFINE VAR hCon              AS HANDLE.
DEFINE VAR vTotalTamboresOf  AS INTEGER.
DEFINE VAR vTotalKilosOf     AS DECIMAL.
DEFINE VAR vArticulo         AS INTEGER. 
DEFINE VAR vEnvase           AS INTEGER.

DEFINE VARIABLE iCantProd AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCantRel  AS INTEGER    NO-UNDO.


/****** CALCULO LOS TAMBORES DE PRODUCCION DE CASCARA (CODIGO 54)  ***********/
vArticulo = pArt.
vTambores = 0.
vKilos    = 0.

FOR EACH lotes_ubicacion WHERE lotes_ubicacion.id_sucursal_ubicacion = pSuc
                           AND lotes_ubicacion.id_tipotambor         = 11,                                                      
    EACH lotes_cascara OF lotes_ubicacion WHERE (lotes_cascara.id_articulo = pArt OR lotes_cascara.id_articulo = pArt + 500 OR lotes_cascara.id_articulo = 626) /*el articulo 626 seria un lote de bolsas de produccion (caso especial para la prod 1171/06*/
                         NO-LOCK.   
  FIND comercial.sucursales WHERE comercial.sucursales.id_sucursal = pSuc NO-LOCK NO-ERROR.
  FIND envases_prod WHERE envases_prod.id_envase = 14 NO-LOCK NO-ERROR.
  FIND productos_terminados WHERE productos_terminados.id_articulo = lotes_cascara.id_articulo NO-LOCK NO-ERROR.
  FIND calidades OF lotes_cascara NO-LOCK NO-ERROR.

  CREATE stock_tambores.
  ASSIGN stock_tambores.id_empresa            = 1
         stock_tambores.id_sucursal           = lotes_cascara.id_sucursal
         stock_tambores.sucursal              = sucursales.abreviatura
         stock_tambores.id_sucursal_ubicacion = pSuc 
         stock_tambores.id_lote               = lotes_ubicacion.id_lote
         stock_tambores.id_tipotambor         = 11
         stock_tambores.anio_lote             = lotes_cascara.anio - 2000
         stock_tambores.id_envase             = lotes_cascara.id_envase
         stock_tambores.envase                = envases_prod.abreviatura
         stock_tambores.id_articulo           = lotes_cascara.id_articulo
         stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "CascaraLimon"
         stock_tambores.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "cascara"
         stock_tambores.id_calidad            = lotes_cascara.id_calidad
         stock_tambores.tambores              = lotes_ubicacion.cantidad
         stock_tambores.kilos                 = lotes_ubicacion.cantidad * 50
         stock_tambores.orden_reporte         = 10
         stock_tambores.cliente               = STRING(lotes_cascara.id_lote_cliente).
  
END.

/*producciones no asociadas a lote o asociadas parcialmente*/
FOR EACH produccion_cascara WHERE produccion_cascara.id_sucursal = pSuc
                              AND (produccion_cascara.id_articulo = pArt OR produccion_cascara.id_articulo = pArt + 900)
                            NO-LOCK.
  iCantProd = produccion_cascara.cantidad.
  iCantRel  = 0.
  FOR EACH r_produccion_cascara_lote WHERE produccion_cascara.id_sucursal   = r_produccion_cascara_lote.id_sucursal_prod
                                       AND produccion_cascara.id_produccion = r_produccion_cascara_lote.id_produccion
                                     NO-LOCK.
    iCantRel = iCantRel + r_produccion_cascara_lote.cantidad.    
  END.
  IF iCantProd > iCantRel THEN DO:
    FIND comercial.sucursales WHERE comercial.sucursales.id_sucursal = pSuc NO-LOCK NO-ERROR.
    FIND envases_prod WHERE envases_prod.id_envase = 14 NO-LOCK NO-ERROR.
    FIND calidades WHERE calidades.id_calidad = 626 NO-LOCK NO-ERROR.
    FIND productos_terminados WHERE productos_terminados.id_articulo = produccion_cascara.id_articulo NO-LOCK NO-ERROR.
  
    CREATE stock_tambores.
    ASSIGN stock_tambores.id_empresa            = produccion_cascara.id_empresa
           stock_tambores.id_sucursal           = produccion_cascara.id_sucursal
           stock_tambores.sucursal              = sucursales.abreviatura
           stock_tambores.id_sucursal_ubicacion = pSuc 
           stock_tambores.id_lote               = produccion_cascara.id_produccion
           stock_tambores.id_tipotambor         = 12
           stock_tambores.anio_lote             = YEAR(produccion_cascara.fecha) - 2000
           stock_tambores.id_envase             = 14
           stock_tambores.envase                = envases_prod.abreviatura
           stock_tambores.id_articulo           = produccion_cascara.id_articulo
           stock_tambores.articulo              = productos_terminados.abreviatura
           stock_tambores.calidad               = calidades.descripcion
           stock_tambores.id_calidad            = 626
           stock_tambores.tambores              = iCantProd - iCantRel
           stock_tambores.kilos                 = (iCantProd - iCantRel) * 50
           stock_tambores.orden_reporte         = 10.    


  END.

END.
  
/***************************************************************************************************/

