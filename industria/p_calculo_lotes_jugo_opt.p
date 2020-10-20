define input parameter p_suc as integer.

define var v_tambores as integer.
define var v_tam_pedidos as integer.
define var v_kilos_pedidos as decimal format ">>>,>>>,>>9.99".
define var v_kilos_prod as decimal format ">>>,>>>,>>9.99".
define var v_kilos_despachados as decimal format ">>>,>>>,>>9.99".
define var v_kilos_stock as decimal format ">>>,>>>,>>9.99".
define var v_kilos_faltantes as decimal format ">>>,>>>,>>9.99".
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_envase as integer.
define VAR v_articulo as integer.
define VAR v_orden as integer.

define var v_tambores1 as integer.
define var v_tam_pedidos1 as integer.
define var v_kilos_pedidos1 as decimal format ">>>,>>>,>>9.99".
define var v_kilos_prod1 as decimal format ">>>,>>>,>>9.99".
define var v_kilos_despachados1 as decimal format ">>>,>>>,>>9.99".
define var v_kilos_stock1 as decimal format ">>>,>>>,>>9.99".
define var v_kilos_faltantes1 as decimal format ">>>,>>>,>>9.99".

DEFINE VAR v_lista_articulo AS CHARACTER INITIAL "42,43,46,53,52,66,71,70,50,51,512,513,514,517,518,519,520,41,57,90,58,61,74,76,761,762,763,582,952,953".
DEFINE VAR v_lista_orden    AS CHARACTER INITIAL "60,60,65,1,10,15,20,25,73,74,75,80,81,90,91,92,93,100,110,111,105,130,170,180,190,193,195,196,40,30".

DEFINE VARIABLE iAcc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAccJugo AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAccAceite AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAccFold AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKilos AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKilosJugo AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKilosAceite AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKilosFold AS INTEGER    NO-UNDO.




define buffer b_tam for tambores_industria.

find comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.

/******************** TAMBORES DE LOTE JUGO CON OF **********************************/

FOR EACH contratos NO-LOCK.
    
    FIND FIRST clientes of contratos no-lock no-error.

    FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  =  p_suc 
                                          AND tambores_industria.id_locacion_ubicacion  = 4
                                          AND (tambores_industria.id_tipotambor         = 3
                                           OR tambores_industria.id_tipotambor          = 6
                                           OR tambores_industria.id_tipotambor          = 7)
                                          AND contratos.id_contrato                     = tambores_industria.id_contrato_of
                                          AND contratos.id_tipo_contrato                = tambores_industria.id_tipocontrato_of
                                          AND contratos.anio                            = tambores_industria.anio_of
                                       BREAK BY tambores_industria.nromov.


                                      ACCUMULATE tambores_industria.id_lote (count by tambores_industria.nromov) .
                                      v_tambores1 = v_tambores1 + 1.
                                      v_tam_pedidos1 = v_tam_pedidos1 + 1.
                                      v_kilos_pedidos1 = v_kilos_pedidos1 + tambores_industria.kilos_tambor.
                                      iAcc = iAcc + 1.

                                      ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.nromov).
                                      
                                      
                                      v_kilos_prod1 = v_kilos_prod1 + tambores_industria.kilos_tambor.
                                      if tambores_industria.id_tipo_movsto > 0 and tambores_industria.nro_remito > 0 then 
                                          v_kilos_despachados1 = v_kilos_despachados1 + tambores_industria.kilos_tambor.
                                      
                                      
                                      IF LAST-OF(tambores_industria.nromov) THEN DO:
                                          
                                          IF tambores_industria.id_tipotambor = 3 THEN DO:
                                            FIND FIRST lotes_jugo OF tambores_industria NO-LOCK NO-ERROR.
                                            FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_jugo.estado_lote no-lock no-error.
                                          END.
                                          ELSE DO:
                                              FIND FIRST lotes_aceite OF tambores_industria NO-LOCK NO-ERROR.
                                              FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.
                                          END.
                                              
                                          
                                          IF (AVAILABLE lotes_jugo AND lotes_jugo.estado_lote >= 2 AND tambores_industria.id_tipotambor = 3) OR
                                             (AVAILABLE lotes_aceite AND lotes_aceite.estado_lote >= 2) THEN DO:

                                              v_tambores = v_tambores + v_tambores1.
                                              v_tam_pedidos = v_tam_pedidos + v_tam_pedidos1.
                                              v_kilos_pedidos = v_kilos_pedidos + v_kilos_pedidos1.
                                              v_kilos_prod    = v_kilos_prod + v_kilos_prod1.
                                              v_kilos_despachados = v_kilos_despachados + v_kilos_despachados1.


                                                    
                                          {i_calculo_lotes_jugo_opt.i}
                                          iAcc = 0.

                                          /**************** SECTOR DE DATOS DE CONTRATOS *********************************************/

                                          v_total_tambores_of = 0.
                                          v_total_kilos_of = 0.

                                          FOR EACH items_contratos of contratos no-lock.
                                              v_total_tambores_of = v_total_tambores_of + items_contratos.cantidad.
                                          END.


                                          IF AVAILABLE contratos THEN DO:
                                              ASSIGN stock_tambores.orden_fabricacion   = string(contratos.orden_fabricacion) 
                                                     stock_tambores.id_contrato         = contratos.id_contrato
                                                     stock_tambores.anio                = contratos.anio
                                                     stock_tambores.id_cliente          = contratos.id_cliente
                                                     stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
                                                     stock_tambores.cantidad_total_of   = v_total_tambores_of
                                                     stock_tambores.kilos_total_of      = v_total_tambores_of * tambores_industria.kilos_tambor
                                                     stock_tambores.anio_contrato       = integer(substring(string(year(contrato.fecha)),3,2)).

                                              v_kilos_stock = v_kilos_stock + (v_total_tambores_of * tambores_industria.kilos_tambor).

                                              IF AVAILABLE clientes THEN ASSIGN stock_tambores.cliente             = clientes.nombre.
                                              ELSE ASSIGN stock_tambores.cliente             = "SIN CLIENTE ASIGNADO".

                                          END.
                                          /*************************************************************************************************/


                                          END.
                                          
                                          v_tambores1 = 0.
                                          v_tam_pedidos1 = 0.
                                          v_kilos_pedidos1 = 0.
                                          v_kilos_prod1 = 0.
                                          v_kilos_despachados1 = 0.
                                          v_kilos_faltantes1 = 0.


                                      END.

                                      
    END.

                                      
                                          create info_contrato.
                                          assign info_contrato.orden_fabricacion  = string(contratos.orden_fabricacion)
                                                 info_contrato.anio_corto         = integer(substring(string(contratos.anio),3,2))
                                                 info_contrato.id_contrato        = contratos.id_contrato
                                                 info_contrato.id_tipo_contrato   = contratos.id_tipo_contrato
                                                 info_contrato.anio               = INTEGER(SUBSTRING(STRING(contratos.anio),3,2))
                                                 info_contrato.id_cliente         = contratos.id_cliente
                                                 info_contrato.cliente            = contratos.nombre.
                                          
                                          
    
    
                                           v_kilos_faltantes = v_kilos_pedidos - v_kilos_prod.
        
    
                                          ASSIGN
                                            info_contrato.tambores_pedidos   = v_tam_pedidos
                                            info_contrato.kilos_pedidos      = v_kilos_pedidos
                                            info_contrato.kilos_producidos   = v_kilos_prod
                                            info_contrato.kilos_despachados  = v_kilos_despachados
                                            info_contrato.kilos_stock        = v_kilos_stock
                                            info_contrato.kilos_faltante    = v_kilos_faltantes.
                                          
                                          v_tam_pedidos = 0.
                                          v_kilos_pedidos = 0.
                                          v_kilos_prod = 0.
                                          v_kilos_despachados = 0.
                                          v_kilos_stock = 0.
                                          v_kilos_faltantes = 0.

END.


MESSAGE "TErmino el calculo de los tambores de jugo con contratos" VIEW-AS ALERT-BOX.


/******************** TAMBORES DE LOTE JUGO SIN OF **********************************/
FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND (tambores_industria.id_tipotambor          = 3
                                        OR tambores_industria.id_tipotambor          = 6
                                        OR tambores_industria.id_tipotambor          = 7)
                                      AND tambores_industria.id_contrato_of         = ""
                                      AND tambores_industria.id_tipocontrato_of     = 0
                                      AND tambores_industria.anio_of                = 0
                                    BREAK BY tambores_industria.nromov.

    ACCUMULATE tambores_industria.id_lote (count by tambores_industria.nromov).
    
    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.nromov).
    
    /*by facundo 23/02/2005 el accumulate acumula todos los registro y yo necesito contabilizar en funcion del tipotambor*/
    IF tambores_industria.id_tipotambor = 3 THEN
      ASSIGN iAccJugo   = iAccJugo + 1
             iKilosJugo = iKilosJugo + tambores_industria.kilos_tambor.    

    IF tambores_industria.id_tipotambor = 6 THEN
      ASSIGN iAccAceite   = iAccAceite + 1
             iKilosAceite = iKilosAceite + tambores_industria.kilos_tambor.    
    
    IF tambores_industria.id_tipotambor = 7 THEN
      ASSIGN iAccFold   = iAccFold + 1
             iKilosFold = iKilosFold + tambores_industria.kilos_tambor.    
    
    IF LAST-OF(tambores_industria.nromov) THEN DO:
        IF tambores_industria.id_tipotambor = 3 THEN DO:
            FIND FIRST lotes_jugo OF tambores_industria NO-LOCK NO-ERROR.
            FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_jugo.estado_lote no-lock no-error.
        END.
        ELSE DO:
            FIND FIRST lotes_aceite OF tambores_industria NO-LOCK NO-ERROR.
            FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.
        END.
                                              
                                          
        IF (AVAILABLE lotes_jugo AND lotes_jugo.estado_lote >= 2) OR
           (AVAILABLE lotes_aceite AND lotes_aceite.estado_lote >= 2) THEN DO:      
          
          /*by facundo 23/02/2005*/
          IF tambores_industria.id_tipotambor = 3 THEN
            ASSIGN iAcc   = iAccJugo
                   iKilos = iKilosJugo.    
          
          IF tambores_industria.id_tipotambor = 6 THEN
            ASSIGN iAcc   = iAccAceite
                   iKilos = iKilosAceite.    
          
          IF tambores_industria.id_tipotambor = 7 THEN
            ASSIGN iAcc   = iAccFold 
                   iKilos = iKilosFold.    
            
          FIND FIRST envases_prod of tambores_industria no-lock no-error.
          FIND FIRST productos_terminados of tambores_industria no-lock no-error.
          FIND FIRST calidades of tambores_industria no-lock no-error.
          FIND FIRST r_productos_calidad where r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                           and r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                           no-lock no-error. 
          
          IF NOT AVAILABLE productos_terminados THEN DO:
              MESSAGE "Hay tambores con el siguiente articulo cargado " + string(tambores_industria.id_articulo)  + " " + STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio)
                       VIEW-AS alert-box.
          END.
              CREATE stock_tambores.
              ASSIGN stock_tambores.id_empresa            = tambores_industria.id_empresa
                     stock_tambores.id_sucursal           = tambores_industria.id_sucursal
                     stock_tambores.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
                     stock_tambores.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion 
                     stock_tambores.id_lote               = tambores_industria.id_lote 
                     stock_tambores.id_tipotambor         = tambores_industria.id_tipotambor
                     stock_tambores.anio_lote             = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)) 
                     stock_tambores.fecha                 = tambores_industria.fecha
                     stock_tambores.id_envase             = tambores_industria.id_envase
                     stock_tambores.envase                = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
                     stock_tambores.id_articulo           = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0
                     stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                     stock_tambores.orden_reporte         = IF LOOKUP(STRING(productos_terminados.id_articulo) ,v_lista_articulo) <> 0 THEN integer(ENTRY(LOOKUP(STRING(productos_terminados.id_articulo) ,v_lista_articulo), v_lista_orden)) ELSE 1000
                     stock_tambores.id_estado             = IF AVAILABLE estados_lotes THEN estados_lotes.id_estado_lote ELSE 0    
                     stock_tambores.estado                = IF AVAILABLE estados_lotes THEN estados_lotes.descripcion ELSE "NONE"
                     stock_tambores.calidad               = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
                     stock_tambores.id_calidad            = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                     stock_tambores.tambores              = iAcc
                     stock_tambores.kilos                 = iKilos  /*(accum total by tambores_industria.nromov tambores_industria.kilos_tambor)*/
                     stock_tambores.kilos_400             = IF AVAILABLE r_productos_calidad THEN (iKilos) * r_productos_calidad.coeficiente
                                                                                               ELSE 0.
          ASSIGN  iAccJugo      = 0 
                  iAccAceite    = 0
                  iAccFold      = 0        
                  iAcc          = 0
                  iKilosJugo    = 0
                  iKilosAceite  = 0
                  iKilosFold    = 0
                  iKilos        = 0.
        END.
    END.
END.
