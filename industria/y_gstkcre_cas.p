/**************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES         */
/* DANIEL REYNA                                   */
/**************************************************/
DEFINE BUFFER stock  FOR stock_historico_tambores.
DEFINE BUFFER bstock FOR stock_historico_tambores.


DEFINE INPUT PARAMETER x1     AS INTEGER.
DEFINE INPUT PARAMETER x2     AS INTEGER.
DEFINE INPUT PARAMETER x3     AS INTEGER.
DEFINE INPUT PARAMETER pdesde AS INTEGER.
DEFINE INPUT PARAMETER phasta AS INTEGER.
DEFINE INPUT PARAMETER xtipo  LIKE tipos_movi.id_tipo_movimiento.

DEFINE VARI tid_sucursal   LIKE tambores_industria.id_sucursal.
DEFINE VARI tid_empresa    LIKE tambores_industria.id_empresa.
DEFINE VARI tnromov        LIKE tambores_industria.nromov.
DEFINE VARI tid_tipotambor LIKE tambores_industria.id_tipotambor.


DEFINE VARI xxx     AS CHARACTER.
DEFINE VARI vdesde  AS INTEGER.
DEFINE VARI vhasta  AS INTEGER.
DEFINE VARI nomarch AS CHARACTER.

 
/* MESSAGE x1 " " x2 " " x3 " " x4 " " pdesde " " phasta " " xtipo VIEW-AS ALERT-BOX. */
FIND tipos_movi WHERE tipos_movi.id_tipo_movimiento = xtipo 
                NO-LOCK NO-ERROR.
IF NOT AVAILABLE tipos_movi THEN DO:
  MESSAGE "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " xtipo
  VIEW-AS ALERT-BOX.
  RETURN "error".
END.    

nomarch = "{1}".
/* find {1} where 
                   {1}.id_empresa = x1 and 
                   {1}.id_sucursal = x2 and
                   {1}.id_tipotambor = x3 and
                   {1}.nromov = x4 
                   no-lock no-error.
IF NOT AVAILABLE {1} THEN DO:
    MESSAGE "NO EXISTE CABECERA DE " nomarch VIEW-AS ALERT-BOX ERROR.
    RETURN "error".
END.
*/

IF pdesde = 0 OR phasta = 0 THEN DO:
  MESSAGE "ERROR en el Desde y/o Hasta de Tambores se anula" SKIP
          "la generacion de movimientos de Stock"
  VIEW-AS ALERT-BOX ERROR.
  RETURN "ADM-ERROR".            
END.
DEFINE BUFFER xbuf FOR tambores_industria.
DEFINE VARI vrow AS ROWID.

    vdesde = 0.
    vhasta = 0.                       
FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = x1
                                AND tambores_industria.id_sucursal   = x2
                                AND tambores_industria.id_tipotambor = x3
                                AND tambores_industria.id_tambor     = pdesde
                              NO-LOCK.    

IF AVAILABLE tambores_industria THEN DO:

    CREATE stock.
    ASSIGN
                    stock.id_articulo        = tambores_industria.id_articulo
                    stock.fecha              = TODAY
                    stock.id_tipo_movimiento = xtipo
                    stock.id_serial          = NEXT-VALUE(serial-stock-tambores) 
                    stock.id_lote            = IF xtipo = 18 THEN 0 ELSE tambores_industria.id_lote
                    stock.anio               = tambores_industria.anio
                    stock.tambor_desde       = pDesde
                    stock.tambor_hasta       = pHasta
                    stock.id_suc_origen      = IF tambores_industria.id_sucursal_ubicacion = 0 THEN (tambores_industria.id_sucursal * 10) ELSE (tambores_industria.id_sucursal_ubicacion * 10)
                    stock.id_suc_des         = IF tambores_industria.id_sucursal_ubicacion = 0 THEN tambores_industria.id_sucursal ELSE tambores_industria.id_sucursal_ubicacion
                    stock.datos_adicionales  = tipos_movi.descripcion
                    stock.c_usuario          = USERID("userdb")
                    stock.c_fecha            = TODAY
                    stock.c_hora             = STRING(TIME,"hh:mm:ss")
                    stock.id_empresa         = tambores_industria.id_empresa
                    stock.id_sucursal        = tambores_industria.id_sucursal
                    stock.id_tipotambor      = tambores_industria.id_tipotambor
                    stock.nromov             = tambores_industria.nromov
                    stock.signo              = IF tipos_movi.codigo = "+" 
                                                THEN "-"
                                                ELSE "+"
                    stock.id_envase          = tambores_industria.id_envase
                    stock.id_calidad         = tambores_industria.id_calidad.            
                CREATE bstock.
                BUFFER-COPY stock EXCEPT stock.id_serial TO bstock.
                ASSIGN
                    bstock.id_serial         = NEXT-VALUE(serial-stock-tambores)
                    bstock.id_suc_origen      = IF tambores_industria.id_sucursal_ubicacion = 0 THEN tambores_industria.id_sucursal ELSE tambores_industria.id_sucursal_ubicacion 
                    bstock.id_suc_des         = IF tambores_industria.id_sucursal_ubicacion = 0 THEN (tambores_industria.id_sucursal * 10) ELSE (tambores_industria.id_sucursal_ubicacion * 10)
                    bstock.signo              = IF tipos_movi.codigo = "+" 
                                                THEN "+"
                                                ELSE "-".
        
END.        
RETURN.

