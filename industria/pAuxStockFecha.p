  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iUbi    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFecha  AS DATE       NO-UNDO.
  
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  
  iNro = 53357.
  iTbo = 4.
  dFecha = DATE("12/01/2005").
  
  FOR EACH stock_historico_tambores WHERE stock_historico_tambores.nromov        = iNro
                                      AND stock_historico_tambores.signo         = "-"
                                      AND stock_historico_tambores.fecha        <= dFecha
                                      AND stock_historico_tambores.tambor_desde <= iTbo                                      
                                      AND stock_historico_tambores.tambor_hasta >= iTbo
                                    NO-LOCK.
    DISP fecha id_suc_ori id_suc_des tambor_desde tambor_hasta.
    iCount = iCount + (stock_historico_tambor.tambor_hasta - stock_historico_tambores.tambor_desde + 1).

  END.

  DISP iCount.

