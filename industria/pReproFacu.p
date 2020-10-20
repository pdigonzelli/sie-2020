CURRENT-WINDOW:WIDTH = 200.

DEFINE TEMP-TABLE ttRepro
    FIELD id_lote AS INTEGER
    FIELD anio AS INTEGER
    FIELD tambores AS INTEGER
    FIELD kilos AS DECIMAL
    FIELD nromov AS INTEGER
    FIELD id_lote_des AS INTEGER
    FIELD anio_des AS INTEGER
    FIELD tambores_des AS INTEGER
    FIELD kilos_des AS DECIMAL
    FIELD nromov_des AS INTEGER.


  DEFINE VARIABLE pdDesde AS DATE       NO-UNDO.
  DEFINE VARIABLE pdHasta AS DATE       NO-UNDO.
  DEFINE VARIABLE piSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE piTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  ASSIGN pdDesde = DATE("01/01/2005")
         pdHasta = DATE("12/05/2005")
         piSuc   = 0
         piTip   = 3.


  DEFINE BUFFER buLot FOR tambores_industria.
  DEFINE BUFFER buRep FOR tambores_industria.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q AS DECIMAL    NO-UNDO.

  FOR EACH buLot NO-LOCK WHERE buLot.fecha >= pdDesde
                           AND buLot.fecha <= pdHasta
                           AND (IF piSuc <> 0 THEN buLot.id_sucursal_ubicacion = piSuc ELSE TRUE)
                           AND (IF piTip <> 0 THEN buLot.id_tipotambor         = piTip ELSE TRUE)
                         BREAK BY buLot.nromov.
    i = i + 1.
    k = k + buLot.kilos_tambor.
    IF LAST-OF(buLot.nromov) THEN DO:    
      
      FOR EACH buRep NO-LOCK WHERE buRep.id_empresa_destino     = buLot.id_empresa
                               AND buRep.id_sucursal_destino    = buLot.id_sucursal
                               AND buRep.id_tipotambor_destino  = buLot.id_tipotambor
                               AND buRep.nromov_destino         = buLot.nromov
                             BREAK BY buRep.nromov_destino.
        p = p + 1.
        q = q + buRep.kilos_tambor.
        IF LAST-OF(buRep.nromov_destino) THEN DO:
          
          iLot = buRep.id_lote.
          iAno = buRep.anio.
          CREATE ttRepro.
              ASSIGN ttrepro.id_lote = buLot.id_lote
                ttrepro.anio = buLot.anio
                ttrepro.tambores = i
                ttrepro.kilos = k
                ttrepro.nromov = buLot.nromov
                ttrepro.id_lote_des = buRep.id_lote
                ttrepro.anio_des = buRep.anio
                ttrepro.tambores_des = p
                ttrepro.kilos_des = q
                ttrepro.nromov_des = buRep.nromov.
          p = 0.
          q = 0.
          
        END.
      END.
      i = 0.
      k = 0.
      
    END.
  END.

                   

  FOR EACH ttrepro BY id_lote.
      DISP ttrepro WITH WIDTH 200.
  END.
