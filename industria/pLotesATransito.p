DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE e AS INTEGER    NO-UNDO.
DEFINE VARIABLE s AS INTEGER    NO-UNDO.
DEFINE VARIABLE t AS INTEGER    NO-UNDO.
DEFINE VARIABLE n AS INTEGER    NO-UNDO.

DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
DELETE OBJECT hLibCom.


CURRENT-WINDOW:WIDTH = 150.
  
FOR EACH tambores_industria
    WHERE nromov = 77409
    BREAK BY id_sucursal_ubicacion.
  i = i + 1.
  /*
  IF LAST-OF(id_sucursal_ubicacion) THEN DO:
    DISP id_lote anio i.
    i = 0.
  END.*/

  id_sucursal_ubicacion = 82.

  ASSIGN e = id_empresa
         s = id_sucursal
         t = id_tipotambor
         n = nromov.

END.


RUN updateLoteUbicacion IN hLib (e, s, t, n, 95).



