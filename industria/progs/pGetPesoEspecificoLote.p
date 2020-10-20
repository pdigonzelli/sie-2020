DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE fPes AS DECIMAL    NO-UNDO.

DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
DELETE OBJECT hLibCom.

FIND FIRST lotes_jugo
     WHERE lotes_jugo.id_lote     = 228
       AND lotes_jugo.anio        = 2006
       AND lotes_jugo.id_articulo = 53
    NO-LOCK.
FIND FIRST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.


fpes = DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLib, inspecciones_lote.bx_correg).

MESSAGE inspecciones_lote.bx_correg SKIP fpes
  VIEW-AS ALERT-BOX INFO BUTTONS OK.


