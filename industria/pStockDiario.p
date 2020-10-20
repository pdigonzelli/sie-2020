DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

PROPATH = PROPATH + ",..\industria\prowinapi".

RUN libTareasAutomaticas.p PERSISTENT SET hLib.

RUN stockDiario IN hLib.

QUIT.
