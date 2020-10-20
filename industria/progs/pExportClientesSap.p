DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


RUN d:\temp\industria\libProg2Sap.p PERSISTENT SET hLib.

/*
RUN transferClientes IN hLib (100,
                              DATE('01/01/2006'), 
                              DATE('31/12/2007')).

*/
RUN exportArchivoLsmw IN hLib ('batch_input_clientes_sap',
                               '',
                               'lsmwClientes.xls',
                               3, 9, 5).

  

