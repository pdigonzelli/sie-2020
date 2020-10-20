
 DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


RUN ..\industria\libProg2Sap.p PERSISTENT SET hLib.


RUN transferMaterialesIndustriaSD IN hLib.

/*  
RUN exportArchivoLsmw IN hLib ('batch_input_materiales_sap',
                               "WHERE SPART = 'FF'",
                               'lsmwMateriales.xls',
                               7, 16, 9).
 */

DEFINE BUFFER buMat FOR BATCH_input_materiales_sap.

CURRENT-WINDOW:WIDTH = 150.
FOR EACH bumat
    WHERE spart = "IN".
    DISP matnr FORMAT "x(20)"
         maktx format "x(20)"
         tdline FORMAT "x(20)"
         prdha FORMAT "x(10)"
         stawn FORMAT "x(10)"
        WITH WIDTH 150.
END.
