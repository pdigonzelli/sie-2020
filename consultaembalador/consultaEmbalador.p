DEFINE VAR puerto AS CHARACTER INITIAL "COM3".
DEFINE VAR EMBALADOR AS CHARACTER NO-UNDO FORMAT '9999999999'.

DEFINE VAR I AS INTEGER NO-UNDO.

DEFINE VAR hWin AS HANDLE NO-UNDO.

    RUN wMuestraProduccionEmbalador.w PERSISTENT SET hWin. 
    RUN initializeObject IN hWin. 
    RUN setVariables IN hWin (98, embalador).

REPEAT:

    RUN muestraEstado IN hWin ("Leyendo tarjeta ......").
    RUN leoserial1.p(puerto , OUTPUT embalador).
    
    IF embalador = "0000000000" THEN
        DO:
            MESSAGE 'PROBLEMA DE PUERTOS' VIEW-AS ALERT-BOX.
            QUIT.
        END.

    RUN muestraEstado IN hWin ("Mostrando ......").
    RUN addContadorTarjetasLeidas IN hWin.
    RUN muestraEmbalador IN hWin (embalador). 
    
    PAUSE 5 NO-MESSAGE.    
END.    
