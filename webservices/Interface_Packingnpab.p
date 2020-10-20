/****************************************************************************/
/* NOMBRE PROGRAMA......:   Interface_Packing.p                             */
/****************************************************************************/
/* Genera los Movimientos de Packing en SAP a trav‚s de una Interfaz        */
/****************************************************************************/
/* PROGRAMADOR..........:   Gabriel Navarro                                 */
/****************************************************************************/


/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/



/* Recibe Par metros de items_stock */

DEFINE INPUT PARAMETER  xSucursal   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xTipoMov    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xSucEnvio   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xNro        AS INT64   NO-UNDO.



/* Define Variables del Programa */

DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
DEFINE VAR RET AS LOGICAL NO-UNDO.

CREATE SERVER hAppSrv.

/*DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().*/

MESSAGE 'CONECTANDO APPSERVER'.
ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap2").

IF NOT ret THEN
DO:
  RETURN ERROR "Failed to connect to AppServer".
END.
ELSE
DO:
    MESSAGE 'EJECUTANDO INTERFACE_PACKINGAP'.
    RUN interface_PackingnAP.p ON SERVER hAppSrv TRANSACTION DISTINCT (xSucursal,xTipoMov,xSucEnvio,xNro).
     
END.
 
CATCH E AS Progress.Lang.Error :
    UNDO , THROW E.
END CATCH.

FINALLY.
    IF hAppSrv:CONNECTED () THEN
        ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.
END FINALLY.