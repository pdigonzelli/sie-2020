/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_etibin.p                                    */
/****************************************************************************/
/*  Imprime etiquetas de bines para camaras                                 */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

SESSION:DATA-ENTRY-RETURN = TRUE.

/*********** Parametros ****************/
DEFINE INPUT PARAMETER PCNROBIN AS INTEGER  NO-UNDO.

/*********** Variables Generales ****************/
DEFINE VARIABLE x_cant_etiq     AS INTEGER   FORMAT "z9" INITIAL 1.
DEFINE VARIABLE xTrazab         AS CHARACTER.



RUN imprime_etiquetas.


PROCEDURE imprime_etiquetas:

    OUTPUT TO PRINTER.
   
    FIND FIRST BINES WHERE
        bines.nro_BIN = PCNROBIN AND
        bines.serial_cosecha = pcnrobin no-lock no-error.

        FIND FIRST lotes_plantacion WHERE
            lotes_plantacion.codigo_trazabilidad = bines.codigo_trazabilidad 
            NO-LOCK NO-ERROR.

        IF AVAILABLE lotes_plantacion THEN DO:
            xTrazab = TRIM(lotes_plantacion.codigo_trazabilidad).

            PUT CONTROL "^XA".
            PUT CONTROL "^PQ" 1 "^FS".
            PUT CONTROL "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            PUT CONTROL "^LT0".

            PUT CONTROL "^FO000,045^BY4,3.^B3B,N,,Y,N^FR^FD" STRING(bines.nro_bin,"999999999999") "^FS".

            CASE LENGTH(xTrazab,"character"):
                WHEN 3 THEN
                    PUT CONTROL "^FO500,120^A0B,220,200^FD" xTrazab "^FS".
                WHEN 4 THEN
                    PUT CONTROL "^FO500,100^A0B,220,170^FD" xTrazab "^FS".
                WHEN 5 THEN
                    PUT CONTROL "^FO500,080^A0B,220,150^FD" xTrazab "^FS".
                WHEN 6 THEN
                    PUT CONTROL "^FO500,060^A0B,220,140^FD" xTrazab "^FS".
                WHEN 7 THEN
                    PUT CONTROL "^FO500,040^A0B,220,130^FD" xTrazab "^FS".
            END CASE.

            PUT CONTROL "^XZ".
        END.

    OUTPUT CLOSE.

END PROCEDURE.
