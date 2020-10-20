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
DEFINE VARIABLE XUP             AS CHARACTER NO-UNDO.
/*
DEFINE VARIABLE PCNROBIN        AS INTEGER  NO-UNDO.

PCNROBIN = 20000001.
*/

RUN imprime_etiquetas.


PROCEDURE imprime_etiquetas:

    OUTPUT TO PRINTER.
   
    FIND FIRST BINES WHERE
        bines.nro_BIN = PCNROBIN /* AND
        bines.serial_cosecha = pcnrobin */  NO-LOCK NO-ERROR.

        FIND FIRST lotes_plantacion WHERE
            lotes_plantacion.codigo_trazabilidad = bines.codigo_trazabilidad 
            NO-LOCK NO-ERROR.

        IF AVAILABLE lotes_plantacion THEN DO:
            xTrazab = TRIM(lotes_plantacion.codigo_trazabilidad).
            FIND LOTE OF LOTES_PLANTACION NO-LOCK.
            FIND ORIGENES OF LOTES_PLANTACION NO-LOCK.
            
            XUP = ORIGENES.RENSPA + "-" + STRING(ORIGENES.ID_FINCA_SENASA, '9999') +  "-" + STRING(LOTE.ID_LOTE_SENASA,'999').

            PUT CONTROL "^XA".
            PUT CONTROL "^PQ" 1 "^FS".
            PUT CONTROL "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            PUT CONTROL "^LT0".

            PUT CONTROL "^FO030,045^BY4,3.0^B2B,300,Y,R,N^FR^FD" STRING(bines.nro_bin,"999999999999") "^FS".

            IF bines.nro_bin > 20000000 THEN
                PUT CONTROL "^FO380,050^A0B,40,40^FD---- ESPECIAL ----^FS".
            ELSE            
                PUT CONTROL "^FO380,050^A0B,40,40^FD---------------------------^FS".
            
            PUT CONTROL "^FO420,120^A0B,80,40^FD" "FINCA:" ORIGENES.DESCRIPCION "^FS".
            PUT CONTROL "^FO490,120^A0B,80,40^FD" "LOTE :" LOTES_PLANTACION.DESCRIPCION "^FS".
            PUT CONTROL "^FO560,120^A0B,80,40^FD" "UP :" XUP "^FS".
            PUT CONTROL "^FO630,120^A0B,80,40^FD" "TRAZ.:" LOTES_PLANTACION.CODIGO_TRAZABILIDAD "^FS".
/*            
            CASE LENGTH(xTrazab,"character"):
                WHEN 3 THEN
                    PUT CONTROL "^FO520,120^A0B,150,200^FD" xTrazab "^FS".
/*                    PUT CONTROL "^FO520,120^A0B,220,200^FD" xTrazab "^FS". */
                WHEN 4 THEN
                    PUT CONTROL "^FO520,100^A0B,150,170^FD" xTrazab "^FS".
/*                    PUT CONTROL "^FO520,120^A0B,220,170^FD" xTrazab "^FS". */
                WHEN 5 THEN
                    PUT CONTROL "^FO520,080^A0B,150,150^FD" xTrazab "^FS".
/*                    PUT CONTROL "^FO520,120^A0B,220,150^FD" xTrazab "^FS". */
                WHEN 6 THEN
                    PUT CONTROL "^FO520,060^A0B,150,140^FD" xTrazab "^FS".
/*                    PUT CONTROL "^FO520,120^A0B,220,140^FD" xTrazab "^FS". */
                WHEN 7 THEN
                    PUT CONTROL "^FO520,040^A0B,150,130^FD" xTrazab "^FS".
/*                    PUT CONTROL "^FO520,120^A0B,220,130^FD" xTrazab "^FS". */
            END CASE.
*/
            PUT CONTROL "^XZ".
        END.

    OUTPUT CLOSE.

END PROCEDURE.
