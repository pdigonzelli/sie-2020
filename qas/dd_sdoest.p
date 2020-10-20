/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_sdoest.p  (wbalpax.w)                       */
/****************************************************************************/
/*  Actualiza Estimacion de Cosecha                                         */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/
ROUTINE-LEVEL ON ERROR UNDO, THROW.


/*********** Parametros ****************/
DEFINE INPUT PARAMETER x_fecha      LIKE balanza_tickets.fecha_cosecha.
DEFINE INPUT PARAMETER x_proveedor  LIKE estimacion_cosecha.id_proveedor.
DEFINE INPUT PARAMETER x_fincsen    LIKE estimacion_cosecha.id_finca_senasa.
DEFINE INPUT PARAMETER x_lotesen    LIKE estimacion_cosecha.id_lote_senasa.
DEFINE INPUT PARAMETER x_certific   LIKE estimacion_cosecha.certificado.
DEFINE INPUT PARAMETER x_trazab     LIKE estimacion_cosecha.codigo_trazabilidad.
DEFINE INPUT PARAMETER x_peso       LIKE balanza_tickets.peso_neto_ticket.
DEFINE INPUT PARAMETER x_proceso    AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE x_periodo           LIKE estimacion_cosecha.periodo.

x_periodo   = YEAR(x_fecha).

/******* Estimaci¢n Cosecha *******/

FIND FIRST origenes WHERE
    origenes.id_proveedor       = x_proveedor AND
    origenes.id_finca_senasa    = x_fincsen AND
    origenes.estado NO-LOCK NO-ERROR.

IF AVAILABLE origenes THEN DO:
    FIND FIRST lote WHERE
        lote.id_proveedor   = x_proveedor AND
        lote.id_origen      = origenes.id_origen AND
        lote.id_lote_senasa = x_lotesen AND
        lote.certificado    = x_certific AND
        lote.estado NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE lote THEN DO:
        FIND FIRST lote WHERE
            lote.id_proveedor   = x_proveedor AND
            lote.id_origen      = origenes.id_origen AND
            lote.id_lote_senasa = x_lotesen AND
            lote.cert_china     = x_certific AND
            lote.estado NO-LOCK NO-ERROR.
    END.

    IF AVAILABLE lote THEN DO:
        FIND FIRST estimacion_cosecha WHERE
            estimacion_cosecha.periodo          = x_periodo     AND
            estimacion_cosecha.id_proveedor     = x_proveedor   AND
            estimacion_cosecha.id_finca_senasa  = x_fincsen     AND
            estimacion_cosecha.id_lote_senasa   = x_lotesen     AND
            estimacion_cosecha.certificado      = x_certific
            NO-ERROR.

        IF AVAILABLE estimacion_cosecha THEN DO:
            CASE x_proceso:
                WHEN "alta" THEN DO:
                    ASSIGN estimacion_cosecha.cantidad_ingresada =
                        estimacion_cosecha.cantidad_ingresada + (x_peso / 20).
                END.
                WHEN "baja" THEN DO:
                    ASSIGN estimacion_cosecha.cantidad_ingresada =
                        estimacion_cosecha.cantidad_ingresada - (x_peso / 20).
    
                    IF estimacion_cosecha.cantidad_ingresada < 0 THEN
                        ASSIGN estimacion_cosecha.cantidad_ingresada = 0.
                END.
            END CASE.
        END.        
    END.        
END.
