/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_sdoest.p  (wbalpax.w)                       */
/****************************************************************************/
/*  Actualiza Estimacion de Cosecha                                         */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/
ROUTINE-LEVEL ON ERROR UNDO, THROW.


/*********** Parametros ****************/
define input parameter x_fecha      like balanza_tickets.fecha_cosecha.
define input parameter x_proveedor  like estimacion_cosecha.id_proveedor.
define input parameter x_fincsen    like estimacion_cosecha.id_finca_senasa.
define input parameter x_lotesen    like estimacion_cosecha.id_lote_senasa.
define input parameter x_certific   like estimacion_cosecha.certificado.
define input parameter x_trazab     like estimacion_cosecha.codigo_trazabilidad.
define input parameter x_peso       like balanza_tickets.peso_neto_ticket.
define input parameter x_proceso    as character format "x(4)".

define variable x_periodo           like estimacion_cosecha.periodo.

x_periodo   = year(x_fecha).

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

    if available lote then do:
        find first estimacion_cosecha where
            estimacion_cosecha.periodo          = x_periodo     and
            estimacion_cosecha.id_proveedor     = x_proveedor   and
            estimacion_cosecha.id_finca_senasa  = x_fincsen     and
            estimacion_cosecha.id_lote_senasa   = x_lotesen     and
            estimacion_cosecha.certificado      = x_certific
            no-error.

        if available estimacion_cosecha then do:
            case x_proceso:
                when "alta" then do:
                    assign estimacion_cosecha.cantidad_ingresada =
                        estimacion_cosecha.cantidad_ingresada + (x_peso / 20).
                end.
                when "baja" then do:
                    assign estimacion_cosecha.cantidad_ingresada =
                        estimacion_cosecha.cantidad_ingresada - (x_peso / 20).
    
                    if estimacion_cosecha.cantidad_ingresada < 0 then
                        assign estimacion_cosecha.cantidad_ingresada = 0.
                end.
            end case.
        end.        
    end.        
END.
