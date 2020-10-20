/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_balcax.p     (dd_balpax.p)                  */
/****************************************************************************/
/*  Permite borrar registros de balanza y  movsucu                           */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

/* Parametros */
define input  parameter x_balanza   like balanza_pesadas.id_balanza.
define input  parameter x_pesada    like balanza_pesadas.id_pesada.



define variable x_sucursal  as integer.

DEFINE VAR V1 AS INTEGER NO-UNDO.
DEFINE VAR V2 AS INTEGER NO-UNDO.

if x_balanza = 2 then x_sucursal = 98.
if x_balanza = 4 then x_sucursal = 97.



/****** Lee registro de balanza_pesadas *******/
find balanza_pesadas where
    balanza_pesadas.id_balanza  = x_balanza and
    balanza_pesadas.id_pesada   = x_pesada no-error.

if available balanza_pesadas then DO TRANSACTION ON ERROR UNDO , RETURN ERROR 'Error en la rutina panulapesada1 . Avise a sistemas': 
    /****** Controla si no fue procesado *******/

       IF balanza_pesadas.id_sucursal = 98 THEN  ASSIGN v1 = 110
                                       v2 = 97.
                          ELSE ASSIGN v1 = 130
                                      v2 = 98. 

        for each balanza_tickets where
            balanza_tickets.id_balanza  = balanza_pesadas.id_balanza and
            balanza_tickets.id_pesada   = balanza_pesadas.id_pesada:


            /******* Borra saldos packing *******/
            find first saldos_packing where
                saldos_packing.nro_partida = balanza_tickets.nro_partida and
                saldos_packing.nro_partida_serial = balanza_tickets.nro_partida_serial
                no-error.

               if available saldos_packing and saldos_packing.cantidad_volcada = 0 then do:

                 run dd_baldex.p
                    (input balanza_tickets.id_balanza,
                    input balanza_tickets.id_pesada,
                    input balanza_tickets.nro_ticket).

                 delete saldos_packing.

                /******* Llama al programa para estimacion cosecha *******/
                if balanza_tickets.id_tipo_cosecha = 1 and
                    (balanza_tickets.union_europea OR
                    balanza_tickets.china) and
                    balanza_tickets.id_sucursal_etiqueta <> v1 and
                    balanza_tickets.id_origen_origen <> v2 then do:

                    run dd_sdoest.p
                        (input balanza_tickets.fecha_cosecha,
                        input balanza_tickets.id_proveedor,
                        input balanza_tickets.id_finca_senasa,
                        input balanza_tickets.id_lote_senasa,
                        input balanza_tickets.certificado,
                        input balanza_tickets.codigo_trazabilidad,
                        input balanza_tickets.peso_neto_ticket,
                        input "baja").
               end.
            end.
            DELETE BALANZA_TICKETS.
        END.
        DELETE BALANZA_PESADAS.
end.    
ELSE UNDO , RETURN ERROR 'NO ENCONTRO LA PESADA'.

CATCH myerror AS PROGRESS.lang.syserror:
    UNDO , RETURN ERROR myerror:getmessage(1).
END CATCH.
