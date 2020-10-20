/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_etibal_sap.p                                */
/****************************************************************************/
/*  Imprime etiquetas de balanza                                            */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

SESSION:DATA-ENTRY-RETURN = TRUE.

/*********** Parametros ****************/
define input parameter x_partida    like balanza_tickets.nro_partida.
define input parameter x_partida_s  like balanza_tickets.nro_partida_serial.

/*
define variable x_partida           like balanza_tickets.nro_partida.
define variable x_partida_s         like balanza_tickets.nro_partida_serial.
x_partida   = 424966.
x_partida_s = 1.
*/

/*********** Variables Generales ****************/
define variable x_remito            as character format "x(13)".
define variable x_cant_etiq         as integer   format "z9".
DEFINE VARIABLE x_suc_origen        AS INTEGER.

run imprime_etiquetas.

/*********************************************************/
/*   Imprime etiquetas codigo barras de balanzas         */
/*********************************************************/
procedure imprime_etiquetas:

    find first balanza_tickets where
        balanza_tickets.nro_partida         = x_partida and
        balanza_tickets.nro_partida_serial  = x_partida_s
        NO-LOCK no-error.

    if available balanza_tickets then do:
        IF balanza_tickets.id_balanza = 2 THEN
            x_cant_etiq = 2.
        ELSE
            x_cant_etiq = 4.

        IF balanza_tickets.id_sucursal = 98 THEN
            x_suc_origen = 97.
        ELSE
            x_suc_origen = 98.

        if balanza_tickets.id_proveedor_origen = 1 and
            balanza_tickets.id_origen_origen = x_suc_origen then
            x_cant_etiq = 1.

        find first balanza_pesadas of balanza_tickets no-lock no-error.
        find first proveedores of balanza_tickets no-lock no-error.
        find first origenes where
            origenes.id_proveedor = balanza_tickets.id_proveedor and
            origenes.id_origen    = balanza_tickets.id_origen
            no-lock no-error.
        find first lote where
            lote.id_proveedor = balanza_tickets.id_proveedor and
            lote.id_origen    = balanza_tickets.id_origen and
            lote.id_lote      = balanza_tickets.id_lote
            no-lock no-error.
        find first envases_prod of balanza_tickets no-lock no-error.
        find first productos_terminados where
            productos_terminados.id_articulo =
            balanza_tickets.id_materia_prima
            no-lock no-error.
        find first variedades where
            variedades.id_articulo = balanza_tickets.id_materia_prima and
            variedades.id_variedad = balanza_tickets.id_variedad
            no-lock no-error.
        find first tipo_cosecha of balanza_tickets no-lock no-error.

        x_remito    = substring(balanza_tickets.nro_remito,1,4) + "-" +
                      substring(balanza_tickets.nro_remito,5).
                

        IF balanza_pesadas.id_balanza = 2 THEN
            OUTPUT TO \\ARLAVD037\zebra.

        
        /****************************************/
        /********** Etiquetas 100 x 75 **********/
        /****************************************/

        put control "^XA".
        put control "^LT10".
        put control "^PQ" x_cant_etiq "^FS".
        put control "^PR8,8,8^FS". /** velocidad de impresion 8 pulgadas por segundo**/
        
        PUT CONTROL "^FO000,080^BY4,3.0^B2B,100,N,R,N^FR^FD" string(x_partida,"99999999") + string(x_partida_s,"99") "^FS".
/*            
        put control "^FO680,020,^BY3^B3R,N,100,N,N^FD" string(x_partida,"999999999") "^FS".
*/        
        put control "^FO120,010^A0N,30,42^FDPartida^FS".
        put control "^FO400,010^A0N,30,42^FDPesada^FS".
        put control "^FO605,010^A0N,30,42^FDFec.Cos.^FS".

        put control "^FO120,045^A0N,40,42^FD" string(x_partida) + string(x_partida_s,"99") "^FS".
        put control "^FO400,045^A0N,40,42^FD" string(balanza_tickets.id_pesada) "^FS".
        put control "^FO605,045^A0N,40,42^FD" string(balanza_tickets.fecha_cosecha,"99/99/99") "^FS".

        if balanza_tickets.id_tipo_cosecha = 4 then
            put control "^FO120,96^A0N,40,42^FDProduc: " substr(proveedores.nombre,1,15) " (FAMAILLA)^FS".
        else
            put control "^FO120,96^A0N,40,42^FDProduc: " proveedores.nombre "^FS".

        put control "^FO120,147^A0N,40,42^FDRemito: " x_remito "^FS".

        put control "^FO120,195^A0N,30,42^FDFinca^FS".
        put control "^FO470,195^A0N,30,42^FDLote^FS".

        put control "^FO120,228^A0N,40,42^FD" origenes.abreviatura "^FS".
        put control "^FO470,228^A0N,40,42^FD" lote.abreviatura "^FS".

        put control "^FO120,278^A0N,30,42^FDTrazab.^FS".
        put control "^FO300,278^A0N,30,42^FDU.P. " substr(origenes.zona_up,1,2) "^FS".
        put control "^FO520,278^A0N,30,42^FDCertif.^FS".
        put control "^FO660,278^A0N,30,42^FDFruta^FS".

        put control "^FO120,311^A0N,40,42^FD" balanza_tickets.codigo_trazabilidad "^FS".
        put control "^FO300,311^A0N,40,42^FD" string(balanza_tickets.id_finca_senasa,"9999") + '-' +
            string(balanza_tickets.id_lote_senasa,"999") "^FS".
        put control "^FO520,311^A0N,40,42^FD" balanza_tickets.certificado "^FS".
        if balanza_tickets.union_europea then 
            put control "^FO660,311^A0N,40,42^FDU.E.^FS".
        if balanza_tickets.china then 
            put control "^FO660,311^A0N,40,42^FDCHINA^FS".
        if balanza_tickets.union_europea = false and
            balanza_tickets.china = false then 
            put control "^FO660,311^A0N,40,42^FDNO UE^FS".

        put control "^FO120,360^A0N,30,42^FDEspecie^FS".
        put control "^FO320,360^A0N,30,42^FDVariedad^FS".
        put control "^FO540,360^A0N,30,42^FDEstado^FS".

        put control "^FO120,393^A0N,40,42^FD" productos_terminados.abreviatura "^FS".
        put control "^FO320,393^A0N,40,42^FD" variedades.abreviatura "^FS".
        case balanza_tickets.id_tipo_cosecha:
            when 4 then
                put control "^FO540,393^A0N,40,42^FDPROCESADO^FS".
            when 5 then
                put control "^FO540,393^A0N,40,42^FDTERC.P/FRIO^FS".
            otherwise
                put control "^FO540,393^A0N,40,42^FDS/PROCESAR^FS".
        end case.

        put control "^FO120,442^A0N,30,42^FDFecha Ingreso^FS".
        put control "^FO450,442^A0N,30,42^FDBines^FS".
        put control "^FO600,442^A0N,30,42^FDBandejas^FS".

        put control "^FO120,475^A0N,40,42^FD"
            string(balanza_tickets.fecha_entrada,"99/99/99") +
            "  " + substring(balanza_tickets.hora_entrada,1,5) "^FS".
        put control "^FO430,475^A0N,40,42^FD"
            string(balanza_tickets.cant_env_entrada,"zz,zz9") "^FS".
        put control "^FO600,475^A0N,40,42^FD"
            string(balanza_tickets.peso_neto_ticket / 20,"zz,zz9.99") "^FS".

        IF origenes.certificada THEN
            PUT CONTROL "^FO120,525^A0N,30,35^FDGLN: " origenes.gln "^FS".

        put control "^FO100,000^GB0,570,4^FS".
        put control "^FO100,084^GB670,000,4^FS".
        put control "^FO100,134^GB670,000,4^FS".
        put control "^FO100,184^GB670,000,4^FS".
        put control "^FO100,266^GB670,000,4^FS".
        put control "^FO100,348^GB670,000,4^FS".
        put control "^FO100,430^GB670,000,4^FS".
        put control "^FO100,512^GB670,000,4^FS".

        put control "^XZ".

        output close.
        
        FIND CURRENT balanza_tickets EXCLUSIVE-LOCK NO-ERROR.
        balanza_tickets.copias = balanza_tickets.copias + 1.
        FIND CURRENT balanza_tickets NO-LOCK.
    end.

end procedure.
