
/********************************************************
Purpose:       Lanzar el programa que estima los datos de la OE.
Parameters:    INPUT  pIdOrdenEntrega id_orden_entrega
               el resto son los parametros de salida con los gastos de la OE
Author:	       Facundo Juarez
Last Modified: 21/05/2003 11:32 am 
*********************************************************/


DEFINE VAR vTotalFactura    	    LIKE orden_entrega.total_factura    NO-UNDO.
DEFINE VAR vFob                     AS DECIMAL DECIMALS 4               NO-UNDO.
DEFINE VAR vKilos           	    AS INTEGER                          NO-UNDO.
DEFINE VAR vTambores        	    AS INTEGER                          NO-UNDO.
DEFINE VAR vComision        	    AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEntry 	    AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaFlete      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaSeguro     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaGenerico   AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaBunker     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcOrigen  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcDestino AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaInbSur     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaDcr        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEbaf       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgencia            AS CHARACTER                        NO-UNDO.
DEFINE VAR vRes                     AS CHARACTER                        NO-UNDO.
DEFINE VAR vGastoAgenciaEntry       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaFlete       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThc         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaSeguro      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaVarios      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaToll        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaHandling    AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInLand      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBunker      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBL          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThcDestino  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInbSur      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaDcr         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAgp         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaT7          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaEbaf        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAms         AS DECIMAL                          NO-UNDO.


RUN ..\Industria\facundo\develop\progress8\includes\p_prespupesto_oe.p (
    INPUT 8963, 
    OUTPUT vTotalFactura,
    OUTPUT vFob, 
    OUTPUT vKilos,
    OUTPUT vTambores,
    OUTPUT vComision,
    OUTPUT vGastoClausulaEntry,
    OUTPUT vGastoClausulaFlete,
    OUTPUT vGastoClausulaSeguro,
    OUTPUT vGastoClausulaGenerico,
    OUTPUT vGastoClausulaBunker,
    OUTPUT vGastoClausulaThcOrigen,
    OUTPUT vGastoClausulaThcDestino,
    OUTPUT vGastoClausulaInbSur,
    OUTPUT vGastoClausulaDcr,
    OUTPUT vGastoClausulaEbaf,
    OUTPUT vGastoAgenciaEntry,
    OUTPUT vGastoAgenciaFlete,
    OUTPUT vGastoAgenciaThc,
    OUTPUT vGastoAgenciaSeguro,
    OUTPUT vGastoAgenciaVarios,
    OUTPUT vGastoAgenciaToll,
    OUTPUT vGastoAgenciaHandling,
    OUTPUT vGastoAgenciaInLand,
    OUTPUT vGastoAgenciaBunker,
    OUTPUT vGastoAgenciaBL,
    OUTPUT vGastoAgenciaThcDestino,
    OUTPUT vGastoAgenciaInbSur,
    OUTPUT vGastoAgenciaDcr,
    OUTPUT vGastoAgenciaAgp,
    OUTPUT vGastoAgenciaT7,
    OUTPUT vGastoAgenciaEbaf,
    OUTPUT vGastoAgenciaAms,
    OUTPUT vGastoAgencia).


vRes =  "Total Factura: "  + STRING(vTotalFactura)    	      + CHR(13) +
        "Fob: "            + STRING(vFob)    	              + CHR(13) +
        "Kilos: "          + STRING(vKilos)           	      + CHR(13) +
        "Tambores: "       + STRING(vTambores)        	      + CHR(13) +
        "Comision: "       + STRING(vComision)        	      + CHR(13) +
        "Cl Entry: "       + STRING(vGastoClausulaEntry)      + CHR(13) +
        "Cl Flete: "       + STRING(vGastoClausulaFlete)      + CHR(13) +
        "Cl Seguro: "      + STRING(vGastoClausulaSeguro)     + CHR(13) +
        "Cl Generico: "    + STRING(vGastoClausulaGenerico)   + CHR(13) +
        "Cl Bunker: "      + STRING(vGastoClausulaBunker)     + CHR(13) +
        "Cl THC Origen: "  + STRING(vGastoClausulaThcOrigen)  + CHR(13) +
        "Cl THC Destino: " + STRING(vGastoClausulaThcDestino) + CHR(13) +
        "Cl Inbalance: "   + STRING(vGastoClausulaInbSur)     + CHR(13) +
        "Cl DCR: "         + STRING(vGastoClausulaDcr)        + CHR(13) +
        "Cl EBAF: "        + STRING(vGastoClausulaEbaf)       + CHR(13) +
        "Ag Entry: "       + STRING(vGastoAgenciaEntry)       + CHR(13) +
        "Ag Flete: "       + STRING(vGastoAgenciaFlete)       + CHR(13) +
        "Ag Thc: "         + STRING(vGastoAgenciaThc)         + CHR(13) +
        "Ag Seguro: "      + STRING(vGastoAgenciaSeguro)      + CHR(13) +
        "Ag Varios: "      + STRING(vGastoAgenciaVarios)      + CHR(13) +
        "Ag Toll: "        + STRING(vGastoAgenciaToll)        + CHR(13) +
        "Ag Handling: "    + STRING(vGastoAgenciaHandling)    + CHR(13) +
        "Ag InLand: "      + STRING(vGastoAgenciaInLand)      + CHR(13) +
        "Ag Bunker: "      + STRING(vGastoAgenciaBunker)      + CHR(13) +
        "Ag BL: "          + STRING(vGastoAgenciaBL)          + CHR(13) +
        "Ag Thc Destino: " + STRING(vGastoAgenciaThcDestino)  + CHR(13) +
        "Ag Inb Sur: "     + STRING(vGastoAgenciaInbSur)      + CHR(13) +
        "Ag Dcr: "         + STRING(vGastoAgenciaDcr)         + CHR(13) +
        "Ag Agp: "         + STRING(vGastoAgenciaAgp)         + CHR(13) +
        "Ag T7: "          + STRING(vGastoAgenciaT7)          + CHR(13) +
        "Ag Ebaf: "        + STRING(vGastoAgenciaEbaf)        + CHR(13) +
        "Ag Ams: "         + STRING(vGastoAgenciaAms)         + CHR(13) +
        "Gastos Agencia "  + chr(13) + vGastoAgencia.
MESSAGE vRes VIEW-AS ALERT-BOX.
