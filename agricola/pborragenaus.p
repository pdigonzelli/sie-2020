DEF INPUT PARAMETER p-tipo AS INTEGER NO-UNDO.
DEF INPUT PARAMETER p-empresa AS INTEGER NO-UNDO.
DEF INPUT PARAMETER p-cuadrilla AS INTEGER NO-UNDO.
define input parameter p-fecha as date no-undo.
define input parameter p-referencia as date no-undo.

DEF VAR v_causa AS INTEGER. 
DEF VAR v_genera AS LOGICAL.
DEF VAR v_existen AS INTEGER.
DEF BUFFER bctrol For liq_control_finca.
DEF BUFFER bitemsctrol FOR  liq_items_control_finca.
DEF BUFFER bctrollote FOR liq_control_finca_lotes.
DEF BUFFER bctrol-01 FOR liq_control_finca.
DEF VAR v_nro AS INTEGER.
DEF VAR v_cuenta AS INTEGER.


IF p-empresa = 0  THEN
DO:
    MESSAGE "Debe ingresar una empresa particular para la prueba" VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

IF p-cuadrilla = 0  THEN
DO:
    MESSAGE "Debe ingresar una cuadrilla particular para la prueba" VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.


CASE p-tipo:
    WHEN 1 THEN v_causa = 6. /* lluvia */
    WHEN 2 THEN v_causa = 8. /*ausente sin aviso */
END CASE.

     FOR EACH liq_items_control_finca WHERE
            liq_items_control_finca.id_empresa = p-empresa AND
            liq_items_control_finca.id_cuadrilla = p-cuadrilla AND
            liq_items_control_finca.fecha = p-fecha AND
            liq_items_control_finca.id_causa = v_causa AND
            liq_items_control_finca.c_usuario = "c-genauto":
                v_cuenta = v_cuenta + 1.
                DELETE liq_items_control_finca.
     END.

        
MESSAGE "Se borraron " + STRING(v_cuenta) + " registros automaticos" SKIP 
        "con causa " + string(v_causa) VIEW-AS ALERT-BOX INFORMATION.                  
