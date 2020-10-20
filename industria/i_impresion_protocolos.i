DEFINE INPUT PARAMETER r_prot AS ROWID.

define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var v_oe as integer.
define var v_filtro as character.
define var v_fecha as character.
DEFINE VAR v_o_f AS CHAR.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.

{i_impresion_protocolos_2.i}
