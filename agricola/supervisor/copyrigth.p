/********************************************************************/
/*  NOMBRE PROGRAMA......:   copyrigth.p                            */
/********************************************************************/

/*--- SISTEMAS: (contacto, gessi, posmaster, nudelman, core) ---*/
{../sistema.i}

/*--- DEFINO TITULO ---*/
define variable c-tit as character format "x(50)" no-undo.
&IF "{&SISTEMA}" = "gessi" &THEN
  c-tit = "PAC-ERP GESSI".
&ENDIF
&IF "{&SISTEMA}" = "posmaster" &THEN
  c-tit = "PAC-ERP POS-MASTER".
&ENDIF
&IF "{&SISTEMA}" = "nudelman" &THEN
  c-tit = "SAUKEN-LAW".
&ENDIF
&IF "{&SISTEMA}" = "contacto" &THEN
  c-tit = "PAC-CRM".
&ENDIF
&IF "{&SISTEMA}" = "core" &THEN
  c-tit = "PAC-Core".
&ENDIF

input clear.
bell.
message "Aplicaci¢n Inform tica: " + c-tit skip(1)
        "Copyrigth " + string(year(today)) + " Grupo Sƒuken S.A." skip 
        "Registro de Propiedad Intelectual: 891218, 327720, 327721" skip(1)
        "Todos los Derechos Reservados" skip(1)
        "Prohibida su Copia y Reproducci¢n. Para utilizar esta"
        "aplicaci¢n debe disponer de una Licencia de Uso otorgada por Grupo"
        "Sƒuken S.A., PAC Software S.A. o de alguno de sus distribuidores autorizados." skip(1)
        "Grupo Sauken y PAC Software y sus derivados son Marcas Registradas." skip
        "Las Marcas y Logos que identifican a esta aplicaci¢n son Marcas Registradas."
        view-as alert-box title "Acerca de Aplicaci¢n Inform tica: " + c-tit.
hide message no-pause.
return.
