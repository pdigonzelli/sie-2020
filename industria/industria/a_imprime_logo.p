/*** Elimina Grafico de Impresora ***/

output to VALUE("\\192.168.1.104\ibm4400").

put control "^XA^IDR:lgsmver.grf^XZ".

output close.

message "Archivo Eliminado"
    view-as alert-box.


/*** Carga Grafico en la Impresora desde C:\temp ***/

define var cFile as character.
define var cAux as character.

input from "..\industria\lgsmver.grf".
repeat:
  import unformatted cAux .
  cFile = cFile + cAux.
end.

input close.

output to VALUE("\\192.168.1.104\ibm4400").
put control cFile.

output close.

message "Archivo Cargado"
    view-as alert-box.


/*** Imprime Grafico ***/

output to VALUE("\\192.168.1.104\ibm4400").

put control "^XA".
put control "^PQ" 1 "^FS".
put control "^FO100,100^XGR:lgsmver.grf,1,1^FS".
put control "^XZ".

output close.
