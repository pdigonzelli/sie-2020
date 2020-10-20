
define output parameter sal as logical initial no format "Si/No".

/*--- PROCEDIMIENTOS ESTANDAR ---*/
run ../s_core.p (output sal).
run ../s_cotizacion.p (output sal).
/* run ../s_faccob.p (output sal). */

/*--- VALIDACIONES ---*/
if opsys <> "UNIX" and
   session:temp-directory <> "c:~\winnt~\temp~\"   and 
   session:temp-directory <> "c:~\windows~\temp~\" and
   session:temp-directory <> "c:~\temp~\" then
  message "El directorio de temporarios de la sesi¢n que ha iniciado, puede no ser"
          "el adecuado para la visualizaci¢n e impresi¢n de los reportes de ‚ste m¢dulo." skip(1)
          "El directorio de temporarios es:" session:temp-directory skip(1)
          "El directorio de temporarios deber¡a contener carpetas de no m s de 8"
          "letras. Por favor, edite las propiedades del ¡cono de inicio de la aplicaci¢n"
          "y cambie, por ejemplo, -T %TEMP% a -T c:~\temp o avise a Gerencia de Sistemas."
          view-as alert-box warning.
if search("c:~\winnt~\fonts~\PF_I2OF5_TXT.ttf") = ? and search("c:~\windows~\fonts~\PF_I2OF5_TXT.ttf") = ? then
  message "Los juegos de caracteres o tipos de letras (fonts) necesarios para imprimir"
          "c¢digos de barras no se encuentran instalados en esta computadora." skip(1)
          "Si no va a imprimir formularios con c¢digos de barra puede ignorar esta alerta." skip(1)
          "Por favor, avise a Gerencia de Sistemas." view-as alert-box warning.

sal = yes.
return.
