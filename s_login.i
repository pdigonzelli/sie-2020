/*--- DESCONEXI‡N DE BASES DE DATOS DEL SISTEMA ---*/
repeat contador = 1 to num-entries(conexion):
  if connected(entry(contador, conexion)) then
    disconnect value(entry(contador, conexion)) no-error.
  if connected(entry(contador, conexion)) then
    message color white/red
            "La Base de Datos [" + par_bases.id_base + "] No se ha Podido Desconectar !!!" skip(1)
            "Esto puede ocasionar problemas en las aplicaciones. Le recomendamos que salga"
            "del sistema y vuelva a entrar (re-arranque la aplicaci¢n). Muchas gracias."
            view-as alert-box warning.
end.
conexion = "".

/*--- SOLO PARA GESSI ---*/
&IF "{&SISTEMA}" = "gessi" &THEN
  if connected("sucursal") then
    disconnect sucursal.
&ENDIF
