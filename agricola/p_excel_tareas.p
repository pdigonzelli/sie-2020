def var p_archivo as character.
def var v_grupo as character.

{s_varsis.i}
p_archivo = vlc_dir_temp + "tareas.txt".

output to value(p_archivo).
put "Sector;Grupo;Cod;Abreviatura;Descripcion".
put skip.
for each tareas WHERE tareas.vigente NO-LOCK BY id_grupo BY abreviatura:
   find first grupos_tareas of tareas no-lock no-error.
   if available grupos_tareas Then v_grupo = grupos_tareas.descripcion.
                              Else v_grupo = "".
   export delimiter ";" 
   tareas.id_sector v_grupo
   id_tarea tareas.abreviatura tareas.descripcion.
end.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
