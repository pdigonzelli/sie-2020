for each par_grupos no-lock where letra_inicial = "w" :
    for each par_menu_grupos where par_menu_grupos.letra_inicial = par_grupos.letra_inicial .
        assign accion_seleccion_gui = accion_seleccion.
        find par_permisos where nombre_programa = accion_seleccion_gui no-error.
        if not available par_permisos then 
        do:
            create par_permisos .
            assign nombre_programa = accion_seleccion_gui.
        end.
        assign puede_ejecutar = puede_ejecutar + "," + par_menu_grupos.letra_inicial + "_demo".
    end.
end.
