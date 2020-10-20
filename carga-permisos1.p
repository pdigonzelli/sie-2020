define temp-table tt like general.par_menu_grupos.


for each par_menu_grupos:
    assign par_menu_grupos.accion_seleccion_gui = "".
end.    

for each par_menu_grupos :
    if par_menu_grupos.accion_seleccion_gui = "" then
        assign par_menu_grupos.accion_seleccion_gui = par_menu_grupos.accion_seleccion.
    find par_permisos where par_permisos.nombre_programa = par_menu_grupos.accion_seleccion_gui no-error.    
    if not available par_permisos then
    do:
        create par_permisos.
        assign par_permisos.nombre_programa = par_menu_grupos.accion_seleccion_gui
               par_permisos.puede_ejecutar  = par_menu_grupos.letra_inicial + "_demo".
    end.
    par_permisos.puede_ejecutar  = "*".
end.

/*
input from  c:\temp\gessituc.txt. 

repeat :
    create tt.
    import tt.
    find par_menu_grupos where par_menu_grupos.letra_inicial = tt.letra_inicial and
                               par_menu_grupos.item_menu = tt.item_menu  no-error.
    if not available par_menu_grupos then next.                           
    /*
    if not available par_menu_grupos then 
    do:
        create par_menu_grupos.
        buffer-copy tt to par_menu_grupos.
    end.
    */
    assign par_menu_grupos.accion_seleccion_gui = tt.accion_seleccion_gui.
    delete tt.
end.    

input close.

*/

for each par_menu_grupos :
    if par_menu_grupos.accion_seleccion_gui = "" then
        assign par_menu_grupos.accion_seleccion_gui = par_menu_grupos.accion_seleccion.
    find par_permisos where par_permisos.nombre_programa = par_menu_grupos.accion_seleccion_gui no-error.    
    if not available par_permisos then
    do:
        create par_permisos.
        assign par_permisos.nombre_programa = par_menu_grupos.accion_seleccion_gui
               par_permisos.puede_ejecutar  = par_menu_grupos.letra_inicial + "_demo".
    end.
    par_permisos.puede_ejecutar  = "*".
end.
