/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_carpax.p                                    */
/****************************************************************************/
/*  Permite realizar carga de movimientos en packing                        */
/*  Llama a los programas: dd_carpx1.p - dd_movdes.p                        */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

session:time-source = "produccion". 

/*********** Variables de carga ****************/
define new shared variable x_fecha_des     as date format "99/99/9999".
define new shared variable x_fecha_has     as date.
define new shared variable x_hora_des      as character    format "99:99".
define new shared variable x_hora_has      as character    format "99:99".
define new shared variable x_materia       like productos_terminados.id_articulo initial 1.
define new shared variable x_variedad      like variedades.id_variedad initial 1.
define new shared variable x_packing       as integer.

define variable g_procesa       as character format "x(20)" initial "PROCESANDO ...".
define variable tipo_salida     as logical.
define variable x_hora_des1     as character.
define variable x_hora_has1     as character.

/*********** Variables de busqueda ****************/
define variable x1              like productos_terminados.id_articulo.
define variable x2              like variedades.id_variedad.

/********** Saldos Playa **********/
define new shared temp-table x_playa
    field id_sucursal       like  items_stock.id_sucursal
    field id_articulo       like  items_stock.id_articulo
    field id_variedad       like  items_stock.id_variedad
    field id_proveedor      like  items_stock.id_proveedor
    field id_origen         like  items_stock.id_origen
    field id_tipo_cosecha   like  items_stock.id_tipo_cosecha
    field ingresos          as DECIMAL
    index x_playa as primary unique
        id_sucursal
        id_proveedor
        id_origen
        id_articulo
        id_variedad
        id_tipo_cosecha.

/********** Saldos Camaras **********/
define new shared temp-table x_camaras
    field id_sucursal       like  items_stock.id_sucursal
    field id_articulo       like  items_stock.id_articulo
    field id_variedad       like  items_stock.id_variedad
    field id_color          like  items_stock.id_color
    field estado_fruta      like  items_stock.estado_fruta
    field fecha             like  items_stock.fecha
    field ingresos          as DECIMAL
    index x_camaras as primary unique
        id_sucursal
        id_articulo
        id_variedad
        id_color
        estado_fruta
        fecha.

x_fecha_des = today.
x_hora_des  = "0600".
x_fecha_has = today + 1.
x_hora_has  = "0600".

SESSION:DATA-ENTRY-RETURN = TRUE.

define button b_aceptar label "&Aceptar"
size 12 by 1.19.  

define button b_salir label "&Salir"
size 12 by 1.19.  

form
    "Proceso del Dia"                   at row 2    col 7
    x_fecha_des                         at row 2    col 25
    "Materia Prima"                     at row 3.5  col 7
    x_materia                           at row 3.5  col 25
    productos_terminados.abreviatura    at row 3.5  col 44  format "x(20)"
    "Variedad"                          at row 5    col 7
    x_variedad                          at row 5    col 25
    variedades.abreviatura              at row 5    col 44  format "x(20)"
    b_aceptar                           at row 7.5  col 15
    b_salir                             at row 7.5  col 43  skip(1)
    with frame ing centered no-labels overlay
        title " Carga de Movimientos en Packing (Lavalle) "
        width 70 three-d view-as dialog-box.

/******* Procesando *******/
form
    skip(1)
    space(3)
    g_procesa
    skip(1)
    with frame procesando centered no-labels overlay
    at col 1 row 5 bgcolor 1 fgcolor 14 font 6
    width 30 three-d.


ASSIGN FRAME procesando:FRAME = FRAME ing:HANDLE.
hide frame procesando.


/********** Habilita radio buttons y botones **********/
enable b_aceptar    with frame ing.
enable b_salir      with frame ing.

/********** Cierre de Windows **********/
on window-close of frame ing do:
    apply "END-ERROR" to frame ing.
end.
on "END-ERROR" of frame ing do:
    hide frame ing.
end.

/********** Busquedas **********/
on "F3" of frame ing or "ENTER" of frame ing anywhere do:

/** Fecha Desde **/
    if frame-field = "x_fecha_des" then do:
/*
        if input x_fecha_des < today - 6 then do:
            message "ERROR - LA FECHA NO PUEDE SER 7 DIAS MENOR AL DIA DE HOY"
            view-as alert-box.
            x_fecha_des = today.
            display x_fecha_des
                with frame ing.
            apply "entry" to x_fecha_des in frame ing.
            return no-apply.
        end.
*/  
        if input x_fecha_des > today then do:
            message "ERROR - LA FECHA NO PUEDE MAYOR AL DIA DE HOY"
            view-as alert-box.
            x_fecha_des = today.
            display x_fecha_des
                with frame ing.
            apply "entry" to x_fecha_des in frame ing.
            return no-apply.
        end.
    end.

/** Materia Prima **/
    if frame-field = "x_materia" then do:
        if keyfunction(lastkey) = "ENTER-MENUBAR" then do:
            x1 = input x_materia.
            run xd_matco.p (input-output x1).
            display x1 @ x_materia with frame ing.
        end.
        find productos_terminados where
            productos_terminados.id_articulo = input x_materia and
            productos_terminados.id_tipo_articulo = 1 no-lock no-error.
        if available productos_terminados then
            display productos_terminados.abreviatura @ productos_terminados.abreviatura
                with frame ing.
        else do:
            x_materia = 1.
            find productos_terminados where
                productos_terminados.id_articulo = x_materia no-lock.
            display x_materia @ x_materia
                productos_terminados.abreviatura @ productos_terminados.abreviatura
                with frame ing.
        end.
    end.

/** Variedad **/
    if frame-field = "x_variedad" then do:
        if keyfunction(lastkey) = "ENTER-MENUBAR" then do:
            x2 = input x_variedad.
            run xd_varco.p (input-output x2,
                input integer(x_materia:screen-value)).
            display x2 @ x_variedad with frame ing.
        end.
        find variedades where
            variedades.id_variedad = input x_variedad and
            variedades.id_articulo = integer(x_materia:screen-value)
            no-lock no-error.
        if available variedades then
            display variedades.abreviatura @ variedades.abreviatura
                with frame ing.
        else do:
            x_variedad = 1.
            find variedades where
                variedades.id_variedad = x_variedad no-lock.
            display x_variedad @ x_variedad
                variedades.abreviatura @ variedades.abreviatura
                with frame ing.
        end.
    end.

end.

/************ Acepta los datos ************/
on CHOOSE of b_aceptar or RETURN of b_aceptar in frame ing do:
    x_fecha_has = date(x_fecha_des:screen-value) + 1.
    view frame procesando.
    display g_procesa with frame procesando.
    run busqueda_movimientos.
    hide frame procesando no-pause.

    x_materia = 1.
    x_variedad = 1.

    find productos_terminados where
        productos_terminados.id_articulo = x_materia no-lock.
    find variedades where
        variedades.id_variedad = x_variedad no-lock.

    display productos_terminados.abreviatura @ productos_terminados.abreviatura
        variedades.abreviatura @ variedades.abreviatura
        with frame ing.

    run display_inputs.
    apply "entry" to x_fecha_des in frame ing.
end.  

/************ Sale del programa ************/
on CHOOSE of b_salir or RETURN of b_salir in frame ing do:
  apply "END-ERROR" to frame ing.
end.

/********** Principal Carga de Datos **********/
    run display_inputs.

    find productos_terminados where
        productos_terminados.id_articulo = x_materia no-lock.
    find variedades where
        variedades.id_variedad = x_variedad no-lock.

    display productos_terminados.abreviatura @ productos_terminados.abreviatura
        variedades.abreviatura @ variedades.abreviatura
        with frame ing.

    set x_fecha_des
        x_materia
        x_variedad
        with frame ing.
 
/*********************************************************/
/*   Busqueda de movimientos segun seleccion             */
/*********************************************************/
procedure busqueda_movimientos:

    for each sucursales_packing where
        sucursales_packing.orden < 700:
        assign
            sucursales_packing.cantidad  = 0
            sucursales_packing.cantidad1 = 0.
    end.
    release sucursales_packing.

    x_fecha_des = date(x_fecha_des:screen-value in frame ing).
    x_materia   = integer(x_materia:screen-value in frame ing).
    x_variedad  = integer(x_variedad:screen-value in frame ing).
    x_packing   = 1.

    run dd_carpx1.p (input x_fecha_des,
        input x_fecha_has,
        input x_materia,
        input x_variedad).

    run dd_movdes.p.

end procedure.

/*********** Display de campos de inputs ***********/
procedure display_inputs:
    display
        x_fecha_des
        x_materia
        x_variedad
        with frame ing.
end procedure.
