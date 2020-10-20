/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_movcax.i                                     */
/****************************************************************************/
/*  FECHA CREACION ......:   19/12/2011                                     */
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/*  MODIFICACION.........:   Libreria con cabecera y detalle                */
/****************************************************************************/
/*
 *
 * PARAMETROS PASADOS EN LINEA
 *
 *   {&programa}            = nombre del programa
 *   {&archivo}             = nombre del archivo
 *   {&use_index}           = use-index + nombre del índice a utilizar
 *   {&otros_archivos1}     = nombre de otros archivos detalle separados por , 
 *   {&archivos_relacion1}  = expresion relacion de archivos detalle (completa query)
 *   {&otros_archivos2}     = nombre de otros archivos detalle separados por , 
 *   {&archivos_relacion2}  = expresion relacion de archivos detalle (completa query)
 *   {&with_form_trabajo}   = datos faltantes de la frame de trabajo
 *   {&color_trabajo}       = color de la frame de trabajo
 *   {&temp_table}          = yes/no                                     
 *   {&archivo_cabecera}    = nombre del archivo de cabecera
 *   {&use_index_cabecera}  = use-index + nombre del índice a utilizar cabecera
 *   {&condicion_where_cab} = where - condici¢n para establecer relaci¢n cabecera
 *
 * PARAMETROS PASADOS A TRAVES DE ARCHIVOS
 *
 *   {&condicion_where}     = where - condici¢n para establecer relaci¢n/condici¢n de selecci¢n
 *   {&variables_busqueda}  = variables utilizadas en procedimiento de b£squeda
 *   {&proceso_display}     = proceso previo a la visualizacion de los campos display
 *   {&campos_display}      = campos que se visualizan en form
 *   {&campos_display1}     = campos que se visualizan y se a¤aden -display, insert-
 *   {&campos_display2}     = campos que se visualizan y se a¤aden -display, insert-
 *   {&busqueda}            = procedimiento de b£squeda
 *   {&relaciones}          = procedimientos de campos relacionados
 *   {&borrado}             = procedimiento de borrado de informaci¢n relacionada
 *   {&borrado_posterior}   = procedimiento de borrado de informaci¢n posterior
 *   {&creacion}            = procedimiento adicional de alta (pre update)
 *   {&campos_create}       = campos que se dan de alta -update del create-
 *   {&modificacion}        = procedimiento adicional de alta (pos update) o modificaci¢n
 *   (&proceso_display_cab) = proceso de visualizacion de campos de cabecera
 *   {&campos_display_cab}  = campos displayados en browser de cabecera
 *   {&funciones}           = funciones adicionales
 *
*/

propath = propath + ",n:/supervisor".

SESSION:DATA-ENTRY-RETURN = TRUE.

ON F3 ENTER-MENUBAR.
ON F4 END-ERROR.
ON F8 CLEAR.

{{&variables_busqueda}}

define variable v_columna as decimal.
define variable v_columna01 as decimal.
define variable v_columna_browse as decimal.
define variable v_columna_browse2 as decimal.
define variable funcion as logical.
define variable x_borrar as logical initial true.

define variable q as rowid.
define variable estado_upd  as logical initial false. 
define variable respuesta  as  logical format "Si/No" initial "No".
define variable vali_OK as logical no-undo.
define variable g_actualiza as character format "x(28)" initial "ACTUALIZANDO SALDOS ...".


&IF "{&temp_table}" = "" or "{&temp_table}" = "no" &THEN 
    define new shared buffer {&archivo} for {&archivo}.
&ENDIF            

/********** Botones del browser **********/
define button b_add label "&Agregar"
    size 11 by 1.19.

define button b_borrar label "&Borrar"
    size 11 by 1.19.

define button b_etique label "&Etiquetas Partida"
    size 20 by 1.19.

/********** Frame de botones del browse **********/
define frame botones
    b_add b_borrar b_etique with three-d . 

/********** Botones del window **********/
define button bu1 label "&Salir" size-chars 10 by 3.
define button b_ok label "OK" AUTO-GO size 14 by 1.

/********** Form de la frame trabajo  ***********/
form
    {{&campos_display}}
    with frame trabajo view-as dialog-box centered 2 columns  
    overlay side-labels three-d width 118.

form
    xclave at row 1 column 3  format "x(10)" WIDGET-ID 2 BLANK
    with frame carga_clave centered no-labels overlay
    title "Ingrese Clave"
    width 22 three-d view-as dialog-box.

/********* Query del browser1 **********/
define query qb1 for {&archivo} {&otros_archivos1} scrolling.

/********* Query del browser2 **********/
define query qb2 for {&archivo} {&otros_archivos2} scrolling.

/********** Browse del browser **********/
define browse b1 query qb1 no-lock
display {{&campos_display1}} 
    with 8 down size 131.2 by 8 SEPARATORS font 0.

define browse b2 query qb2 no-lock
display {{&campos_display2}} 
    with 8 down size 131.2 by 8 SEPARATORS font 0.

/********** Frame del viewer, browser y botones *******/
define frame f-base
    {{&campos_display_cab}}
    skip 
    b1  
    skip(1)
    b2
    skip(1)
    with {&with_form_trabajo} view-as dialog-box three-d scrollable.


/********** Ubicacion en pantalla de los browsers *********/
b1:COLUMN-SCROLLING = yes.
b2:COLUMN-SCROLLING = yes.
frame f-base:row = 1.
frame f-base:height-chars = 24.5. /*frame f-base:height-chars + 3.*/
if frame f-base:width-chars < 55 Then frame f-base:width-chars = 55. 
  
v_columna_browse = (frame f-base:width-chars - b1:width-chars) / 2. 
if v_columna_browse < 1 Then v_columna_browse = 1.

v_columna_browse2 = (frame f-base:width-chars - b2:width-chars) / 2. 
if v_columna_browse2 < 1 Then v_columna_browse2 = 1.

b1:col = v_columna_browse.
b2:col = v_columna_browse2.

b1:row = 4.5.
b2:row = 13.


/******* Actualiza Saldos *******/
form
    skip(1)
    space(3)
    g_actualiza
    skip(1)
    with frame actualiza centered no-labels overlay
    at col 1 row 5 bgcolor 1 fgcolor 14 font 6
    width 40 three-d.

ASSIGN FRAME actualiza:FRAME = FRAME f-base:HANDLE.
hide frame actualiza.


/**********FUNCIONES ADICIONALES****************************/
{{&funciones}}

/***********************************************************/

/*************RELACIONES******************/
on "F3" of frame trabajo or "RETURN" of frame trabajo anywhere do:
    {{&relaciones}}.
end.

/******************AGREGAR REGISTROS**************/ 
on "F9" OF  b1 or "INSERT" of b1 or "CHOOSE" of b_add or "RETURN" of b_add DO:

    _sale: 
    do on endkey undo, leave :
        frame trabajo:title = "Agregar".
        
        create {&archivo}.
        {{&creacion}}
        update {{&campos_create}} with frame trabajo.

        assign {&archivo}.c_usuario = userid("userdb") 
               {&archivo}.c_fecha = today
               {&archivo}.c_hora = string(time, "hh:mm:ss").

        {{&modificacion}}

        if KEY <> "RETURN" then
            undo _sale, leave _sale.
    end.

    hide frame trabajo no-pause.
    run abre_query.

end.

/****************BORRAR REGISTROS*************/
on "DELETE-CHARACTER"  of b1 or "CHOOSE" of b_borrar do:

    define variable respuesta as logical format "Si/No" initial "No".

    _sale:
    do on error undo, retry:
        do transaction on endkey undo _sale, leave _sale on error undo _sale, leave _sale:
            bell.
            message " Borrar  el registro !!. Est  seguro?" 
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE ""
                update respuesta auto-return.
            if respuesta = no then
              undo _sale, leave _sale.

            get current qb1 exclusive-lock. 
            if available {&archivo} then do:
                {{&borrado}}
                if x_borrar then do:
                    delete {&archivo}.
                    {{&borrado_posterior}}
                    run abre_query.
                end.
            end.
        end.
    end.

    apply "entry" to b_add in frame botones.
end.

/******************AGREGAR REGISTROS**************/ 
on "VALUE-CHANGED" OF b1 or "MOUSE-SELECT-CLICK" of b1 DO:
    q = rowid({&archivo}).
    reposition qb2 to rowid q.
end.

/******************AGREGAR REGISTROS**************/ 
on "VALUE-CHANGED" OF b2 or "MOUSE-SELECT-CLICK" of b2 DO:
    q = rowid({&archivo}).
    reposition qb1 to rowid q.
end.


on window-close of frame f-base do:
    apply "RETURN" to this-procedure.
end.   

on window-close of frame trabajo do:
    apply "END-ERROR" to frame trabajo.
end. 


/****************Principal********************/

frame botones:frame = frame f-base:handle.
frame botones:row = frame f-base:height-chars - 2.5.

v_columna = (frame f-base:width-chars - frame botones:width-chars) / 2.
if v_columna < 1 Then v_columna = 1.
frame botones:col = v_columna.

funcion = yes.
{{&proceso_display}}

find last {&archivo_cabecera} {&use_index_cabecera} {&condicion_where_cab} no-lock no-error.
if available {&archivo_cabecera} then do:
    {{&proceso_display_cab}}
    run abre_query.
end.
else do:
    {{&proceso_display_cab}}
end.
funcion = no.

ENABLE b_add b_borrar b_etique WITH FRAME botones.

enable b1 b2 with frame f-base.

wait-for "return" of this-procedure.


/******** Abre querys de los browsers *******/
procedure abre_query:
    open query qb1 for each {&archivo} {&use_index} {{&condicion_where}} {&archivos_relacion1}.
    open query qb2 for each {&archivo} {&use_index} {{&condicion_where}} {&archivos_relacion2}.
end procedure.
