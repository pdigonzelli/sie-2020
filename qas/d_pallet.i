/****************************************************************************/
/*  NOMBRE PROGRAMA......:   d_pallet.i -libreria                           */
/****************************************************************************/
/*  FECHA CREACION ......:   01/04/03                                       */
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/
/*
 *
 * PARAMETROS PASADOS EN LINEA
 *
 *   {&archivo1}            = nombre del archivo
 *   {&archivo2}            = nombre del archivo
 *   {&use_index1}          = use-index + nombre del índice a utilizar
 *   {&use_index2}          = use-index + nombre del índice a utilizar
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
 *   {&condicion_where1}    = where - condici¢n para establecer relaci¢n/condici¢n de selecci¢n
 *   {&condicion_where2}    = where - condici¢n para establecer relaci¢n/condici¢n de selecci¢n
 *   {&variables_busqueda}  = variables utilizadas en procedimiento de b£squeda
 *   {&campos_display}      = campos que se visualizan en form
 *   {&campos_display1}     = campos que se visualizan y se a¤aden -display, insert-
 *   {&campos_display2}     = campos que se visualizan y se a¤aden -display, insert-
 *   {&campos_update}       = campos que se actualizan -update- opci¢n GET -F5-
 *   {&relaciones}          = procedimientos de campos relacionados
 *   {&creacion}            = procedimiento adicional de alta (pre update)
 *   {&campos_create}       = campos que se dan de alta -update del create-
 *   {&modificacion}        = procedimiento adicional de alta (pos update) o modificaci¢n
 *   (&proceso_display_cab) = proceso de visualizacion de campos de cabecera
 *   {&campos_display_cab}  = campos displayados en browser de cabecera
 *   {&funciones}           = funciones adicionales
 *
*/


SESSION:DATA-ENTRY-RETURN = TRUE.

ON F1 CLEAR.
ON F2 CLEAR.
ON F3 ENTER-MENUBAR.
ON F4 END-ERROR.
ON F5 CLEAR.
ON F6 CLEAR.
ON F7 CLEAR.
ON F8 CLEAR.
ON F9 CLEAR.
ON F10 CLEAR.
ON F11 CLEAR.
ON F12 CLEAR.

{{&parametros}}

{{&variables_busqueda}}

define variable v_columna as decimal.
define variable funcion as logical.

define variable q as rowid.
define variable estado_alta as logical initial false. 
define variable respuesta  as  logical format "Si/No" initial "No".


&IF "{&temp_table}" = "" or "{&temp_table}" = "no" &THEN 
    define new shared buffer {&archivo1} for {&archivo1}.
    define new shared buffer {&archivo2} for {&archivo2}.
&ENDIF            

/********** Botones del browser **********/
define button b_add label "&Agregar"
    size 14 by 1.19.

define button b_imprim label "&Etiqueta"
    size 14 by 1.19.

/********** Frame de botones del browse **********/
define frame botones
    b_add b_imprim with three-d . 

/********** Botones del window **********/
define button bu1 label "&Salir" size-chars 10 by 3.
define button b_ok label "OK" AUTO-GO size 14 by 1.

/********** Form Campos Amplia Cabecera ***********/
form 
    {{&campos_amplia}} 
    b_ok at 50 with frame amplia view-as dialog-box side-labels
    3 columns three-d.

/********** Form de la frame trabajo  ***********/
form
    {{&campos_display}}
    with frame trabajo view-as dialog-box centered 2 columns  
    overlay side-labels three-d width 110.

/********** Query del viewer **********/
define query qcab for {&archivo_cabecera} scrolling.

/********* Query del browser1 **********/
define query qb1 for {&archivo1} {&otros_archivos1} scrolling.
define query qb2 for {&archivo2} {&otros_archivos2} scrolling.

/********** Browse del browser **********/
define browse b1 query qb1 no-lock
display {{&campos_display1}} 
    with 12 down size 32.5 by 11.2 SEPARATORS font 0.

define browse b2 query qb2 no-lock
display {{&campos_display2}} 
    with 12 down size 103 by 11.2 SEPARATORS font 0.

/********** Frame del viewer, browser y botones *******/
define frame f-base
    {{&campos_display_cab}}
    skip 
    b1  
    skip 
    b2  
    skip(1)
    with {&with_form_trabajo} view-as dialog-box three-d
    scrollable width 140.


/********** Ubicacion en pantalla de los browsers *********/
b1:COLUMN-SCROLLING = yes.
b2:COLUMN-SCROLLING = yes.
frame f-base:row = 1.
frame f-base:height-chars = 24. /*frame f-base:height-chars + 3.*/
if frame f-base:width-chars < 55 Then frame f-base:width-chars = 55. 
  
b1:col = 2.
b1:row = 9.2.

b2:col = 35.5.
b2:row = 9.2.


/********** Abre query del viewer **********/
open query qcab for each {&archivo_cabecera} {&use_index_cabecera}
    {&condicion_where_cab} no-lock.


/**********FUNCIONES ADICIONALES****************************/
{{&funciones}}

/***********************************************************/

/******************MOVIMIENTO EN BROWSE**************/ 
on "VALUE-CHANGED" OF b1 or "MOUSE-SELECT-CLICK" of b1 DO:
    open query qb2 for each {&archivo2} {&use_index2} {{&condicion_where2}} {&archivos_relacion2}.
    {{&proceso_display_cab}}
end.


/*************RELACIONES******************/
on "F3" of frame trabajo or "RETURN" of frame trabajo or "TAB"
    of frame trabajo anywhere do:
    {{&relaciones}}.
end.


/******************AGREGAR REGISTROS**************/ 
on "F9" OF b1 or "INSERT" of b1 or "CHOOSE" of b_add
    or "RETURN" of b_add do:

    _sale: 
    do on endkey undo, leave :
        estado_alta = yes. 
        frame trabajo:title = "Agregar".
        
        create {&archivo1}.
        {{&creacion}}
        update {{&campos_create}} with frame trabajo.

        assign {&archivo1}.c_usuario = userid("userdb") 
               {&archivo1}.c_fecha = today
               {&archivo1}.c_hora = string(time, "hh:mm:ss").

        {{&modificacion}}
        if keyfunction(lastkey) <> "RETURN" then
            undo _sale, leave _sale.
    end.

    hide frame trabajo no-pause.
    {{&proceso_display_cab}}
    run abre_query.
    estado_alta = false.    
    apply "entry" to b_add in frame botones.

end.

on window-close of frame f-base do:
    apply "RETURN" to this-procedure.
end.   

on window-close of frame trabajo do:
    apply "END-ERROR" to frame trabajo.
end. 

on window-close of frame amplia do:
    apply "END-ERROR" to frame amplia.
end. 


/****************Principal********************/

frame botones:frame = frame f-base:handle.
frame botones:row = frame f-base:height-chars - 2.5.

v_columna = (frame f-base:width-chars - frame botones:width-chars) / 2.
if v_columna < 1 Then v_columna = 1.
frame botones:col = v_columna.

funcion = yes.
find last {&archivo_cabecera} {&use_index_cabecera} {&condicion_where_cab} no-lock no-error.
if available {&archivo_cabecera} then do:
    {{&proceso_display_cab}}
    run abre_query.
end.
funcion = no.


FIND FIRST usuarios_produccion WHERE
    usuarios_produccion.id_usuario = USERID("userdb") NO-LOCK NO-ERROR.
    
IF AVAILABLE usuarios_produccion AND
    (usuarios_produccion.id_usuario = "computos" OR
    usuarios_produccion.id_usuario = "m_gabriel" OR
    usuarios_produccion.id_usuario = "m_gabrielf") THEN DO:

    enable b_add b_imprim with frame botones.
    apply "entry" to b_add in frame botones.
END.
ELSE DO:
    disable b_add with frame botones.
    enable b_imprim with frame botones.
    apply "entry" to b_imprim in frame botones.
END.

enable b1 b2 with frame f-base.

wait-for "return" of this-procedure.


/******** Abre querys de los browsers *******/
procedure abre_query:
    open query qb1 for each {&archivo1} {&use_index1} {{&condicion_where1}} {&archivos_relacion1}.
    open query qb2 for each {&archivo2} {&use_index2} {{&condicion_where2}} {&archivos_relacion2}.
end procedure.
