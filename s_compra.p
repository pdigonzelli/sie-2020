/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_compra.p                                     */
/****************************************************************************/
/*  Programa para actividades globales de los sistemas.                     */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  FECHA CREACION.......:   30/03/96                                       */
/*  REVISION.............:   1.00                                           */
/****************************************************************************/
/*  PROGRAMADOR..........:   Grupo Sƒuken - Elena Grimozzi                  */
/*  FECHA MODIFICACION...:   29/04/99                                       */
/*  MODIFICACION.........:   Conversi¢n a GUI                               */
/****************************************************************************/


/*--- VARIABLES GLOBALES DEL SISTEMA DE COMPRAS ---*/
define new global shared variable vlc_ubicacion like sucursales.id_sucursal.
define new global shared variable vlc_suc_origen AS INTEGER.

/*--- PARAMETROS ---*/
define output parameter sal as logical initial no.

RUN ../s_core.p (OUTPUT sal).
RUN sSucursales.w(OUTPUT vlc_suc_origen).

/*--- MODULO ESTANDAR ---*/
find first par_estado no-lock.
assign vlc_ubicacion = par_estado.id_suc_origen
       sal           = yes.
release par_estado.
return.


/*--- PROGRAMA ESPECIFICO DE SUPAMER S.A. y CASA PETRINI S.A.

La variable "vlc_ubicacion" se utiliza para poder direccionar los reportes
On-Line hacia distintas impresoras (en funcion de donde se esta comprando),
usando la tabla de excepciones (par_impre_form) de impresion.


&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
define variable suc  as integer extent 4 initial [0,93,99,105].
define variable menu as character extent 4 format "x(24)" initial
       [ "     Administraci¢n     ",
         " Centro de Perecederos  ",
         "      Panificadora      ",
         "    Dep¢sito Insumos    "].

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    display menu
       with frame menu centered overlay no-labels row 9 color white/green 1 columns
                      title color white/red " Selecci¢n de ubicaci¢n ".
&ELSE
    display menu
       with frame menu centered overlay no-labels 1 columns
                      title ' Elija ubicaci¢n '
                      three-d view-as dialog-box.
    {s_close.i                
     &frame = "menu"}
                      
&ENDIF
repeat:
  choose field menu color white/red auto-return with frame menu.
  assign vlc_ubicacion = suc[frame-index].
  if frame-index <> 0 then
   do:
      assign sal = yes.
      leave.
   end.
end.
hide frame menu no-pause.
return.

/*************************/
/* SOPORTE MODALIDAD GUI */
/*************************/
&ELSE
DEFINE BUTTON btn_aceptar AUTO-END-KEY 
     LABEL "&Aceptar" 
     SIZE 15 BY 1.14 TOOLTIP "Aceptar ubicaci¢n e ingresar al m¢dulo seleccionado".

DEFINE BUTTON btn_salir AUTO-END-KEY 
     LABEL "&Salir" 
     SIZE 15 BY 1.14 TOOLTIP "Regresar a la pantalla anterior".

DEFINE VARIABLE r-ubicacion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Administraci¢n", 0,
"Centro de Perecederos", 93,
"Panificadora", 99,
"Dep¢sito Insumos", 105
     SIZE 27 BY 4.05 TOOLTIP "Ubicaciones disponibles" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 5.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 2.62.

DEFINE FRAME Dialog-Frame
     r-ubicacion AT ROW 3.14 COL 10 NO-LABEL
     btn_aceptar AT ROW 8.86 COL 7
     btn_salir AT ROW 8.86 COL 24
     "Ubicaciones:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.48 COL 3
          FONT 6
     RECT-1 AT ROW 2.43 COL 3
     RECT-2 AT ROW 8.14 COL 3
     SPACE(1.79) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         NO-UNDERLINE THREE-D
         TITLE "Selecci¢n de Ubicaci¢n"
         DEFAULT-BUTTON btn_aceptar CANCEL-BUTTON btn_salir.

ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Selecci¢n de Ubicaci¢n */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

ON CHOOSE OF btn_aceptar IN FRAME Dialog-Frame /* Aceptar */
DO:
   assign r-ubicacion.
   assign vlc_ubicacion = r-ubicacion
                    sal = yes.
END.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME dialog-frame.
END.
RUN disable_UI.
return.

PROCEDURE disable_UI :
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

PROCEDURE enable_UI :
  DISPLAY r-ubicacion 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 r-ubicacion RECT-2 btn_aceptar btn_salir 
      WITH FRAME Dialog-Frame.
END PROCEDURE.
&ENDIF

--- FIN DE PROGRAMA ESPECIFICO ---*/

