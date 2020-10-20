/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_faccob.p                                     */
/****************************************************************************/
/*  Programa para actividades globales de los sistemas para los m¢dulos de  */
/*  facturaci¢n y cobranzas                                                 */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  FECHA CREACION.......:   31/10/95                                       */
/*  REVISION.............:   1.10                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- VARIABLES GLOBALES DEL SISTEMA DE FACTURACION Y COBRANZAS ---*/
define new global shared variable vlc_sucursal    like puntos_venta.id_sucursal.
define new global shared variable vlc_punto_venta like puntos_venta.id_punto_venta.

/*--- PARAMETROS ---*/
define output parameter sal as logical initial no.

/*--- VARIABLES ---*/
define variable aux_campo  as  character.
define variable respuesta  as  logical format "Si/No" initial no.
define variable resp_ayuda as  logical format "Si/No" initial no.

define variable c-nombre   like sucursales.nombre no-undo.
define button b_aceptar    label "&Aceptar"  size 12 by 1.3 tooltip "Aceptar y continuar".
define button b_cancelar   label "&Cancelar" size 12 by 1.3 tooltip "Cancelar y regresar a pantalla inicial".

/*--- FORMULARIO ---*/
&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
     form 
          vlc_sucursal label " Sucursal"
                       help " Sucursal"
          validate(can-find(sucursales 
                      where sucursales.id_sucursal = input vlc_sucursal and
                            sucursales.estado = yes),
                      " Debe ingresar una sucursal existente")
          
          sucursales.nombre no-label
          vlc_punto_venta label "Punto de Venta"
                          help " Punto de Venta"
          validate(can-find(puntos_venta where 
                            puntos_venta.id_sucursal = input vlc_sucursal and
                            puntos_venta.id_punto_venta = input vlc_punto_venta),
                      " Debe ingresar un punto de venta existente")
      with frame sucdsp centered row 10 overlay side-labels color white/green.
&ELSE
     form skip(1)
          "Sucursal:"        at row 2 col 20 right-aligned
          vlc_sucursal      at row 2 col 22 no-label 
                       
          validate(can-find(sucursales 
                      where sucursales.id_sucursal = input vlc_sucursal and
                            sucursales.estado = yes),
                      " Debe ingresar una sucursal existente")
          
          sucursales.nombre at row 2 col 29 no-label
          "Punto de Venta:"  at row 3 col 20 right-aligned
          vlc_punto_venta   at row 3 col 22 no-label 

          validate(can-find(puntos_venta where 
                            puntos_venta.id_sucursal = input vlc_sucursal and
                            puntos_venta.id_punto_venta = input vlc_punto_venta),
                      " Debe ingresar un punto de venta existente")

          skip(1.2)
          space(18) 
          b_aceptar 
          space(5)
          b_cancelar skip
          with frame sucdsp centered row 10 overlay side-labels view-as dialog-box three-d
                title " Sucursal y Punto de Venta ".
                
&ENDIF

/*--- PROGRAMA PRINCIPAL ---*/
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
    session:data-entry-return = yes.

    vlc_sucursal:load-mouse-pointer('glove').
    vlc_sucursal:tooltip = " Haga doble click en el mouse o F3 para consulta relacionada".
    vlc_sucursal:fgcolor = 9.

    vlc_punto_venta:load-mouse-pointer('glove').
    vlc_punto_venta:tooltip = " Haga doble click en el mouse o F3 para consulta relacionada".
    vlc_punto_venta:fgcolor = 9.

    on "CHOOSE" of b_aceptar in frame sucdsp
     do:
        find sucursales where 
             sucursales.id_sucursal = input frame sucdsp vlc_sucursal and
             sucursales.estado = yes
               no-lock no-error.
        find puntos_venta where
             puntos_venta.id_sucursal    = input frame sucdsp vlc_sucursal and         
             puntos_venta.id_punto_venta = input frame sucdsp vlc_punto_venta
               no-lock no-error.
        if available sucursales and available puntos_venta then
         do:
            assign vlc_sucursal vlc_punto_venta.
            assign sal = yes.
            apply "CLOSE" to current-window.
            apply "RETURN" to this-procedure.
         end.
        else
         do:
            bell.
            message "La sucursal o punto de venta ingresado NO es v lido !!! " 
                    view-as alert-box information title " Error ".
            apply "RETURN" to vlc_sucursal in frame sucdsp.
         end.
     end.
     
    on "CHOOSE" of b_cancelar in frame sucdsp
     do:
        apply "RETURN" to this-procedure.
     end.
     
    on "ENTER-MENUBAR"         of vlc_sucursal in frame sucdsp or
       "MOUSE-SELECT-DBLCLICK" of vlc_sucursal in frame sucdsp
     do:
        assign vlc_sucursal.
        run x_suc.p (input-output vlc_sucursal).
        display vlc_sucursal @ vlc_sucursal with frame sucdsp.

        find sucursales where 
             sucursales.id_sucursal      = vlc_sucursal and
             sucursales.estado           = yes 
             no-lock no-error.
        if available sucursales then
           display sucursales.nombre @ sucursales.nombre with frame sucdsp.
        else
           display "" @ sucursales.nombre with frame sucdsp.
           
     end.

    on "RETURN" of vlc_sucursal in frame sucdsp or
       "TAB"    of vlc_sucursal in frame sucdsp
     do:
        find sucursales where 
             sucursales.id_sucursal       = (input frame sucdsp vlc_sucursal) and
             sucursales.estado            = yes
             no-lock no-error.
        if available sucursales then
           display sucursales.nombre @ sucursales.nombre with frame sucdsp.
        else
           display "" @ sucursales.nombre with frame sucdsp.
     end.

    on "ENTER-MENUBAR"         of vlc_punto_venta in frame sucdsp or
       "MOUSE-SELECT-DBLCLICK" of vlc_punto_venta in frame sucdsp
     do:
        assign vlc_sucursal vlc_punto_venta.
        run h_xptov1.p (input vlc_sucursal,
                        input-output vlc_punto_venta).
        display vlc_punto_venta @ vlc_punto_venta with frame sucdsp.
     end.

    on "RETURN" of vlc_punto_venta in frame sucdsp or
       "TAB"    of vlc_punto_venta in frame sucdsp
     do:
        apply "CHOOSE" to b_aceptar in frame sucdsp.
     end.

    enable b_aceptar b_cancelar vlc_sucursal vlc_punto_venta with frame sucdsp.
    wait-for "RETURN" of this-procedure focus vlc_sucursal pause 60.

&ELSE

_sale:
do on endkey undo _sale, retry _sale on error undo _sale, leave _sale:
  update vlc_sucursal vlc_punto_venta with frame sucdsp
    editing:
      message substring(" " + kblabel("GO")            + "=confirma, " +
                              kblabel("HELP")          + "=ayuda, " +
                              kblabel("ENTER-MENUBAR") + "=relaciona, " +
                              kblabel("CUT")           + "=comenta, " +
                              kblabel("END-ERROR")     + "=sale" +
                              fill(" ",80), 1, 79).
      readkey pause 60.
      if lastkey = -1 then
         undo _sale, leave _sale.
      apply lastkey.
      if not go-pending then
         do:
           {s_ayuda.i}

         if frame-field = "vlc_sucursal" then
            do:
              if keyfunction(lastkey) = "ENTER-MENUBAR" then
                 do:
                   assign vlc_sucursal.
                   run x_suc.p (input-output vlc_sucursal).
                   display vlc_sucursal @ vlc_sucursal with frame sucdsp.
                 end.

              find sucursales where
                   sucursales.id_sucursal       = input frame sucdsp vlc_sucursal and
                   sucursales.estado            = yes 
                   no-lock no-error.
              if available sucursales then
                 do:
                   message " Descripci¢n: " + sucursales.nombre.
                   display sucursales.nombre @ sucursales.nombre
                         with frame sucdsp.
                 end.
              else
                 display "" @ sucursales.nombre with frame sucdsp.
            end.

         if frame-field = "vlc_punto_venta" then
            do:
              if keyfunction(lastkey) = "ENTER-MENUBAR" then
                 do:
                   assign vlc_sucursal vlc_punto_venta.
                   run h_xptov1.p (input vlc_sucursal,
                                   input-output vlc_punto_venta).
                   display vlc_punto_venta @ vlc_punto_venta with frame sucdsp.
                 end.

            end.
         end.

    end. /* del editing */

  assign vlc_sucursal vlc_punto_venta.
  sal = yes.
end.
hide message no-pause.
hide frame sucdsp no-pause.

&ENDIF

return.
