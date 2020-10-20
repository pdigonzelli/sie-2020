/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_stock.p                                      */
/****************************************************************************/
/*  Programa para actividades globales de los sistemas.                     */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  FECHA CREACION.......:   31/10/95                                       */
/*  REVISION.............:   1.00                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- VARIABLES GLOBALES DEL SISTEMA DE STOCK ---*/
define new global shared variable vlc_suc_desp like r_cendis_suc.id_suc_desp.
define NEW GLOBAL shared variable vlc_dia_oper as date.

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
     form vlc_suc_desp label " Dep¢sito de Despacho"
          help " Dep¢sito de despacho"
          validate(can-find(first r_cendis_suc use-index sucursales
                            where r_cendis_suc.id_suc_desp = input vlc_suc_desp and
                                  r_cendis_suc.id_cendis   = par_estado.id_cendis)
                                  and not can-find(sucursales_sra where
                                  sucursales_sra.id_sucursal = input vlc_suc_desp),
                      " Debe ingresar un dep¢sito de despacho existente")
          sucursales.nombre no-label
      with frame sucdsp centered row 10 overlay side-labels color white/green.
&ELSE
     form skip(1)
          vlc_suc_desp label " Dep¢sito de Despacho"
          /* el validate esta en el programa */
          c-nombre no-label skip(1.2)
          space(18) b_aceptar space(5) b_cancelar skip
          with frame sucdsp centered row 10 overlay side-labels view-as dialog-box three-d title " Sucursal de Despacho ".
&ENDIF

/*--- PROGRAMA PRINCIPAL ---*/
find first par_estado no-lock no-error.
vlc_suc_desp = par_estado.id_suc_desp.
vlc_dia_oper = TODAY.

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN

  vlc_suc_desp:load-mouse-pointer('glove').
  vlc_suc_desp:fgcolor = 9.
  vlc_suc_desp:tooltip = "Haga doble click en el mouse o F3 para consulta relacionada".

  on "CHOOSE" of b_aceptar in frame sucdsp
    do:
      find sucursales where sucursales.id_sucursal = input frame sucdsp vlc_suc_desp no-lock no-error.
      if available sucursales and
              can-find(first r_cendis_suc use-index sucursales
                       where r_cendis_suc.id_suc_desp = input frame sucdsp vlc_suc_desp and
                             r_cendis.id_cendis       = par_estado.id_cendis) then
        do:
          assign vlc_suc_desp.
          assign sal = yes.
          display sucursales.nombre @ c-nombre with frame sucdsp.
          input clear.
          readkey pause 2.
          input clear.
          release par_estado.
          hide frame sucdsp no-pause.
          apply "CLOSE" to current-window.
          apply "RETURN" to this-procedure.
        end.
      else
        do:
          bell.
          message " La Sucursal ingresada NO es v lida !!! " view-as alert-box error.
          /* apply "ENTRY" to vlc_suc_desp in frame sucdsp.*/
          apply "RETURN" to vlc_suc_desp in frame sucdsp.
          return no-apply.
        end.
    end.
     
  on "CHOOSE" of b_cancelar in frame sucdsp
    do:
      release par_estado.
      hide frame sucdsp no-pause.
      apply "RETURN" to this-procedure.
    end.
     
  on "ENTER-MENUBAR" of vlc_suc_desp in frame sucdsp or "MOUSE-SELECT-DBLCLICK" of vlc_suc_desp in frame sucdsp
    do:
      assign vlc_suc_desp.
      run x_sucdsp.p (input-output vlc_suc_desp).
      display vlc_suc_desp @ vlc_suc_desp with frame sucdsp.
      find sucursales where sucursales.id_sucursal      = vlc_suc_desp and
                            sucursales.estado           = yes          and
                           (sucursales.id_tipo_sucursal = 6 or
                            sucursales.id_tipo_sucursal = 3 or
                            sucursales.id_tipo_sucursal = 4) no-lock no-error.
      if available sucursales then
        if can-find(first r_cendis_suc use-index sucursales
                    where r_cendis_suc.id_suc_desp = vlc_suc_desp and
                          r_cendis.id_cendis       = par_estado.id_cendis) then
           display sucursales.nombre @ c-nombre with frame sucdsp.
    end.

  on "RETURN" of vlc_suc_desp in frame sucdsp or "TAB" of vlc_suc_desp in frame sucdsp
    do:
      find sucursales where sucursales.id_sucursal       = (input frame sucdsp vlc_suc_desp) and
                            sucursales.estado            = yes                               and
                           (sucursales.id_tipo_sucursal = 6 or
                            sucursales.id_tipo_sucursal = 3 or
                            sucursales.id_tipo_sucursal = 4) no-lock no-error.
      if available sucursales then
        if can-find(first r_cendis_suc use-index sucursales
                    where r_cendis_suc.id_suc_desp = (input frame sucdsp vlc_suc_desp) and
                          r_cendis.id_cendis       = par_estado.id_cendis) then
          do:
            display sucursales.nombre @ c-nombre with frame sucdsp.
            apply "CHOOSE" to b_aceptar in frame sucdsp.
          end.
        else
          do:
            display "" @ c-nombre with frame sucdsp.
            apply "CHOOSE" to b_aceptar in frame sucdsp.
          end.
      else
        do:
          display "" @ c-nombre with frame sucdsp.
          apply "CHOOSE" to b_aceptar in frame sucdsp.
        end.
    end.

  enable b_aceptar b_cancelar vlc_suc_desp with frame sucdsp.
    
  wait-for "RETURN" of this-procedure focus vlc_suc_desp pause 60.

&ELSE

  _sale:
  do on endkey undo _sale, retry _sale on error undo _sale, leave _sale:
    update vlc_suc_desp with frame sucdsp
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
             if keyfunction(lastkey) = "ENTER-MENUBAR" then
               do:
                 assign vlc_suc_desp.
                 run x_sucdsp.p (input-output vlc_suc_desp).
                 display vlc_suc_desp @ vlc_suc_desp with frame sucdsp.
               end.
             find sucursales
                  where sucursales.id_sucursal       = input frame sucdsp vlc_suc_desp and
                        sucursales.estado            = yes and
                        (sucursales.id_tipo_sucursal = 6 or
                         sucursales.id_tipo_sucursal = 3 or
                         sucursales.id_tipo_sucursal = 4)           
                      no-lock no-error.
             if available sucursales then
               if can-find(first r_cendis_suc use-index sucursales
                           where r_cendis_suc.id_suc_desp = input frame sucdsp vlc_suc_desp and
                                 r_cendis.id_cendis       = par_estado.id_cendis) then
                 do:
                   message " Descripci¢n: " + sucursales.nombre.
                   display sucursales.nombre @ sucursales.nombre with frame sucdsp.
                 end.
               else
                 display "" @ sucursales.nombre with frame sucdsp.
             else
               display "" @ sucursales.nombre with frame sucdsp.
           end.
      end.
    assign vlc_suc_desp.
    sal = yes.
  end.

  release par_estado.
  input clear.
  readkey pause 2.
  input clear.
  hide message no-pause.
  hide frame sucdsp no-pause.

&ENDIF

release par_estado.
return.
