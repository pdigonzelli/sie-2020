/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_cotizacion.p                                 */
/****************************************************************************/
/*  Carga de cotizaciones del d¡a (d¢lar, euro y real)                      */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os - Grupo Sauken S.A.           */
/*  FECHA CREACION.......:   01/06/2007                                     */
/*  REVISION.............:   1.01                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:   24/03/2008                                     */
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  MODIFICACION.........:   Parche para que grabe "monedas_cotizaciones"   */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- PARAMETROS ---*/
define output parameter sal as logical initial no format "Si/No".

/*--- VARIABLES DEL PROGRAMA ---*/
define variable meses as character initial "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre" no-undo.
define variable dias  as character initial "Domingo,Lunes,Martes,Mi‚rcoles,Jueves,Viernes,S bado" no-undo.
define variable hoy   as character format "x(50)" no-undo.
define variable aux1  as integer no-undo.
define variable aux2  as integer no-undo.

/*--- COTIZACIONES ---*/
define variable c_dolar like cotizacion.importe_comprador.
define variable v_dolar like cotizacion.importe_vendedor.
define variable c_euro  like cotizacion.importe_comprador.
define variable v_euro  like cotizacion.importe_vendedor.
define variable c_real  like cotizacion.importe_comprador.
define variable v_real  like cotizacion.importe_vendedor.

/*--- SOLO ENTRA SI SE CAMBIà HACE MµS DE MEDIA HORA ---*/
find last cotizacion use-index ultima 
          where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 2 and cotizacion.fecha = today and
                cotizacion.c_fecha = today no-lock no-error.
if available cotizacion then
  do:
    assign aux1 = integer(substring(replace(cotizacion.c_hora,":",""),1,4))
           aux2 = integer(substring(replace(string(time,"HH:MM:SS"),":",""),1,4)).
    if aux2 - aux1 <= 30 then
      do:
        sal = yes.
        return.
      end.
  end.

/*--- DOLAR ---*/
find last cotizacion use-index ultima 
          where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 2 and cotizacion.fecha <= today no-lock no-error.
if available cotizacion then
  assign c_dolar = cotizacion.importe_comprador
         v_dolar = cotizacion.importe_vendedor.

/*--- EURO ---*/
find last cotizacion use-index ultima 
          where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 99 and cotizacion.fecha <= today no-lock no-error.
if available cotizacion then
  assign c_euro = cotizacion.importe_comprador
         v_euro = cotizacion.importe_vendedor.

/*--- REAL ---*/
find last cotizacion use-index ultima 
          where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 12 and cotizacion.fecha <= today no-lock no-error.
if available cotizacion then
  assign c_real = cotizacion.importe_comprador
         v_real = cotizacion.importe_vendedor.

/*--- HOY ES ---*/
hoy = entry(weekday(today),dias) + ", " + string(day(today)) + " de " + entry(month(today),meses) + " de " + string(year(today)).
if session:display-type <> "GUI" then
  hoy = fill(" ", integer((50 - length(hoy)) / 2)) + hoy.

/*--- PROCESO PRINCIPAL ---*/
session:data-entry-return = yes.
input clear.
bell.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  display hoy no-label   at row 2   col  5
          "Comprador"    at row 4   col 10
          "Vendedor"     at row 4   col 36
          "D¢lar:"       at row 5   col  8 right-aligned
          c_dolar        at row 5   col 10 no-labels help "Importe del d¢lar comprador" validate(c_dolar > 0, "Debe ingresar un importe")
          v_dolar        at row 5   col 36 no-labels help "Importe del d¢lar vendedor"  validate(v_dolar > 0, "Debe ingresar un importe")
          "Euro:"        at row 6   col  8 right-aligned
          c_euro         at row 6   col 10 no-labels help "Importe del euro comprador"  validate(c_euro  > 0, "Debe ingresar un importe")
          v_euro         at row 6   col 36 no-labels help "Importe del euro vendedor"   validate(v_euro  > 0, "Debe ingresar un importe")
          "Real:"        at row 7   col  8 right-aligned
          c_real         at row 7   col 10 no-labels help "Importe del real comprador"  validate(c_real  > 0, "Debe ingresar un importe")
          v_real         at row 7   col 36 no-labels help "Importe del real vendedor"   validate(v_real  > 0, "Debe ingresar un importe")
      with frame cotizacion overlay row 6 centered side-labels color white/cyan title " Cotizaci¢n de las Monedas ".
  update c_dolar v_dolar c_euro v_euro c_real v_real with frame cotizacion.
&ELSE
  display hoy no-label   at row 1.5 col 12 font 9 bgcolor 15
          "Comprador"    at row 3   col 12 font 9
          "Vendedor"     at row 3   col 38 font 9
          "D¢lar:"       at row 4.2 col 11 right-aligned
          c_dolar        at row 4.2 col 12 no-labels help "Importe del d¢lar comprador" validate(c_dolar > 0, "Debe ingresar un importe")
          v_dolar        at row 4.2 col 38 no-labels help "Importe del d¢lar vendedor"  validate(v_dolar > 0, "Debe ingresar un importe")
          "Euro:"        at row 5.4 col 11 right-aligned
          c_euro         at row 5.4 col 12 no-labels help "Importe del euro comprador"  validate(c_euro  > 0, "Debe ingresar un importe")
          v_euro         at row 5.4 col 38 no-labels help "Importe del euro vendedor"   validate(v_euro  > 0, "Debe ingresar un importe")
          "Real:"        at row 6.6 col 11 right-aligned
          c_real         at row 6.6 col 12 no-labels help "Importe del real comprador"  validate(c_real  > 0, "Debe ingresar un importe")
          v_real         at row 6.6 col 38 no-labels help "Importe del real vendedor"   validate(v_real  > 0, "Debe ingresar un importe")
      with frame cotizacion overlay row 6 centered side-labels title "Cotizaci¢n de las Monedas" view-as dialog-box three-d.
  {s_close1.i 
     &frame        = "cotizacion"
     &oculto_extra = "s_nada.clo"
     &buttons      = "ok-cancel" 
  }
  {s_nomprg.i &nombre_frame = "cotizacion"}
  {s_tooltip.i "cotizacion"}
  do on endkey undo, leave on error undo, retry:
    update c_dolar v_dolar c_euro v_euro c_real v_real with frame cotizacion.
  end.
  if i-button = 2 then
    do:
      sal = yes.
      return.
    end.
&ENDIF
hide frame cotizacion no-pause.

/*--- CARGA DE COTIZACIONES ---*/
do transaction:

  /*--- DOLAR ---*/
  find cotizacion where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 2 and cotizacion.fecha = today no-error.
  if not available cotizacion then
    do:
      create cotizacion.
      assign cotizacion.id_moneda_origen = 80 
             cotizacion.id_moneda_cambio = 2 
             cotizacion.fecha            = today.
    end.
  assign cotizacion.importe_comprador = c_dolar
         cotizacion.importe_vendedor  = v_dolar.
  {s_audito.i "cotizacion"}
  
  /*--- EURO ---*/
  find cotizacion where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 99 and cotizacion.fecha = today no-error.
  if not available cotizacion then
    do:
      create cotizacion.
      assign cotizacion.id_moneda_origen = 80 
             cotizacion.id_moneda_cambio = 99
             cotizacion.fecha            = today.
    end.
  assign cotizacion.importe_comprador = c_euro
         cotizacion.importe_vendedor  = v_euro.
  {s_audito.i "cotizacion"}
  
  /*--- REAL ---*/
  find cotizacion where cotizacion.id_moneda_origen = 80 and cotizacion.id_moneda_cambio = 12 and cotizacion.fecha = today no-error.
  if not available cotizacion then
    do:
      create cotizacion.
      assign cotizacion.id_moneda_origen = 80 
             cotizacion.id_moneda_cambio = 12
             cotizacion.fecha            = today.
    end.
  assign cotizacion.importe_comprador = c_real
         cotizacion.importe_vendedor  = v_real.
  {s_audito.i "cotizacion"}


  /* PARCHE PARA QUE GRABE moneda_cotizaciones USADA EN TODO PAC-CORE --*/

  /* DOLAR */
  find moneda_cotizaciones where moneda_cotizaciones.id_moneda_origen = 2 and moneda_cotizaciones.fecha = today no-error.
  if not available moneda_cotizaciones then
    do:
      create moneda_cotizaciones.
      assign moneda_cotizaciones.id_moneda_origen = 2
             moneda_cotizaciones.fecha            = today.
    end.
  assign moneda_cotizaciones.tipo_cambio        = c_dolar
         moneda_cotizaciones.tipo_cambio_compra = v_dolar.
  {s_audito.i "moneda_cotizaciones"}

  /* REAL */
  find moneda_cotizaciones where moneda_cotizaciones.id_moneda_origen = 12 and moneda_cotizaciones.fecha = today no-error.
  if not available moneda_cotizaciones then
    do:
      create moneda_cotizaciones.
      assign moneda_cotizaciones.id_moneda_origen = 12
             moneda_cotizaciones.fecha            = today.
    end.
  assign moneda_cotizaciones.tipo_cambio        = c_real
         moneda_cotizaciones.tipo_cambio_compra = v_real.
  {s_audito.i "moneda_cotizaciones"}

  /* EURO */
  find moneda_cotizaciones where moneda_cotizaciones.id_moneda_origen = 99 and moneda_cotizaciones.fecha = today no-error.
  if not available moneda_cotizaciones then
    do:
      create moneda_cotizaciones.
      assign moneda_cotizaciones.id_moneda_origen = 99
             moneda_cotizaciones.fecha            = today.
    end.
  assign moneda_cotizaciones.tipo_cambio        = c_euro
         moneda_cotizaciones.tipo_cambio_compra = v_euro.
  {s_audito.i "moneda_cotizaciones"}

end.

sal = yes.
return.
