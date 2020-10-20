/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_global.p                                     */
/****************************************************************************/
/*  Programa para actividades globales de los sistemas.                     */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  FECHA CREACION.......:   13/11/96                                       */
/*  REVISION.............:   1.10                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- PARAMETROS ---*/
define output parameter sal as logical initial no format "Si/No".

/*--- VARIABLES GLOBALES DEL SISTEMA ---*/
define new global shared variable vlc_suc_desp like r_cendis_suc.id_suc_desp.
define new global shared variable vlc_dia_oper like r_cendis_suc.fecha_distribucion.
define new global shared variable vlc_dias_baja as integer format "999" initial 60.

/*--- VARIABLES DEL PROGRAMA ---*/
define variable meses as character initial "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre".
define variable dias  as character initial "Domingo,Lunes,Martes,Mi‚rcoles,Jueves,Viernes,S bado".
define variable f1    as character.
define variable f2    as character.

/*--- PROGRAMA PRINCIPAL ---*/
find first par_estado no-lock no-error.
if not available par_estado then
 do:
    bell.
    message color white/red
            "Faltan Definir Par metros !!! Avise a Gerencia de Sistemas !!!." skip
            "ERROR: Tabla 'par_estado' no definida." 
            view-as alert-box error.
    return.
 end.
assign vlc_suc_desp = par_estado.id_suc_origen.
find first r_cendis_suc where (r_cendis_suc.id_cendis   = par_estado.id_cendis) and
                              (r_cendis_suc.id_sucursal = vlc_suc_desp) and
                              (r_cendis_suc.id_suc_desp = vlc_suc_desp) no-error.
if not available r_cendis_suc then
 do:
    bell.
    message color white/red
            "No est  disponible 'r_cendis_suc' para esta Sucursal." skip
            "Avise a Gerencia de Sistemas !!!. " 
            view-as alert-box error.
    return.
  end.
assign vlc_dia_oper = r_cendis_suc.fecha_distribucion.
release r_cendis_suc.
release par_estado.
if vlc_dia_oper <> today then
  do:
    bell.
    input clear.
    f1 = "Fecha de Computadora/Actual: " +
         entry(weekday(today),dias) + ", " + string(day(today)) + " de " +
         entry(month(today),meses) + " de " + string(year(today)).
    f2 = "Fecha Operativa de POS-Master: " +
         entry(weekday(vlc_dia_oper),dias) + ", " + string(day(vlc_dia_oper)) + " de " +
         entry(month(vlc_dia_oper),meses) + " de " + string(year(vlc_dia_oper)).
    message color white/red
            skip(1)
            "ATENCION !!! Difieren Fechas !!! Verifique si son correctas !!!"
            skip(1)
            f1
            skip
            f2
            skip(2)
            view-as alert-box information.
  end.
  
sal = yes.
return.
