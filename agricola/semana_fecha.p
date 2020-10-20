/****************************************************************************/
/*  NOMBRE PROGRAMA......:   semana_fecha.p                                 */
/****************************************************************************/
/*  Dados año y semana, calcula las fechas desde y hasta de la semana       */
/****************************************************************************/

define input    parameter anio      as integer.
define input    parameter semana    as integer.
define output   parameter fec_des   as date.
define output   parameter fec_has   as date.

fec_des = date(1,1,anio).
fec_des = fec_des + 9 - weekday(fec_des).
fec_des = fec_des + (semana - 2) * 7.

fec_has = fec_des + 6.
