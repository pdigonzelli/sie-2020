/* SEMANA_ANIO.P */

define input    parameter fecha     as date.
define output   parameter semana    as integer.
define output   parameter anio      as integer.

define variable b   as date.

b = date(01,01,year(fecha)).

semana  = (fecha - b) / 7 + 1.
anio    = year(fecha).

if semana > 53 then do: /*by facundo cambie 52 por 53*/
    semana  = 1.
    anio    = year(fecha) + 1.
end.
