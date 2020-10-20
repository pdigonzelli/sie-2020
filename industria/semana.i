/* SEMANA.I */


/* devuelve a que semana del a¤o corresponde la fecha */
function semana returns integer (input fecha as date).
define var semana as integer.
define var b as date.
b = date(01,01,year(fecha)).
semana = (fecha - b) / 7 + 1.
return semana.
end function.
