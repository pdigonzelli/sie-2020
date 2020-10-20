/****************************************************************************/
/*  Recibe los datos del lector y llama al programa correspondiente         */
/*  Llama a los programas: d_dathor.p                                       */
/****************************************************************************/

define input parameter x_dato   as character.
define input parameter x_lector as integer.

/*
display x_dato.
*/

define var programa as character format "x(50)" no-undo.

input from "c:\lectores.cfg".
import programa.
input close.

programa = trim(entry(2,programa,"=")).

run value(programa)(input x_dato, input x_lector).
