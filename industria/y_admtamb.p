/****************************************************/
/* PROGRAMA DE CONTROL DE PRODUCTOS INDUSTRIALES    */
/* AUTOR: DANIEL REYNA                              */
/****************************************************/

define vari vpi_sucursal like sucursales.id_sucursal initial 95.
define vari vpi_lote     like tambores_industria.id_lote.
define vari vpi_articulo like articulos.id_articulo.
define vari vpi_aniolote as integer format "99" initial 02.




for each tambores_industria 
where tambores_industria.id_sucursal_ubicacion = vpi_sucursal and
      tambores_industria.id_locacion_ubicacion <> 10 and
/*      tambores_industria.id_lote               = 0 and */
      substring(string(tambores_industria.anio,"9999"),3,2) 
                                               = string(vpi_aniolote,"99") /* and    tambores_industria.id_articulo           = vpi_articulo */
      no-lock by id_lote by id_articulo:
      find productos_terminados where 
        productos_terminados.id_articulo = tambores_industria.id_articulo
        no-lock no-error.
        if  available productos_terminados then
      display 
              tambores_industria.id_tambor
              tambores_industria.id_lote
              tambores_industria.id_articulo
              
              productos_terminados.descripcion 
                    when available productos_terminados
              tambores_industria.id_sucursal_ubicacion.
              
              
end.