/*************************************************************************/
/*  Graba los movimientos y pallets transporte de los tambores           */
/*************************************************************************/

define input parameter x_dato   as character.
define input parameter x_lector as integer.
session:time-source = "parametros".

define variable x_longitud  as integer.
define variable x_etiqueta  as char.
define variable x_suffix    as character.

x_longitud  = length(x_dato).
x_suffix  = substr(x_dato,11,1). /* VER EL TAMAÑO DE LA ETIQUETA */

/* message "El suffix es " x_suffix view-as alert-box. */

    if x_suffix = "Z" then do:
        
        x_etiqueta = substr(x_dato,4,7).
        /* message "La etiqueta es " x_etiqueta view-as alert-box. */

        if x_lector = 5 then do:
            find tambores_industria where id_etiqueta = integer(x_etiqueta) no-error.
            if available tambores_industria then
            do:
                    find tambores_transporte where
                         tambores_transporte.id_etiqueta    = tambores_industria.id_etiqueta no-error.
    
                    if not available tambores_transporte then 
                    do:
                        create tambores_transporte.
                        assign
                            tambores_transporte.id_lector    = x_lector
                            tambores_transporte.id_sucursal  = tambores_industria.id_sucursal
                            tambores_transporte.id_empresa  = tambores_industria.id_empresa
                            tambores_transporte.id_etiqueta    = tambores_industria.id_etiqueta
                            tambores_transporte.fecha        = today
                            tambores_transporte.hora         = string(time,"hh:mm:ss").
                        
                        ASSIGN 
                            /*  AHORA LOS MUEVO A ESOS TAMBORES A DESTINO PUERTO TRANSITORIO */
                           tambores_industria.id_empresa_ubicacion = 9
                           tambores_industria.id_sucursal_ubicacion = 9
                           tambores_industria.id_locacion_ubicacion = 9
                           tambores_industria.id_posicion_ubicacion = 9.

                        /* MESSAGE "Ya creo el tambor en tambores_transporte" VIEW-AS ALERT-BOX. */
                    end.
                    ELSE MESSAGE "Ya se despacho este tambor." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                    release tambores_transporte.

            end.
            ELSE MESSAGE "No se encontro un tambor con la etiqueta " X_etiqueta VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        end.      
    end.
