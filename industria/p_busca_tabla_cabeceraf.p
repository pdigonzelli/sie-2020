DEFINE INPUT PARAMETER v_id_tipotambor AS INTEGER.
DEFINE INPUT PARAMETER v_id_articulo AS INTEGER.
DEFINE INPUT PARAMETER v_id_lote AS INTEGER.
DEFINE INPUT PARAMETER v_anio AS INTEGER.
DEFINE INPUT PARAMETER v_sucursal AS INTEGER.
DEFINE OUTPUT PARAMETER p_id_empresa AS INTEGER.
DEFINE OUTPUT PARAMETER p_id_sucursal AS INTEGER.
DEFINE OUTPUT PARAMETER p_id_tipotambor AS INTEGER.
DEFINE OUTPUT PARAMETER p_nromov AS INTEGER.

FOR EACH  {1} WHERE {1}.id_tipotambor              = v_id_tipotambor
                 AND {1}.id_articulo                = v_id_articulo
                 AND {1}.id_lote                    = v_id_lote
                 AND {1}.anio                       = v_anio
                /* AND {1}.id_sucursal_ubicacion      = v_sucursal
                 AND {1}.id_locacion_ubicacion      = 4 */
                 NO-LOCK, FIRST tambores_industria OF {1} WHERE tambores_industria.id_locacion_ubicacion = 4 
                                                            AND tambores_industria.id_sucursal_ubicacion = v_sucursal
                                                          NO-LOCK
                 BREAK BY {1}.nromov.
  
  IF LAST-OF({1}.nromov) THEN
  DO:
    p_id_empresa     = {1}.id_empresa.
    p_id_sucursal    = {1}.id_sucursal.
    p_id_tipotambor  = {1}.id_tipotambor.
    p_nromov         = {1}.nromov.
    LEAVE.
  END.
END.

/*
IF AVAILABLE {1} THEN DO:
   
    p_id_empresa     = {1}.id_empresa.
    p_id_sucursal    = {1}.id_sucursal.
    p_id_tipotambor  = {1}.id_tipotambor.
    p_nromov         = {1}.nromov.
END.
*/